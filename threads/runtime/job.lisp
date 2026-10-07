;;;; job.lisp
;;;;
;;;; Jobs: units of work executed by the scheduler, and the protocol by
;;;; which threads wait for them to complete.

(in-package #:coalton/threads/runtime)

(defvar *debug-tasks* nil
  "When true, a serious condition signaled by a task running on behalf
of another thread invokes the debugger in the thread running the task,
before the condition is transferred to the waiting thread. The
debugger offers a TRANSFER-ERROR restart that resumes the transfer.

When false (the default), the condition is transferred immediately and
re-signaled in the waiting thread.")

(defun %noop ()
  nil)

(define-condition task-aborted (error)
  ()
  (:report "A parallel task was aborted before it finished.")
  (:documentation "Signaled by an operation waiting for a task that was
unwound before it finished, for example because its worker thread was
interrupted. Coalton code catches it as the exception TaskAborted of
coalton/threads/parallel."))

(defun panic (format-control &rest arguments)
  "Signal a Coalton Panic, the condition for bugs detected at runtime,
whose message is FORMAT-CONTROL applied to ARGUMENTS."
  (error 'coalton/classes:panic
         :message (apply #'format nil format-control arguments)))

;;; Domains
;;;
;;; A worker that waits for a job (the second half of a join, the tasks
;;; of a scope, or a future running elsewhere) runs other jobs in the
;;; meantime, on top of the waiting computation's stack. That is only
;;; safe if none of those jobs can end up waiting for the computation
;;; buried beneath it: otherwise the stack would hold a cycle that the
;;; program's dependencies do not have, and the worker would deadlock.
;;;
;;; Fork-join jobs (joins and scope tasks) are only ever waited for by
;;; the computation that created them, so running them while waiting is
;;; safe, as in Rayon. Futures, however, can be awaited by anyone. So
;;; each running future starts a DOMAIN, to which the fork-join jobs
;;; created while it runs belong, and a waiting worker only runs
;;; fork-join jobs of its current domain. Computations outside of any
;;; future, including those submitted from outside the pool, belong to
;;; the root domain. Futures themselves run only at the top level of a
;;; worker's stack, or inline in a thread that awaits them before they
;;; have started.
;;;
;;; A waiting worker therefore never needs work that it may not run,
;;; nor work that it cannot reach. The second half of a join it waits
;;; for is in its own deque, unless a thief took it, and the join claims
;;; it there even when other jobs lie above it. The tasks of a scope it
;;; waits for are in the scope's inbox (see SCOPE-SPAWN), or in deques,
;;; where it may claim them in place: they need not reach the top of a
;;; deque, which a job that it may not run could block for good. A
;;; future it waits for is either unstarted, and then it runs the
;;; future itself, or running. Whatever is running elsewhere waits, in
;;; turn, only for the same kinds of things, so a worker stays blocked
;;; only if the program's dependencies have a cycle.

(defvar *domain* nil
  "The domain of the computation running on the current thread: the
future being run, or NIL outside of any future.")

(declaim (inline %make-job))

(deftype job-kind ()
  "What kind of job a job is:

:JOIN for the second half of a join, run either by the join itself,
which takes it back from its deque without claiming it, or by a thief
that claims it.

:TASK for scope tasks and fork-join jobs submitted from outside the
pool, which are always claimed before they run, so that a waiting
worker may claim them wherever it finds them.

NIL for futures, which waiting workers do not run (see \"Domains\"
above), and for latches, which are never run."
  '(member nil :join :task))

(defstruct (job (:constructor %make-job (fn domain kind))
                (:copier nil)
                (:predicate nil))
  ;; The function to call. Replaced with %NOOP once the job starts, so
  ;; that a finished job does not keep its closure alive.
  (fn #'%noop :type function)
  ;; 0 until some thread claims the job for execution, then 1. Jobs
  ;; that can be reached from more than one place (futures) are only
  ;; run by the thread that claims them.
  (claim 0 :type fixnum)
  ;; Threads waiting for completion: a list of parkers, or :DONE once
  ;; the job has completed.
  (waiters '() :type (or list (eql :done)))
  ;; The value of the job, or the condition it signaled.
  (result nil)
  (condition nil)
  ;; The domain in which the job runs (see "Domains" above), until it
  ;; starts, so that stale entries for the job do not keep a finished
  ;; future and its value alive.
  (domain nil)
  (kind nil :type job-kind :read-only t))

(defstruct (future (:include job)
                   (:constructor %make-future (fn))
                   (:copier nil)
                   (:predicate nil))
  ;; The ticket that schedules the future, until the future starts.
  (ticket nil))

;;; A future can be run by a worker that finds it in a deque or a queue,
;;; or by a thread that awaits it before it starts. So that the entry
;;; left behind in the latter case does not keep the future and its
;;; value alive until somebody happens to discard it, the entry is not
;;; the future itself but a ticket for it, which whoever starts the
;;; future first retires. A ticket is a job that is never run: workers
;;; that claim one run its future instead (see EXECUTE-JOB).

(defstruct (ticket (:include job)
                   (:constructor %make-ticket (future))
                   (:copier nil))
  (future nil))

(backend:freeze-type job future ticket)

(declaim (inline job-done-p claim-job job-claimed-p job-helpable-p
                 runnable-while-waiting-p claimable-in-place-p))

(defun job-done-p (job)
  "True if JOB has completed."
  (declare (type job job))
  (eq (job-waiters job) :done))

(defun claim-job (job)
  "Try to claim JOB for execution. Returns true if this thread won."
  (declare (type job job))
  (eql 0 (backend:compare-and-swap (job-claim job) 0 1)))

(defun job-claimed-p (job)
  "True if some thread has claimed JOB for execution."
  (declare (type job job))
  (not (eql 0 (job-claim job))))

(defun job-helpable-p (job)
  "True if JOB is a fork-join job, which waiting workers may run."
  (declare (type job job))
  (not (null (job-kind job))))

(defun runnable-while-waiting-p (job)
  "May the current thread, which is waiting for some job, run JOB in the
meantime? See \"Domains\" above."
  (declare (type job job))
  (and (job-helpable-p job)
       (eq (job-domain job) *domain*)))

(defun claimable-in-place-p (job)
  "May the current thread, which is waiting for some job, claim JOB and
run it wherever it is, even in the middle of a deque?"
  (declare (type job job))
  (and (eq (job-kind job) :task)
       (not (job-claimed-p job))
       (eq (job-domain job) *domain*)))

(defun complete-job (job)
  "Mark JOB as completed and wake every thread waiting for it. The
result or condition of JOB must already have been stored. Completing a
job that has already completed has no effect."
  (declare (type job job))
  ;; Waiters that see :DONE must also see the result.
  (backend:release-fence)
  (backend:uninterruptibly
    (let ((waiters (loop
                     (let ((old (job-waiters job)))
                       (when (eq old (backend:compare-and-swap (job-waiters job) old :done))
                         (return old))))))
      (unless (eq waiters :done)
        (dolist (parker waiters)
          (unpark parker)))))
  (values))

(defun add-job-waiter (job parker)
  "Arrange for PARKER to be unparked when JOB completes. Returns NIL,
without registering PARKER, if JOB has already completed."
  (declare (type job job)
           (type parker parker))
  (loop
    (let ((old (job-waiters job)))
      (when (eq old :done)
        (return nil))
      (when (eq old (backend:compare-and-swap (job-waiters job) old (cons parker old)))
        (return t)))))

(defun nudge-job-waiters (job)
  "Wake the threads waiting for JOB that are asleep, if JOB has not
completed, because there may be work for them to do in the meantime.
The caller must have made that work visible and issued a full fence.
See IDLE."
  (declare (type job job))
  (let ((waiters (job-waiters job)))
    (unless (eq waiters :done)
      (dolist (parker waiters)
        (when (parker-asleep parker)
          (unpark parker)))))
  (values))

(defun debug-task-error (condition)
  "Invoke the debugger on CONDITION, offering to transfer it to the
thread waiting for the failed task."
  (restart-case (invoke-debugger condition)
    (transfer-error ()
      :report "Transfer the condition to the thread waiting for this task."
      nil)))

(defmacro with-condition-capture ((condition-var) form &body on-condition)
  "Evaluate FORM. If it signals a serious condition that nothing inside
FORM handles, unwind, bind CONDITION-VAR to the condition, and evaluate
ON-CONDITION instead."
  (let ((normal (gensym "NORMAL"))
        (capture (gensym "CAPTURE"))
        (c (gensym "CONDITION")))
    `(block ,normal
       (let ((,condition-var
               (block ,capture
                 (handler-bind ((serious-condition
                                  (lambda (,c)
                                    (when *debug-tasks*
                                      (debug-task-error ,c))
                                    (return-from ,capture ,c))))
                   (return-from ,normal ,form)))))
         ,@on-condition))))

(defun run-job (job)
  "Run JOB, which the calling thread has claimed, storing its value or
the serious condition it signaled, then complete it. JOB is completed
even if it exits non-locally, for example because its thread is
interrupted, in which case its condition is a TASK-ABORTED error."
  (declare (type job job))
  (let ((fn (job-fn job))
        (finished nil))
    (setf (job-fn job) #'%noop
          (job-domain job) nil)
    (unwind-protect
         (progn
           (with-condition-capture (condition)
               (setf (job-result job) (funcall fn))
             (setf (job-condition job) condition))
           (setf finished t))
      (unless finished
        (setf (job-condition job) (make-condition 'task-aborted)))
      (complete-job job))))

(defun job-value (job)
  "The value of the completed JOB. If JOB signaled a serious condition,
signal it again in the current thread."
  (declare (type job job))
  (backend:acquire-fence)
  (let ((condition (job-condition job)))
    (if condition
        (error condition)
        (job-result job))))

(defun block-until-done (job)
  "Block the current thread, which need not be a worker, until JOB
completes."
  (declare (type job job))
  (unless (job-done-p job)
    (let ((parker (make-parker)))
      (when (add-job-waiter job parker)
        (loop :until (job-done-p job)
              :do (park parker)))))
  (values))
