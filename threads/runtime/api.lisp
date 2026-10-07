;;;; api.lisp
;;;;
;;;; Fork-join, futures, scopes, and parallel loops over integer ranges.

(in-package #:coalton/threads/runtime)

;;; Running code in the pool

(defun call-in-pool (fn)
  "Call FN, a function of no arguments, on a worker of the global pool
and return its values. If the current thread is a worker, call FN
directly. If FN signals a serious condition, signal it again in the
current thread.

If the current thread is unwound while it waits, by an interrupt or a
deadline, FN does not run if it has not started; otherwise the thread
waits for FN to finish before unwinding further, so that FN never
outlives the call. Interrupting that wait abandons FN, which keeps
running."
  (declare (type function fn))
  (if *worker*
      (funcall fn)
      ;; A fork-join job of the root domain: see "Domains" in job.lisp.
      (let ((job (%make-job (lambda () (multiple-value-list (funcall fn)))
                            nil
                            :task)))
        (submit-from-outside job)
        (unwind-protect (block-until-done job)
          (unless (job-done-p job)
            (if (claim-job job)
                ;; Nobody has started JOB, and now nobody will: whoever
                ;; finds it in a queue discards it.
                (setf (job-fn job) #'%noop
                      (job-domain job) nil)
                ;; An expired deadline would end this wait at once.
                (backend:without-deadline
                  (block-until-done job)))))
        (values-list (job-value job)))))

;;; Fork-join
;;;
;;; JOIN2 pushes the second function as a job onto the worker's deque,
;;; calls the first function, then pops the job back and calls it
;;; directly, unless a thief took it in the meantime. In that case the
;;; worker executes other jobs until the stolen one completes. Calling
;;; the first function on the current thread means that its dynamic
;;; environment (handlers, restarts, special bindings) is the caller's;
;;; the second function runs either on the current thread or on a
;;; worker that stole it.

(defun dispose-of-job-above (worker item)
  "Deal with ITEM, a job other than the one sought, that WORKER popped
from its own deque while looking for the second job of a join. Returns
true if ITEM was put back, in which case the job sought lies beneath it,
unless a thief took it.

ITEM is normally a future that was never awaited, which is passed on to
the workers at the top level, or a stale entry, which is discarded.
Fork-join jobs pushed after the job sought are taken back before the
computations that pushed them return, except for tasks spawned into a
scope of an enclosing computation, which belong to the current domain
and are run. Jobs of other domains are put back; see \"Domains\" in
job.lisp."
  (declare (type worker worker)
           (type job item))
  (cond ((job-claimed-p item)
         nil)
        ((not (job-helpable-p item))
         (enqueue-top-level (worker-pool* worker) item)
         nil)
        ((runnable-while-waiting-p item)
         (execute-job worker item)
         nil)
        (t
         (deque-push (worker-deque worker) item)
         t)))

(defun abandon-job (worker job)
  "Called when a join exits non-locally while its second function, JOB,
may still be in WORKER's deque or running elsewhere: make sure that JOB
either never runs or has finished. Its outcome is discarded."
  (declare (type worker worker)
           (type job job))
  (let ((deque (worker-deque worker)))
    (loop
      (let ((item (deque-pop deque)))
        (cond ((eq item job)
               (return))
              ((or (null item) (dispose-of-job-above worker item))
               ;; JOB was stolen, or lies beneath a job that we may not
               ;; run here. Unless some thread has started it, claim it
               ;; so that nobody ever does: whoever finds it in a deque
               ;; discards it.
               (if (claim-job job)
                   (setf (job-fn job) #'%noop
                         (job-domain job) nil)
                   (wait-for-job worker job))
               (return))))))
  (values))

(defun join2-in-worker (worker fa fb)
  (declare (type worker worker)
           (type function fa fb))
  (let ((job (%make-job fb *domain* :join))
        ;; True while JOB may be in our deque or running elsewhere.
        (pending t))
    (push-local worker job)
    (unwind-protect
         (let ((a (funcall fa))
               (deque (worker-deque worker)))
           (flet ((wait-for-thief ()
                    (wait-for-job worker job)
                    (setf pending nil)
                    (job-value job)))
             (loop
               (let ((item (deque-pop deque)))
                 (cond ((eq item job)
                        ;; Nobody took it: run it here.
                        (setf pending nil)
                        (return (values a (funcall fb))))
                       ((null item)
                        ;; A thief took it.
                        (return (values a (wait-for-thief))))
                       ((not (dispose-of-job-above worker item)))
                       ((claim-job job)
                        ;; JOB lies beneath a job that we may not run
                        ;; here, and no thief has started it. Run it
                        ;; here anyway; whoever finds it in a deque
                        ;; discards it.
                        (setf pending nil)
                        (setf (job-fn job) #'%noop
                              (job-domain job) nil)
                        (return (values a (funcall fb))))
                       (t
                        (return (values a (wait-for-thief)))))))))
      ;; Whatever the exit, never leave JOB behind, so that FB cannot
      ;; run after the join is over.
      (when pending
        (abandon-job worker job)))))

(defun join2 (fa fb)
  "Call FA and FB, functions of no arguments that each return one value,
potentially in parallel, and return their two values.

On a worker, FA is called on the current thread; elsewhere, both are
called on workers while the current thread waits. If FA exits
non-locally, FB is cancelled if it has not started, or else awaited, and
its outcome is discarded. If FB signals a serious condition after FA
has returned, that condition is signaled again in the current thread."
  (declare (type function fa fb))
  (let ((worker *worker*))
    (if worker
        (join2-in-worker worker fa fb)
        (call-in-pool (lambda ()
                        (join2-in-worker *worker* fa fb))))))

;;; Futures

(defun spawn-future (fn)
  "Schedule FN, a function of no arguments returning one value, to run in
the global pool, and return a job representing its eventual value."
  (declare (type function fn))
  (let* ((future (%make-future fn))
         (ticket (%make-ticket future)))
    ;; A future is the domain of the jobs it creates.
    (setf (job-domain future) future
          (future-ticket future) ticket)
    (submit ticket)
    future))

(defun future-done-p (job)
  "True if the future JOB has completed."
  (declare (type job job))
  (job-done-p job))

(defun retire-ticket (worker future)
  "Called when WORKER has claimed FUTURE to run it inline: retire the
ticket that schedules it, so that its entry, wherever it is, no longer
refers to FUTURE, and whoever finds the entry discards it."
  (declare (type worker worker)
           (type future future))
  (let ((ticket (future-ticket future)))
    (when ticket
      (setf (future-ticket future) nil
            (job-claim ticket) 1
            (ticket-future ticket) nil)
      (let ((deque (worker-deque worker)))
        (when (eq ticket (deque-peek-bottom deque))
          ;; Usually, the future was just spawned. Discard the entry,
          ;; and stale entries beneath it.
          (loop :do (deque-pop deque)
                :while (let ((next (deque-peek-bottom deque)))
                         (and next (job-claimed-p next))))))))
  (values))

(defun await-future (job)
  "Return the value of the future JOB, waiting for it if necessary. If
JOB signaled a serious condition, signal it again in the current thread.

A worker awaiting a future that has not started runs it itself; one
awaiting a future running elsewhere runs other tasks of its own
computation meanwhile. Other threads block."
  (declare (type future job))
  (unless (job-done-p job)
    (let ((worker *worker*))
      (cond ((null worker)
             (block-until-done job))
            ((claim-job job)
             (retire-ticket worker job)
             (run-claimed-job worker job))
            (t
             (wait-for-job worker job)))))
  (job-value job))

;;; Scopes
;;;
;;; A scope counts its pending tasks, plus one for the function that
;;; owns it. When the count drops to zero the scope closes, and its
;;; latch completes, releasing the thread waiting at the end of
;;; CALL-WITH-SCOPE.

(defconstant +scope-closed+ (ash 1 (1- (integer-length most-positive-fixnum)))
  "Added to the count of pending tasks of a scope when it closes, so
that later attempts to spawn into it fail. The largest power of two
that is a fixnum: far more than any number of tasks, and representable
in a BACKEND:ATOMIC-WORD on any implementation.")

(defstruct (scope (:constructor %make-scope (domain))
                  (:copier nil)
                  (:predicate nil))
  (pending 1 :type backend:atomic-word)
  (latch (%make-job #'%noop nil nil) :type job :read-only t)
  ;; The domain of the computation that owns the scope, to which its
  ;; tasks belong, wherever they are spawned from.
  (domain nil :read-only t)
  ;; Tasks spawned from other domains or from outside the pool, which
  ;; are also handed to the workers at the top level. A worker of
  ;; another domain may not run them while it waits (see "Domains" in
  ;; job.lisp), so they do not go into the spawning worker's deque.
  ;; The scope's owner runs them while it waits for the scope, unless a
  ;; worker at the top level got to them first.
  (inbox (backend:make-queue :name "Coalton scope inbox")
   :type backend:queue :read-only t)
  ;; The first serious condition signaled by a task of this scope.
  (condition nil))

(backend:freeze-type scope)

(defun scope-task-finished (scope)
  (declare (type scope scope))
  (when (and (= 1 (backend:atomic-decf (scope-pending scope)))
             ;; Unless a task was spawned meanwhile, which will close the
             ;; scope when it finishes.
             (eql 0 (backend:compare-and-swap (scope-pending scope) 0 +scope-closed+)))
    (complete-job (scope-latch scope)))
  (values))

(defun post-to-scope (scope job)
  "Schedule JOB, a task of SCOPE spawned from another domain or from
outside the pool, through SCOPE's inbox, and also hand it to the workers
at the top level."
  (declare (type scope scope)
           (type job job))
  (let ((pool (ensure-pool))
        (inbox (scope-inbox scope)))
    ;; Entries go stale when workers at the top level run their jobs, and
    ;; the scope's owner only discards them when it waits for the scope.
    ;; A job moved to the back of the inbox meanwhile is missing from it
    ;; for a moment; the nudge below wakes an owner that missed it.
    (discard-stale-jobs inbox)
    (backend:enqueue job inbox)
    ;; The scope's owner usually claims JOB from the inbox, which makes
    ;; the copy in the top-level queue stale. This ends with a full
    ;; fence.
    (enqueue-top-level pool job)
    ;; The owner, about to sleep while waiting for the scope, registers
    ;; as a waiter of the latch before it looks at the inbox one last
    ;; time: wake it in case it looked before JOB arrived.
    (nudge-job-waiters (scope-latch scope)))
  (values))

(defun scope-spawn (scope fn)
  "Schedule FN, a function of no arguments whose values are ignored, as
a task of SCOPE. May be called from any thread.

A task spawned from the scope's own domain goes into the deque of the
worker spawning it, like the second half of a join. Others go through
the scope's inbox: in the spawning worker's deque, a task of another
domain could sit above jobs that this worker will wait for, and that
it would then have to reach in some other way."
  (declare (type scope scope)
           (type function fn))
  (when (>= (backend:atomic-incf (scope-pending scope)) +scope-closed+)
    (backend:atomic-decf (scope-pending scope))
    (panic "Cannot spawn a task in a scope that has already finished."))
  (flet ((task ()
           (let ((finished nil))
             (unwind-protect
                  (progn
                    (with-condition-capture (condition)
                        (funcall fn)
                      (backend:compare-and-swap (scope-condition scope) nil condition))
                    (setf finished t))
               (unless finished
                 (backend:compare-and-swap (scope-condition scope)
                                           nil
                                           (make-condition 'task-aborted)))
               (scope-task-finished scope)))
           nil))
    (let ((worker *worker*)
          (job (%make-job #'task (scope-domain scope) :task)))
      (cond ((and worker (eq *domain* (scope-domain scope)))
             (let ((pool (worker-pool* worker)))
               (deque-push (worker-deque worker) job)
               (backend:full-fence)
               (notify-work pool)
               ;; Waking a sleeping worker is not enough: it may belong
               ;; to another domain. The owner of the scope, if it sleeps
               ;; waiting for the scope, can claim the task where it is.
               (when (plusp (pool-sleeping pool))
                 (nudge-job-waiters (scope-latch scope)))))
            (t
             (post-to-scope scope job)))))
  (values))

(defun call-with-scope (fn)
  "Call FN with a new scope, then wait until every task spawned in the
scope has finished, and return the values of FN. If FN returns normally
but some task signaled a serious condition, signal the first such
condition.

If FN exits non-locally, or the current thread is unwound while it
waits for the tasks, by an interrupt or a deadline, the tasks are still
awaited, and their conditions are discarded. Interrupting that wait
abandons the tasks, which keep running."
  (declare (type function fn))
  (let ((worker *worker*))
    (if (null worker)
        (call-in-pool (lambda () (call-with-scope fn)))
        (let ((scope (%make-scope *domain*))
              ;; Whether the owner's count in the scope was released.
              (released nil))
          (flet ((finish ()
                   (backend:uninterruptibly
                     (unless released
                       (setf released t)
                       (scope-task-finished scope)))
                   (wait-for-job worker (scope-latch scope) (scope-inbox scope))))
            (multiple-value-prog1
                (unwind-protect
                     (multiple-value-prog1 (funcall fn scope)
                       (finish))
                  ;; After a non-local exit, from FN or from the wait
                  ;; above, wait for the tasks anyway. Once they have
                  ;; finished, this returns at once.
                  (finish))
              (let ((condition (scope-condition scope)))
                (when condition
                  (error condition)))))))))

;;; Parallel loops over integer ranges
;;;
;;; By default ranges are split adaptively, following Rayon: a loop
;;; starts with a budget of one split per worker, halved at each split.
;;; A half that another worker steals gets a fresh budget, so splitting
;;; continues exactly where there are idle workers to absorb it, and
;;; ranges are otherwise processed in large sequential chunks. With a
;;; positive GRAIN, ranges are instead split in halves until no longer
;;; than GRAIN, which makes the shape of a reduction independent of
;;; scheduling.

(declaim (inline %try-split))

(defun %try-split (length splits migrated workers)
  "Decide whether to split a range of LENGTH elements. Returns two
values: whether to split, and the split budget of each half."
  (declare (type fixnum length splits workers))
  (cond ((< length 2) (values nil splits))
        (migrated (values t (max workers (ash splits -1))))
        ((plusp splits) (values t (ash splits -1)))
        (t (values nil 0))))

(defun %for-chunks-adaptive (start end splits migrated workers chunk)
  (declare (type fixnum start end splits workers)
           (type function chunk))
  (multiple-value-bind (split-p splits) (%try-split (- end start) splits migrated workers)
    (if split-p
        (let ((mid (+ start (ash (- end start) -1)))
              (owner *worker*))
          (join2-in-worker owner
                           (lambda ()
                             (%for-chunks-adaptive start mid splits nil workers chunk))
                           (lambda ()
                             (%for-chunks-adaptive mid end splits (not (eq *worker* owner))
                                                   workers chunk))))
        (funcall chunk start end)))
  (values))

(defun %for-chunks-fixed (start end grain chunk)
  (declare (type fixnum start end grain)
           (type function chunk))
  (if (<= (- end start) grain)
      (funcall chunk start end)
      (let ((mid (+ start (ash (- end start) -1))))
        (join2-in-worker *worker*
                         (lambda () (%for-chunks-fixed start mid grain chunk))
                         (lambda () (%for-chunks-fixed mid end grain chunk)))))
  (values))

(defun parallel-for-chunks (start end grain chunk)
  "Call CHUNK, a function of two integers LO and HI, on disjoint
non-empty ranges [LO, HI) that together cover [START, END), potentially
in parallel. GRAIN is zero for adaptive splitting, or else the largest
length of a range."
  (declare (type fixnum start end)
           (type (and fixnum unsigned-byte) grain)
           (type function chunk))
  (when (< start end)
    (let ((worker *worker*))
      (cond ((null worker)
             (call-in-pool (lambda () (parallel-for-chunks start end grain chunk) nil)))
            ((plusp grain)
             (%for-chunks-fixed start end grain chunk))
            (t
             (let ((workers (pool-size (worker-pool* worker))))
               (%for-chunks-adaptive start end workers nil workers chunk))))))
  (values))

(defun parallel-for (start end grain body)
  "Call BODY on every integer in [START, END), potentially in parallel.
GRAIN is as for PARALLEL-FOR-CHUNKS."
  (declare (type fixnum start end)
           (type (and fixnum unsigned-byte) grain)
           (type function body))
  (parallel-for-chunks start end grain
                       (lambda (lo hi)
                         (declare (type fixnum lo hi))
                         (loop :for i :of-type fixnum :from lo :below hi
                               :do (funcall body i))
                         (values))))

(defun %reduce-chunks-adaptive (start end splits migrated workers chunk combine)
  (declare (type fixnum start end splits workers)
           (type function chunk combine))
  (multiple-value-bind (split-p splits) (%try-split (- end start) splits migrated workers)
    (if split-p
        (let ((mid (+ start (ash (- end start) -1)))
              (owner *worker*))
          (multiple-value-bind (left right)
              (join2-in-worker owner
                               (lambda ()
                                 (%reduce-chunks-adaptive start mid splits nil workers
                                                          chunk combine))
                               (lambda ()
                                 (%reduce-chunks-adaptive mid end splits (not (eq *worker* owner))
                                                          workers chunk combine)))
            (funcall combine left right)))
        (funcall chunk start end))))

(defun %reduce-chunks-fixed (start end grain chunk combine)
  (declare (type fixnum start end grain)
           (type function chunk combine))
  (if (<= (- end start) grain)
      (funcall chunk start end)
      (let ((mid (+ start (ash (- end start) -1))))
        (multiple-value-bind (left right)
            (join2-in-worker *worker*
                             (lambda () (%reduce-chunks-fixed start mid grain chunk combine))
                             (lambda () (%reduce-chunks-fixed mid end grain chunk combine)))
          (funcall combine left right)))))

(defun parallel-reduce-chunks (start end grain chunk combine identity)
  "Combine with the associative function COMBINE, in order, the values of
CHUNK, a function of two integers LO and HI, on disjoint non-empty
ranges [LO, HI) that together cover [START, END), potentially in
parallel. Returns IDENTITY if the range is empty. GRAIN is as for
PARALLEL-FOR-CHUNKS."
  (declare (type fixnum start end)
           (type (and fixnum unsigned-byte) grain)
           (type function chunk combine))
  (cond ((>= start end)
         identity)
        ((null *worker*)
         (call-in-pool (lambda ()
                         (parallel-reduce-chunks start end grain chunk combine identity))))
        ((plusp grain)
         (%reduce-chunks-fixed start end grain chunk combine))
        (t
         (let ((workers (pool-size (worker-pool* *worker*))))
           (%reduce-chunks-adaptive start end workers nil workers chunk combine)))))

(defun parallel-reduce (start end grain map combine identity)
  "Combine the values of MAP on the integers in [START, END) with the
associative function COMBINE, potentially in parallel, preserving their
order. Returns IDENTITY if the range is empty. GRAIN is as for
PARALLEL-FOR-CHUNKS."
  (declare (type fixnum start end)
           (type (and fixnum unsigned-byte) grain)
           (type function map combine))
  (parallel-reduce-chunks start end grain
                          (lambda (lo hi)
                            (declare (type fixnum lo hi))
                            (let ((acc (funcall map lo)))
                              (loop :for i :of-type fixnum :from (1+ lo) :below hi
                                    :do (setf acc (funcall combine acc (funcall map i))))
                              acc))
                          combine
                          identity))

;;; Parallel map over sequences

(defun parallel-map-vector (fn vector)
  "A fresh adjustable vector with a fill pointer, of the same length as
VECTOR, holding the values of FN on the elements of VECTOR, computed in
parallel."
  (declare (type function fn)
           (type vector vector))
  (let* ((n (length vector))
         (out (make-array n :adjustable t :fill-pointer n :initial-element nil)))
    (parallel-for 0 n 0 (lambda (i)
                          (setf (aref out i) (funcall fn (aref vector i)))
                          (values)))
    out))

(defun parallel-map-list (fn list)
  "A fresh list of the values of FN on the elements of LIST, computed in
parallel."
  (declare (type function fn)
           (type list list))
  (let* ((in (coerce list 'simple-vector))
         (out (make-array (length in))))
    (parallel-for 0 (length in) 0 (lambda (i)
                                    (setf (svref out i) (funcall fn (svref in i)))
                                    (values)))
    (coerce out 'list)))
