;;;; pool.lisp
;;;;
;;;; Worker threads, the search for work, and the sleep protocol.
;;;;
;;;; Each worker owns a deque. A worker looking for work pops its own
;;;; deque, then steals from the other workers' deques, starting at a
;;;; random victim, and finally takes work injected by threads outside
;;;; the pool.
;;;;
;;;; Sleep protocol. Every worker is ACTIVE, SEARCHING or SLEEPING. A
;;;; worker that runs out of work searches for a while (spinning, then
;;;; yielding), then sleeps on its parker. Two counters, SEARCHING and
;;;; SLEEPING, make it cheap for producers of work to decide whether
;;;; anyone needs to be woken: a producer wakes a sleeper only if
;;;; nobody is searching. A searcher that finds work wakes up to two
;;;; sleepers, as in Rayon, so that the pool ramps up quickly when work
;;;; keeps appearing. The waker counts the woken worker as SEARCHING
;;;; on its behalf, which prevents a thundering herd. Only workers at
;;;; the top level of their stack count as SEARCHING: a worker waiting
;;;; for a job may not be able to run new work (see "Domains" in
;;;; job.lisp), so it must not stop producers from waking one that can.
;;;;
;;;; A worker about to sleep registers in an idle list and as SLEEPING,
;;;; stops counting as SEARCHING, issues a full fence, and looks for
;;;; work once more before parking. There are two idle lists: workers at
;;;; the top level of their stack can run any job, while workers waiting
;;;; for a job to complete can only run some (see "Domains" in
;;;; job.lisp), so wakeups prefer the former.
;;;;
;;;; Jobs that do not go into the deque of the worker creating them go
;;;; through queues: the injector, for fork-join jobs of the root domain
;;;; submitted from outside the pool, and the top-level queue, for the
;;;; tickets of futures (see job.lisp) and for tasks spawned into a
;;;; scope of another domain. The latter are also posted to the scope's
;;;; inbox, from which the scope's owner takes them while it waits.
;;;; Enqueuing is followed by a fence,
;;;; then by a look at the counters to wake a sleeping worker that can
;;;; run the job, so a queued job is either seen by such a worker about
;;;; to sleep or wakes one up. Pushes onto a worker's own deque fence
;;;; only when the deque was empty; a wakeup that is missed in a race
;;;; there costs parallelism but not progress, because the pushing
;;;; worker runs the job itself if nobody steals it.
;;;;
;;;; The pool has a fixed number of workers. A waiting worker that may
;;;; run none of the work there is sleeps: whatever it waits for is
;;;; either running on some worker, or within reach of a worker that
;;;; may run it, as explained under "Domains" in job.lisp.

(in-package #:coalton/threads/runtime)

(defconstant +search-rounds+ 64
  "Rounds of unsuccessful searching for work before a worker sleeps.")

(defconstant +spin-rounds+ 16
  "Rounds of searching during which a worker spins instead of yielding
its processor.")

(defstruct (worker (:constructor %make-worker (pool index rng))
                   (:copier nil)
                   (:predicate nil))
  (pool nil :read-only t)
  (index 0 :type fixnum :read-only t)
  (deque (make-deque) :type deque :read-only t)
  (parker (make-parker) :type parker :read-only t)
  ;; State of a xorshift random number generator, used to pick victims.
  (rng 1 :type (unsigned-byte 32))
  ;; The condition handlers and restarts in effect at the base of the
  ;; worker thread, as captured by BACKEND:CAPTURE-HANDLERS-AND-RESTARTS.
  ;; See RUN-CLAIMED-JOB.
  (base-handlers-and-restarts nil)
  (thread nil))

(backend:freeze-type worker)

(defvar *worker* nil
  "The worker running on the current thread, or NIL if the current
thread is not a worker.")

(declaim (type (or null worker) *worker*))

;;; The pool

(defstruct (pool (:constructor %make-pool (size))
                 (:copier nil)
                 (:predicate nil))
  (size 1 :type (integer 1 #.most-positive-fixnum) :read-only t)
  ;; The SIZE workers, set once when the pool starts.
  (workers #() :type simple-vector)
  ;; Fork-join jobs of the root domain submitted by threads outside the
  ;; pool, which workers outside any future may run even while waiting.
  (injector (backend:make-queue :name "Coalton task injector") :read-only t)
  ;; Jobs that only workers at the top level of their stack take from
  ;; here: futures, and tasks spawned into a scope of another domain.
  (top-level-queue (backend:make-queue :name "Coalton top-level jobs") :read-only t)
  ;; Number of workers in the SEARCHING and SLEEPING states.
  (searching 0 :type backend:atomic-word)
  (sleeping 0 :type backend:atomic-word)
  ;; The sleeping workers, protected by IDLE-LOCK: those at the top
  ;; level of their stack, which can run any job, and those waiting for
  ;; a job to complete, which can only run some jobs.
  (idle-lock (backend:make-mutex :name "Coalton pool idle list") :read-only t)
  (idle-workers '() :type list)
  (idle-waiters '() :type list)
  (shutdown nil :type boolean)
  ;; Special bindings established in every worker thread.
  (bindings '() :type list))

(backend:freeze-type pool)

(declaim (inline worker-pool* random-index notify-work push-local))

(defun worker-pool* (worker)
  (declare (type worker worker))
  (the pool (worker-pool worker)))

(defun random-index (worker limit)
  "A pseudo-random integer in [0, LIMIT), from WORKER's generator."
  (declare (type worker worker)
           (type (integer 1 #.most-positive-fixnum) limit))
  (let ((x (worker-rng worker)))
    (declare (type (unsigned-byte 32) x))
    (setf x (logxor x (ldb (byte 32 0) (ash x 13))))
    (setf x (logxor x (ash x -17)))
    (setf x (logxor x (ldb (byte 32 0) (ash x 5))))
    (setf (worker-rng worker) x)
    (mod x limit)))

;;; Running jobs

(defmacro with-base-environment ((worker) &body body)
  "Execute BODY with the condition handlers and restarts that are in
effect at the base of WORKER's thread, and with no deadline.

A worker executes jobs from its main loop, but also while it waits for
a job of its own to complete. In the latter case, the jobs it executes
belong to other computations, and must not see the handlers, restarts
and deadline of the computation that is waiting: otherwise a handler
there could catch a condition signaled by an unrelated task, or a task
could invoke a restart of a computation it knows nothing about."
  `(backend:with-handlers-and-restarts ((worker-base-handlers-and-restarts ,worker))
     (backend:without-deadline
       ,@body)))

(defun run-claimed-job (worker job)
  "Run JOB, which the current thread has claimed, on WORKER: in the base
environment of WORKER's thread, in the domain of JOB, and with an ABORT
restart that abandons JOB alone, rather than whatever computation
WORKER was in the middle of."
  (declare (type worker worker)
           (type job job))
  (with-base-environment (worker)
    (let ((*domain* (job-domain job)))
      (restart-case (run-job job)
        (abort ()
          :report "Abandon this task."
          nil))))
  (values))

(defun execute-job (worker job)
  "Run JOB on WORKER as by RUN-CLAIMED-JOB, unless another thread has
already claimed it. If JOB is the ticket of a future, run the future
instead, unless another thread has already claimed the future."
  (declare (type worker worker)
           (type job job))
  (when (claim-job job)
    (if (ticket-p job)
        (let ((future (ticket-future job)))
          (setf (ticket-future job) nil)
          (when (and future (claim-job future))
            (setf (future-ticket future) nil)
            (run-claimed-job worker future)))
        (run-claimed-job worker job)))
  (values))

;;; Finding work

(defun steal-work (worker acceptable-p)
  "Steal a job accepted by ACCEPTABLE-P, or any job if it is NIL, from
another worker of WORKER's pool. Returns NIL if there is none."
  (declare (type worker worker)
           (type (or null function) acceptable-p))
  (let* ((workers (pool-workers (worker-pool* worker)))
         (n (length workers)))
    (when (> n 1)
      (loop
        (let ((retry nil)
              (start (random-index worker n)))
          (dotimes (k n)
            (let* ((i (+ start k))
                   (victim (svref workers (if (>= i n) (- i n) i))))
              (unless (eq victim worker)
                (let ((item (deque-steal (worker-deque victim) acceptable-p)))
                  (cond ((or (null item) (eq item :refused)))
                        ((eq item :retry) (setf retry t))
                        (t (return-from steal-work item)))))))
          (unless retry
            (return nil)))))))

(defun take-from-queue (queue)
  "Dequeue the first job of QUEUE that no thread has claimed, discarding
claimed ones, or return NIL if there is none."
  (declare (type backend:queue queue))
  (loop
    (let ((job (backend:dequeue queue)))
      (when (or (null job) (not (job-claimed-p job)))
        (return job)))))

(defun find-work (worker)
  "Find a job for WORKER to run at the top level of its stack, where it
may run any job: its own newest job, else one stolen from another
worker, else one submitted from outside the pool, else one of the jobs
that only workers at the top level may run. Returns NIL if there is
none."
  (declare (type worker worker))
  (let ((deque (worker-deque worker))
        (pool (worker-pool* worker)))
    (loop
      (let ((job (or (deque-pop deque)
                     (steal-work worker nil)
                     (take-from-queue (pool-injector pool))
                     (take-from-queue (pool-top-level-queue pool)))))
        (cond ((null job)
               (return nil))
              ((job-claimed-p job)
               ;; A stale entry for a job that some thread has claimed
               ;; elsewhere.
               nil)
              (t
               (return job)))))))

(defun acceptable-while-waiting-p (job)
  "Should a worker that is waiting for a job steal JOB? Yes if it may run
JOB, and also if JOB is a stale entry for a job that some thread has
claimed, so as to discard it."
  (declare (type job job))
  (or (job-claimed-p job)
      (runnable-while-waiting-p job)))

(defun steal-while-waiting (worker)
  "Find a job that WORKER may run while it waits, elsewhere than in its
own deque: steal one, or else, outside any future, take one submitted
from outside the pool. Returns NIL if there is none."
  (declare (type worker worker))
  (loop
    (let ((job (or (steal-work worker #'acceptable-while-waiting-p)
                   (and (null *domain*)
                        (take-from-queue (pool-injector (worker-pool* worker)))))))
      (when (or (null job) (not (job-claimed-p job)))
        (return job)))))

(defun find-claimable-in-place (worker)
  "A job anywhere in the deques of WORKER's pool that WORKER, which is
waiting, may claim in place, or NIL. See CLAIMABLE-IN-PLACE-P."
  (declare (type worker worker))
  (let* ((workers (pool-workers (worker-pool* worker)))
         (n (length workers))
         (start (random-index worker n)))
    (dotimes (k n)
      (let* ((i (+ start k))
             (job (deque-find-if #'claimable-in-place-p
                                 (worker-deque (svref workers (if (>= i n) (- i n) i))))))
        (when job
          (return job))))))

(defun find-work-while-waiting (worker inbox)
  "Find a job that WORKER may run while it waits for a job to complete,
or return NIL: from its own deque, else from INBOX, the inbox of the
scope it waits for if any, else by stealing, else anywhere in a deque.
See \"Domains\" in job.lisp."
  (declare (type worker worker)
           (type (or null backend:queue) inbox))
  (let ((deque (worker-deque worker)))
    (flet ((elsewhere ()
             (or (and inbox (take-from-queue inbox))
                 (steal-while-waiting worker)
                 (find-claimable-in-place worker))))
      (loop
        ;; Look before popping: a job that we may not run must stay in
        ;; the deque, where the workers that may run it can find it.
        (let ((job (deque-peek-bottom deque)))
          (when (null job)
            (deque-release-stolen deque)
            (return (elsewhere)))
          (unless (or (job-claimed-p job)
                      (runnable-while-waiting-p job)
                      (not (job-helpable-p job)))
            ;; A fork-join job of an enclosing domain, belonging to a
            ;; computation lower on this worker's stack.
            (return (elsewhere)))
          ;; A thief may take the job meanwhile, and another waiting
          ;; worker may claim it in place.
          (let ((popped (deque-pop deque)))
            (cond ((null popped)
                   (return (elsewhere)))
                  ((job-claimed-p popped)
                   ;; A stale entry: discard it.
                   nil)
                  ((runnable-while-waiting-p popped)
                   (return popped))
                  ((not (job-helpable-p popped))
                   ;; A future that nobody has started: leave it to the
                   ;; workers at the top level, or to whoever awaits it.
                   (enqueue-top-level (worker-pool* worker) popped))
                  (t
                   ;; Not what we looked at; cannot happen, as only
                   ;; this worker pops. Put it back.
                   (deque-push deque popped)
                   (return (elsewhere))))))))))

(defun visible-work-p (worker waiting-p inbox)
  "True if there appeared to be work that WORKER could find: any work if
WAITING-P is false, or else work that it may run while it waits,
including in INBOX if non-NIL."
  (declare (type worker worker)
           (type (or null backend:queue) inbox))
  (let ((pool (worker-pool* worker)))
    (if waiting-p
        (or (and inbox (not (backend:queue-empty-p inbox)))
            (and (null *domain*)
                 (not (backend:queue-empty-p (pool-injector pool))))
            ;; Stale entries count as work, because there may be work
            ;; beneath them once they are discarded.
            (loop :for other :across (pool-workers pool)
              :thereis (if (eq other worker)
                           (let ((job (deque-peek-bottom (worker-deque other))))
                             (and job
                                  (or (acceptable-while-waiting-p job)
                                      ;; A future to pass on; see
                                      ;; FIND-WORK-WHILE-WAITING.
                                      (not (job-helpable-p job)))))
                           (let ((job (deque-peek-top (worker-deque other))))
                             (and job
                                  (acceptable-while-waiting-p job)))))
            (find-claimable-in-place worker))
        (or (not (backend:queue-empty-p (pool-injector pool)))
            (not (backend:queue-empty-p (pool-top-level-queue pool)))
            (loop :for other :across (pool-workers pool)
                  :thereis (deque-maybe-non-empty-p (worker-deque other)))))))

;;; Waking and sleeping

(defun wake-one (pool &optional top-level-only)
  "Wake one sleeping worker of POOL, preferring one at the top level of
its stack, which can run any job, and counting it as searching. If
TOP-LEVEL-ONLY, wake only such a worker. Returns true if a worker was
woken."
  (declare (type pool pool))
  ;; A worker taken out of the idle list must be unparked.
  (backend:uninterruptibly
    (let ((worker (backend:with-mutex ((pool-idle-lock pool))
                    (let ((worker (pop (pool-idle-workers pool))))
                      (cond (worker
                             (backend:atomic-incf (pool-searching pool)))
                            ((not top-level-only)
                             (setf worker (pop (pool-idle-waiters pool)))))
                      (when worker
                        (backend:atomic-decf (pool-sleeping pool)))
                      worker))))
      (when worker
        (unpark (worker-parker worker))
        t))))

(defun notify-work (pool)
  "Called after pushing a job onto a deque of POOL: wake a sleeping
worker if no worker at the top level is searching."
  (declare (type pool pool))
  (when (and (zerop (pool-searching pool))
             (plusp (pool-sleeping pool)))
    (wake-one pool))
  (values))

(defun wake-helpers (pool)
  "Called by a worker of POOL that found work after searching. Work tends
to come in bursts, so wake up to two sleeping workers to help, as Rayon
does: workers then come out of sleep in a tree rather than in a chain."
  (declare (type pool pool))
  (dotimes (i 2)
    (when (plusp (pool-sleeping pool))
      (wake-one pool)))
  (values))

(defun idle (worker job registered inbox)
  "Called when WORKER found no work. Search for work, and sleep if none
turns up, until either some job has been executed, JOB (if non-NIL) has
completed, or (if JOB is NIL) the pool is shutting down. If JOB is
non-NIL, WORKER is waiting for JOB, and looks only for work that it may
run while waiting, including in INBOX if non-NIL.

REGISTERED tells whether WORKER's parker is already registered as a
waiter of JOB. Returns the updated value of REGISTERED.

Interrupts can only unwind IDLE while it runs a job, pauses between two
searches, or sleeps, and deadlines only while it runs a job. Either way,
WORKER leaves the sleep protocol properly."
  (declare (type worker worker)
           (type (or null job) job)
           (type (or null backend:queue) inbox))
  (let ((pool (worker-pool* worker))
        (parker (worker-parker worker))
        (waiting-p (not (null job)))
        ;; :SEARCHING while WORKER counts as searching, which only
        ;; workers at the top level do, :ASLEEP while it is in an idle
        ;; list, and NIL otherwise: what to undo on exit.
        (state nil))
    (labels ((finished-p ()
               (if job
                   (job-done-p job)
                   (pool-shutdown pool)))
             (find-some-work ()
               (if job
                   (find-work-while-waiting worker inbox)
                   (find-work worker)))
             (leave-idle-list ()
               ;; Returns true if WORKER was still in its idle list, and
               ;; false if a waker took it out (counting it as searching,
               ;; at the top level).
               (backend:with-mutex ((pool-idle-lock pool))
                 (let ((list (if waiting-p
                                 (pool-idle-waiters pool)
                                 (pool-idle-workers pool))))
                   (when (member worker list :test #'eq)
                     (setf list (delete worker list :test #'eq))
                     (if waiting-p
                         (setf (pool-idle-waiters pool) list)
                         (setf (pool-idle-workers pool) list))
                     (backend:atomic-decf (pool-sleeping pool))
                     t)))))
      (declare (inline finished-p find-some-work))
      (backend:uninterruptibly
        (unwind-protect
             (progn
               (unless waiting-p
                 (backend:atomic-incf (pool-searching pool))
                 (setf state :searching))
               (loop
                 (dotimes (round +search-rounds+)
                   (when (finished-p)
                     (return-from idle registered))
                   (let ((found (find-some-work)))
                     (when found
                       (when (eq state :searching)
                         (backend:atomic-decf (pool-searching pool))
                         (setf state nil))
                       (wake-helpers pool)
                       (backend:with-local-interrupts
                         (execute-job worker found))
                       (return-from idle registered)))
                   (backend:with-local-interrupts
                     (if (< round +spin-rounds+)
                         (backend:spin-pause)
                         (backend:thread-yield))))
                 ;; Go to sleep.
                 (backend:with-mutex ((pool-idle-lock pool))
                   (cond (waiting-p
                          (push worker (pool-idle-waiters pool)))
                         (t
                          (push worker (pool-idle-workers pool))
                          (backend:atomic-decf (pool-searching pool))))
                   (backend:atomic-incf (pool-sleeping pool))
                   (setf state :asleep))
                 (when (and job (not registered))
                   (setf registered (add-job-waiter job parker)))
                 (setf (parker-asleep parker) t)
                 ;; Producers of work make it visible, fence, then look
                 ;; at the sleep counters, and at the waiters of the job
                 ;; that the work is for; so either they see us asleep,
                 ;; or we see the work.
                 (backend:full-fence)
                 (unless (or (finished-p) (visible-work-p worker waiting-p inbox))
                   (backend:with-local-interrupts
                     (park parker)))
                 ;; Awake: search again. Unless a waker took us out of
                 ;; the idle list, counting us as searching, do both.
                 (setf (parker-asleep parker) nil)
                 (let ((listed (leave-idle-list)))
                   (cond (waiting-p
                          (setf state nil))
                         (t
                          (when listed
                            (backend:atomic-incf (pool-searching pool)))
                          (setf state :searching))))))
          (case state
            (:searching
             (backend:atomic-decf (pool-searching pool)))
            (:asleep
             (setf (parker-asleep parker) nil)
             (unless (or (leave-idle-list) waiting-p)
               (backend:atomic-decf (pool-searching pool))))))))))

(defun wait-for-job (worker job &optional inbox)
  "Run other work on WORKER, as permitted while waiting, until JOB has
completed. INBOX, if non-NIL, is the inbox of the scope whose latch is
JOB."
  (declare (type worker worker)
           (type job job)
           (type (or null backend:queue) inbox))
  (let ((registered nil))
    (loop :until (job-done-p job)
          :do (let ((found (find-work-while-waiting worker inbox)))
                (if found
                    (execute-job worker found)
                    (setf registered (idle worker job registered inbox))))))
  ;; Whatever JOB did must be visible to the caller.
  (backend:acquire-fence)
  (values))

;;; Submitting work

(defun push-local (worker job)
  "Push JOB onto WORKER's own deque, waking a sleeping worker if needed."
  (declare (type worker worker)
           (type job job))
  (when (deque-push (worker-deque worker) job)
    ;; The deque was empty, so idle workers may be going to sleep
    ;; without having seen any work here. Make the push visible before
    ;; reading the sleep counters.
    (backend:full-fence))
  (notify-work (worker-pool* worker)))

(defun inject (pool job)
  "Add JOB, a fork-join job of the root domain submitted from outside
POOL, to its injection queue, waking a sleeping worker if any."
  (declare (type pool pool)
           (type job job))
  (backend:enqueue job (pool-injector pool))
  (backend:full-fence)
  (when (plusp (pool-sleeping pool))
    (wake-one pool))
  (values))

(defun discard-stale-jobs (queue)
  "Discard the jobs at the head of QUEUE that some thread has claimed
elsewhere, moving the first one that nobody has claimed to the back of
QUEUE. Called before adding a job to a queue whose entries go stale when
their jobs run elsewhere, so that stale entries do not pile up while
nobody takes jobs from the queue."
  (declare (type backend:queue queue))
  (loop
    (let ((job (backend:dequeue queue)))
      (cond ((null job)
             (return))
            ((not (job-claimed-p job))
             (backend:enqueue job queue)
             (return)))))
  (values))

(defun enqueue-top-level (pool job)
  "Add JOB to POOL's queue of jobs that only workers at the top level of
their stack take, waking a sleeping such worker if there is one."
  (declare (type pool pool)
           (type job job))
  ;; Entries can go stale while waiting in the queue, when somebody runs
  ;; their job elsewhere; workers at the top level discard them, but may
  ;; be busy for a long time. Bound their number.
  (discard-stale-jobs (pool-top-level-queue pool))
  (backend:enqueue job (pool-top-level-queue pool))
  (backend:full-fence)
  (when (plusp (pool-sleeping pool))
    (wake-one pool t))
  (values))

;;; Worker threads

(defun worker-loop (worker)
  "Execute jobs until the pool of WORKER shuts down."
  (declare (type worker worker))
  (let ((pool (worker-pool* worker)))
    (loop
      (let ((job (find-work worker)))
        (cond (job
               (execute-job worker job))
              ((pool-shutdown pool)
               ;; Look once more, now that the shutdown is visible: a
               ;; job submitted before the shutdown began must not be
               ;; left behind.
               (backend:full-fence)
               (let ((job (find-work worker)))
                 (if job
                     (execute-job worker job)
                     (return))))
              (t
               (idle worker nil nil nil))))))
  (values))

(defun worker-main (worker)
  (declare (type worker worker))
  (with-bindings ((pool-bindings (worker-pool* worker)))
    (let ((*worker* worker))
      (loop
        (with-simple-restart (abort "Abandon the current task and return to the Coalton worker loop.")
          (setf (worker-base-handlers-and-restarts worker)
                (backend:capture-handlers-and-restarts))
          (worker-loop worker)
          (return)))))
  (values))

(defun worker-seed (index)
  "A non-zero xorshift seed for the worker numbered INDEX."
  (let ((seed (ldb (byte 32 0) (* 2654435761 (1+ index)))))
    (if (zerop seed) 1 seed)))

(defun start-worker-thread (worker)
  "Start the thread of WORKER, and return it."
  (declare (type worker worker))
  (backend:make-thread (lambda () (worker-main worker))
                       :name (format nil "Coalton worker ~D" (worker-index worker))))

(defun stop-pool (pool)
  "Ask the workers of POOL to exit once no work is left, and wait for
them to do so."
  (declare (type pool pool))
  (setf (pool-shutdown pool) t)
  (backend:full-fence)
  (let ((workers (pool-workers pool)))
    (loop :for worker :across workers
          :do (unpark (worker-parker worker)))
    (loop :for worker :across workers
          :for thread := (worker-thread worker)
          :when thread
            :do (backend:join-thread thread)))
  (values))

(defun start-pool (size)
  "Create a pool of SIZE workers and start their threads."
  (declare (type (integer 1 #.most-positive-fixnum) size))
  (let ((pool (%make-pool size))
        (workers (make-array size)))
    (dotimes (i size)
      (setf (svref workers i) (%make-worker pool i (worker-seed i))))
    (setf (pool-workers pool) workers
          (pool-bindings pool) (capture-standard-bindings))
    (let ((started nil))
      (unwind-protect
           (progn
             (loop :for worker :across workers
                   :do (setf (worker-thread worker) (start-worker-thread worker)))
             (setf started t))
        (unless started
          (stop-pool pool))))
    pool))

;;; The global pool

(backend:defglobal **pool** nil
  "The pool used by all Coalton parallel operations, started on first use.")

(backend:defglobal **pool-lock** (backend:make-mutex :name "Coalton global pool")
  "Protects changes to **POOL** and **CONFIGURED-WORKER-COUNT**.")

(backend:defglobal **configured-worker-count** nil
  "The size of pools started from now on, or NIL for the default.")

(declaim (type (or null pool) **pool**)
         (type (or null (integer 1 #.most-positive-fixnum)) **configured-worker-count**))

(declaim (inline ensure-pool))

(defun %start-global-pool ()
  (backend:with-mutex (**pool-lock**)
    (or **pool**
        (setf **pool** (start-pool (or **configured-worker-count**
                                       (default-worker-count)))))))

(defun ensure-pool ()
  "The global pool, started if necessary."
  (or **pool** (%start-global-pool)))

(defun submit-from-outside (job)
  "Submit JOB to the global pool from a thread that is not a worker."
  (declare (type job job))
  (loop
    (let ((pool (ensure-pool)))
      (if (job-helpable-p job)
          (inject pool job)
          (enqueue-top-level pool job))
      ;; A pool that is shutting down may have no workers left to see
      ;; JOB. If no worker has claimed JOB, take it back and submit it to
      ;; the next pool. (A stale copy left in the old pool is harmless:
      ;; whoever claims the job first runs it.)
      (unless (and (pool-shutdown pool) (claim-job job))
        (return))
      (setf (job-claim job) 0)))
  (values))

(defun submit (job)
  "Schedule JOB, the ticket of a future (see SPAWN-FUTURE), on the global
pool. May be called from any thread."
  (declare (type job job))
  (let ((worker *worker*))
    (cond (worker
           ;; A worker that spawns futures and awaits them as they finish
           ;; may never pop its deque, which is where the slots of jobs
           ;; that thieves took are cleared otherwise.
           (deque-release-stolen (worker-deque worker))
           (push-local worker job))
          (t
           (submit-from-outside job))))
  (values))

(defun worker-count ()
  "The number of workers of the global pool, whether or not it has
been started yet."
  (let ((pool **pool**))
    (if pool
        (pool-size pool)
        (or **configured-worker-count** (default-worker-count)))))

(defun shutdown ()
  "Stop the global pool, waiting for its workers to finish all
submitted work and exit. The next parallel operation starts a new
pool. Must not be called from a worker, nor while other threads are
waiting for parallel work."
  (when *worker*
    (panic "The Coalton worker pool cannot be shut down from one of its own workers."))
  (let ((pool (backend:with-mutex (**pool-lock**)
                (shiftf **pool** nil))))
    (when pool
      (stop-pool pool)))
  (values))

(defun set-worker-count (count)
  "Use COUNT workers for parallel work from now on, or the default number
if COUNT is NIL. Stops the global pool if it is running; the next
parallel operation starts a pool of the new size."
  (unless (typep count '(or null (integer 1 #.most-positive-fixnum)))
    (panic "The number of workers must be a positive integer, not ~S." count))
  (when *worker*
    (panic "The Coalton worker pool cannot be resized from one of its own workers."))
  (backend:with-mutex (**pool-lock**)
    (setf **configured-worker-count** count))
  (shutdown)
  (values))

(defun current-worker-index ()
  "The index of the worker running the current thread, in [0, (WORKER-COUNT)),
or NIL if the current thread is not a worker."
  (let ((worker *worker*))
    (and worker (worker-index worker))))

;;; An image cannot be saved while threads other than the main thread
;;; are running. The pool is restarted on first use after the image is
;;; loaded.

(defun shutdown-before-save ()
  (shutdown))

(backend:register-image-save-hook 'shutdown-before-save)
