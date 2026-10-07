;;;; runtime-tests.lisp
;;;;
;;;; Tests of the work-stealing runtime underlying coalton/threads.

(in-package #:coalton-tests/threads)

(defvar *deadline-expired* nil
  "Set when a test ran out of time, presumably because the pool
deadlocked, so that the test's pool is abandoned rather than shut down,
which would wait forever.")

(defun abandon-pool ()
  "Terminate the workers of the global pool without waiting for its
work, and start afresh."
  (let ((pool (shiftf rt::**pool** nil)))
    (when pool
      (loop :for worker :across (rt::pool-workers pool)
            :for thread := (rt::worker-thread worker)
            :when (and thread (backend:thread-alive-p thread))
              :do (terminate-thread thread))))
  (setf *deadline-expired* nil))

(defmacro with-worker-count ((count) &body body)
  "Run BODY with a fresh pool of COUNT workers, restoring the default
pool size afterwards."
  `(progn
     (rt:set-worker-count ,count)
     (unwind-protect (progn ,@body)
       (when *deadline-expired*
         (abandon-pool))
       (rt:set-worker-count nil))))

(defun call-with-deadline (seconds fn)
  "Call FN and return its values, failing with an error if it takes
longer than SECONDS, which indicates a hang. FN runs in the current
thread, so that test assertions in it are recorded."
  (let* ((test-thread (backend:current-thread))
         (finished (backend:make-semaphore :name "Coalton threads test deadline"))
         (watchdog
           (backend:make-thread
            (lambda ()
              (unless (backend:wait-on-semaphore finished :timeout seconds)
                (setf *deadline-expired* t)
                (interrupt-thread
                 test-thread
                 (lambda ()
                   (error "The test did not finish within ~D seconds." seconds)))))
            :name "Coalton threads test watchdog")))
    (unwind-protect (funcall fn)
      (backend:signal-semaphore finished)
      (backend:join-thread watchdog))))

(defmacro with-deadline ((seconds) &body body)
  `(call-with-deadline ,seconds (lambda () ,@body)))

(defmacro atomic-push (item place)
  "Push ITEM onto the list in PLACE, the CAR or CDR of a cons, atomically."
  (let ((new (gensym "NEW"))
        (old (gensym "OLD")))
    `(let ((,new (list ,item)))
       (loop
         (let ((,old ,place))
           (setf (cdr ,new) ,old)
           (when (eq ,old (backend:compare-and-swap ,place ,old ,new))
             (return ,new)))))))

(defun wait-until (predicate &optional (seconds 2))
  "Wait, for up to SECONDS seconds, until PREDICATE returns true, and
return its last value."
  (loop :repeat (* 1000 seconds)
        :until (funcall predicate)
        :do (sleep 0.001))
  (funcall predicate))

(defun await-outcome (future)
  "The value of FUTURE, or :ABORTED if its task was abandoned."
  (handler-case (rt:await-future future)
    (rt:task-aborted () :aborted)))

(defun rt-test-fib (n)
  (if (< n 2)
      n
      (multiple-value-bind (a b)
          (rt:join2 (lambda () (rt-test-fib (- n 1)))
                    (lambda () (rt-test-fib (- n 2))))
        (+ a b))))

(define-condition rt-test-error (error)
  ((tag :initarg :tag :reader rt-test-error-tag)))

(define-condition rt-test-signal (condition) ())

;;; The deque

(deftest threads-deque-sequential ()
  (let ((deque (rt::make-deque)))
    (is (null (rt::deque-pop deque)))
    (is (null (rt::deque-steal deque)))
    ;; Enough items to force the buffer to grow several times.
    (loop :for i :from 1 :to 1000 :do (rt::deque-push deque i))
    (is (= 1 (rt::deque-steal deque)))
    (is (= 2 (rt::deque-steal deque)))
    (is (= 1000 (rt::deque-pop deque)))
    (is (= 999 (rt::deque-pop deque)))
    (is (equal (loop :for x := (rt::deque-pop deque) :while x :collect x)
               (loop :for i :from 998 :downto 3 :collect i)))
    (is (null (rt::deque-pop deque)))
    (is (null (rt::deque-steal deque)))
    ;; Reusable after being emptied.
    (rt::deque-push deque :a)
    (is (eq :a (rt::deque-steal deque)))
    (is (null (rt::deque-pop deque)))))

(deftest threads-deque-items-taken-exactly-once ()
  ;; One owner pushes and pops while several thieves steal. Every item
  ;; must be taken exactly once.
  (let* ((n 200000)
         (deque (rt::make-deque))
         (done nil)
         (thieves
           (loop :repeat 4
                 :collect (backend:make-thread
                           (lambda ()
                             (let ((taken '()))
                               (loop
                                 (let ((item (rt::deque-steal deque)))
                                   (cond ((eq item :retry))
                                         (item (push item taken))
                                         (done (return)))))
                               taken)))))
         (owner-taken '()))
    (dotimes (i n)
      (rt::deque-push deque (1+ i))
      (when (zerop (mod i 3))
        (let ((item (rt::deque-pop deque)))
          (when item
            (push item owner-taken)))))
    (loop :for item := (rt::deque-pop deque)
          :while item
          :do (push item owner-taken))
    (setf done t)
    (let ((seen (make-array (1+ n) :element-type 'bit :initial-element 0))
          (count 0)
          (duplicates 0))
      (flet ((note (item)
               (incf count)
               (if (= 1 (sbit seen item))
                   (incf duplicates)
                   (setf (sbit seen item) 1))))
        (mapc #'note owner-taken)
        (dolist (thief thieves)
          (mapc #'note (backend:join-thread thief))))
      (is (= 0 duplicates))
      (is (= n count)))))

(deftest threads-deque-clears-stolen-slots ()
  ;; Thieves cannot clear the slots of the items they take, so the owner
  ;; does, once it finds its deque empty, lest the items stay reachable.
  (let ((deque (rt::make-deque)))
    (rt::deque-push deque (list :a))
    (rt::deque-push deque (list :b))
    (is (equal '(:a) (rt::deque-steal deque)))
    (is (equal '(:b) (rt::deque-steal deque)))
    (is (null (rt::deque-pop deque)))
    (is (every #'null (rt::deque-buffer deque)))))

;;; Fork-join

(deftest threads-join-computes-values ()
  (dolist (workers '(1 2 3 8))
    (with-worker-count (workers)
      (is (= 6765 (with-deadline (60) (rt-test-fib 20))))
      ;; From a worker as well as from outside the pool.
      (is (= 6765 (rt:call-in-pool (lambda () (rt-test-fib 20))))))))

(deftest threads-join-concurrent-callers ()
  ;; Several threads outside the pool use it at the same time.
  (with-worker-count (4)
    (let ((threads (loop :repeat 8
                         :collect (backend:make-thread (lambda () (rt-test-fib 18))))))
      (is (every (lambda (thread) (= 2584 (backend:join-thread thread)))
                 threads)))))

(deftest threads-join-propagates-errors ()
  (with-worker-count (4)
    (with-deadline (60)
      ;; The condition object itself is signaled again.
      (let ((signaled (make-condition 'rt-test-error :tag :right)))
        (is (eq signaled (handler-case (rt:join2 (lambda () 1) (lambda () (error signaled)))
                           (rt-test-error (c) c)))))
      (is (eq :left (handler-case (rt:join2 (lambda () (error 'rt-test-error :tag :left))
                                            (lambda () 2))
                      (rt-test-error (c) (rt-test-error-tag c)))))
      ;; When both fail, the left error wins.
      (is (eq :left (handler-case (rt:join2 (lambda () (error 'rt-test-error :tag :left))
                                            (lambda () (error 'rt-test-error :tag :right)))
                      (rt-test-error (c) (rt-test-error-tag c)))))
      ;; Errors deep in a tree of joins.
      (is (eq :deep (handler-case (rt:parallel-for 0 100000 0
                                                    (lambda (i)
                                                      (when (= i 77777)
                                                        (error 'rt-test-error :tag :deep))
                                                      (values)))
                      (rt-test-error (c) (rt-test-error-tag c)))))
      ;; The pool still works afterwards.
      (is (= 6765 (rt-test-fib 20))))))

(deftest threads-join-leaves-no-task-behind ()
  ;; However a join exits, its second function must have finished or
  ;; never start: nothing may run after the join is over.
  (with-worker-count (4)
    (with-deadline (60)
      (dotimes (trial 20)
        (let ((counter (list 0)))
          (flet ((slow-right ()
                   (sleep 0.002)
                   (backend:atomic-incf (car counter))
                   nil))
            ;; Run inside the pool, so that the first function of each
            ;; join runs on the thread that established the handler
            ;; and the block.
            (rt:call-in-pool
             (lambda ()
               (handler-case (rt:join2 (lambda () (error 'rt-test-error :tag trial))
                                       #'slow-right)
                 (rt-test-error () nil))
               (block escape
                 (rt:join2 (lambda () (return-from escape))
                           #'slow-right))))
            (let ((after-joins (car counter)))
              (sleep 0.01)
              (is (= after-joins (car counter))))))))))

(deftest threads-tasks-run-in-base-environment ()
  ;; A job executed by a worker must not see the handlers and restarts
  ;; of whatever computation that worker is in the middle of.
  (with-worker-count (2)
    (let ((handler-ran nil))
      (multiple-value-bind (restart-visible)
          (rt:call-in-pool
           (lambda ()
             (restart-case
                 (handler-bind ((rt-test-signal (lambda (c)
                                                  (declare (ignore c))
                                                  (setf handler-ran t))))
                   (let ((job (rt::%make-job (lambda ()
                                               (signal 'rt-test-signal)
                                               (and (find-restart 'rt-test-restart) t))
                                             nil
                                             :task)))
                     (rt::execute-job rt::*worker* job)
                     (rt::job-value job)))
               (rt-test-restart () :restarted))))
        (is (not handler-ran))
        (is (null restart-visible))))))

(deftest threads-aborted-task-reports-error ()
  ;; Invoking ABORT in a task abandons that task alone, which then
  ;; completes with a TASK-ABORTED error; the worker carries on.
  (with-worker-count (2)
    (with-deadline (60)
      (is (eq :aborted
              (handler-case (rt:await-future
                             (rt:spawn-future (lambda () (invoke-restart (find-restart 'abort)))))
                (rt:task-aborted () :aborted))))
      (is (= 6765 (rt-test-fib 20)))))
  ;; With a single worker, the scope's tasks run while the scope waits
  ;; for them. Aborting one must neither escape the scope nor let it
  ;; return before its other tasks have finished.
  (with-worker-count (1)
    (with-deadline (60)
      (let ((finished (list 0)))
        (is (eq :aborted
                (handler-case
                    (rt:call-with-scope
                     (lambda (scope)
                       (rt:scope-spawn scope (lambda ()
                                               (sleep 0.01)
                                               (backend:atomic-incf (car finished))))
                       (rt:scope-spawn scope (lambda ()
                                               (invoke-restart (find-restart 'abort))))
                       :body))
                  (rt:task-aborted () :aborted))))
        (is (= 1 (car finished)))))))

;;; Futures and scopes

(deftest threads-futures ()
  (with-worker-count (4)
    (with-deadline (60)
      (let ((futures (loop :for i :below 100
                           :collect (let ((i i)) (rt:spawn-future (lambda () (* i i)))))))
        (is (= (loop :for i :below 100 :sum (* i i))
               (reduce #'+ (mapcar #'rt:await-future futures))))
        (is (every #'rt:future-done-p futures))
        ;; Awaiting again returns the same value.
        (is (= 81 (rt:await-future (nth 9 futures)))))
      ;; Futures awaited from inside the pool.
      (is (= 4950 (rt:call-in-pool
                   (lambda ()
                     (let ((futures (loop :for i :below 100
                                          :collect (let ((i i)) (rt:spawn-future (lambda () i))))))
                       (reduce #'+ (mapcar #'rt:await-future futures)))))))
      (let* ((signaled (make-condition 'rt-test-error :tag :future))
             (future (rt:spawn-future (lambda () (error signaled)))))
        (dotimes (i 2)
          (is (eq signaled (handler-case (rt:await-future future)
                             (rt-test-error (c) c)))))))))

(deftest threads-futures-awaiting-futures ()
  ;; A worker waiting for a future must not run, on top of the waiting
  ;; computation, a future that awaits that computation: the stack would
  ;; hold a cycle that the dependencies do not have.
  (dolist (workers '(1 2 4))
    (with-worker-count (workers)
      (with-deadline (60)
        ;; C awaits A, which awaits B, which is slow.
        (let* ((b (rt:spawn-future (lambda () (sleep 0.02) :b)))
               (a (rt:spawn-future (lambda () (list :a (rt:await-future b)))))
               (c (rt:spawn-future (lambda () (list :c (rt:await-future a))))))
          (is (equal '(:c (:a :b)) (rt:await-future c))))
        ;; Chains of futures, created outside and inside the pool, some of
        ;; them running parallel loops while they wait.
        (flet ((chain ()
                 (let ((previous (rt:spawn-future (lambda () (sleep 0.005) 0))))
                   (dotimes (i 40)
                     (let ((before previous))
                       (setf previous
                             (rt:spawn-future
                              (lambda ()
                                (when (evenp i)
                                  (rt:parallel-for 0 1000 0 (lambda (j) (declare (ignore j)) (values))))
                                (1+ (rt:await-future before)))))))
                   (rt:await-future previous))))
          (is (= 40 (chain)))
          (is (= 40 (rt:call-in-pool #'chain))))))))

(deftest threads-scopes ()
  (with-worker-count (4)
    (with-deadline (60)
      (let ((counter (list 0)))
        (is (eq :body (rt:call-with-scope
                       (lambda (scope)
                         (dotimes (i 1000)
                           (rt:scope-spawn scope (lambda () (backend:atomic-incf (car counter)))))
                         :body))))
        (is (= 1000 (car counter))))
      ;; Tasks spawning tasks, and multiple values from the body.
      (let ((counter (list 0)))
        (labels ((spawn-tree (scope depth)
                   (backend:atomic-incf (car counter))
                   (when (plusp depth)
                     (dotimes (i 2)
                       (rt:scope-spawn scope (lambda () (spawn-tree scope (1- depth))))))))
          (is (equal '(1 2)
                     (multiple-value-list
                      (rt:call-with-scope (lambda (scope)
                                            (spawn-tree scope 10)
                                            (values 1 2)))))))
        (is (= 2047 (car counter))))
      (is (eq :scope-task
              (handler-case (rt:call-with-scope
                             (lambda (scope)
                               (rt:scope-spawn scope (lambda () (error 'rt-test-error :tag :scope-task)))
                               :body))
                (rt-test-error (c) (rt-test-error-tag c)))))
      ;; A scope waits for its tasks even when its body exits non-locally.
      (let ((counter (list 0)))
        (handler-case (rt:call-with-scope
                       (lambda (scope)
                         (rt:scope-spawn scope (lambda ()
                                                 (sleep 0.01)
                                                 (backend:atomic-incf (car counter))))
                         (error 'rt-test-error :tag :body)))
          (rt-test-error () nil))
        (is (= 1 (car counter))))
      (let ((escaped nil))
        (rt:call-with-scope (lambda (scope) (setf escaped scope)))
        (is (eq :refused (handler-case (rt:scope-spawn escaped (lambda () nil))
                           (coalton/classes:panic () :refused))))))))

(deftest threads-scopes-across-domains ()
  ;; Scopes shared between a computation and a future it awaits, and
  ;; scopes spawned into from outside the pool. With a single worker,
  ;; these check that tasks spawned into a scope from another domain do
  ;; not end up where the worker, waiting in its own domain, may not
  ;; run them.
  (with-worker-count (1)
    (with-deadline (60)
      ;; Scope S awaits future B, which runs inline. B joins, and the
      ;; first function of the join spawns into S a task that awaits B.
      (let ((result (list nil))
            (b-cell (list nil)))
        (rt:call-with-scope
         (lambda (s)
           (let ((b (rt:spawn-future
                     (lambda ()
                       (rt:join2 (lambda ()
                                   (rt:scope-spawn s (lambda ()
                                                       (setf (car result)
                                                             (rt:await-future (car b-cell)))))
                                   :left)
                                 (lambda () :right))
                       :b))))
             (setf (car b-cell) b)
             (rt:await-future b))))
        (is (eq :b (car result))))
      ;; Scope S awaits future B, which runs inline. B's own scope T
      ;; spawns a task into T, then one into S.
      (let ((ran (list nil)))
        (rt:call-with-scope
         (lambda (s)
           (rt:await-future
            (rt:spawn-future
             (lambda ()
               (rt:call-with-scope
                (lambda (tt)
                  (rt:scope-spawn tt (lambda () (atomic-push :x (car ran))))
                  (rt:scope-spawn s (lambda () (atomic-push :y (car ran))))))
               :b)))))
        (is (equal '(:x :y) (sort (copy-list (car ran)) #'string<))))
      ;; A thread outside the pool spawns into a scope whose owner is the
      ;; only worker a task that runs a parallel loop.
      (let ((count (list 0)))
        (rt:call-with-scope
         (lambda (s)
           (backend:join-thread
            (backend:make-thread
             (lambda ()
               (rt:scope-spawn s (lambda ()
                                   (rt:parallel-for 0 100 0
                                                    (lambda (i)
                                                      (declare (ignore i))
                                                      (backend:atomic-incf (car count))
                                                      (values))))))))))
        (is (= 100 (car count))))
      ;; None of this needs more workers than the pool started with.
      (is (= 1 (length (rt::pool-workers rt::**pool**)))))))

(deftest threads-deeply-nested-domains ()
  ;; The first scenario of THREADS-SCOPES-ACROSS-DOMAINS, nested: each
  ;; level waits in a different domain, on a single worker.
  (with-worker-count (1)
    (with-deadline (60)
      (let ((count (list 0)))
        (labels ((level (depth)
                   (let ((b-cell (list nil)))
                     (rt:call-with-scope
                      (lambda (s)
                        (let ((b (rt:spawn-future
                                  (lambda ()
                                    (rt:join2 (lambda ()
                                                (rt:scope-spawn s (lambda ()
                                                                    (rt:await-future (car b-cell))
                                                                    (backend:atomic-incf (car count))))
                                                (when (plusp depth)
                                                  (level (1- depth)))
                                                :left)
                                              (lambda () :right))
                                    :b))))
                          (setf (car b-cell) b)
                          (rt:await-future b)))))))
          (rt:call-in-pool (lambda () (level 200))))
        (is (= 201 (car count)))
        (is (= 1 (length (rt::pool-workers rt::**pool**))))))))

(deftest threads-scope-tasks-left-behind ()
  ;; A task that spawns into its own scope returns, leaving the new task
  ;; in its worker's deque beneath a future, while that worker goes on
  ;; to a future that awaits the scope's owner. The owner, waiting for
  ;; the scope, must reach the task even though it may not run the
  ;; future above it.
  (with-worker-count (2)
    (with-deadline (60)
      (dotimes (trial 20)
        (is (eq :done
                (rt:await-future
                 (rt:spawn-future
                  (lambda ()
                    (let ((h rt::*domain*))
                      (rt:call-with-scope
                       (lambda (s)
                         (rt:scope-spawn
                          s (lambda ()
                              (rt:spawn-future (lambda () nil))
                              (rt:scope-spawn s (lambda () nil))
                              (rt:spawn-future (lambda () (rt:await-future h)))))
                         (loop :repeat 200000 :do (backend:spin-pause))))
                      :done))))))))))

(deftest threads-scope-rejects-late-spawns ()
  ;; A spawn racing with the end of a scope either joins the scope, which
  ;; then waits for the new task, or fails.
  (with-worker-count (2)
    (with-deadline (120)
      (let* ((handoff (list nil))
             (stop (list nil))
             (late (list 0))
             (spawner
               (backend:make-thread
                (lambda ()
                  (loop :until (car stop)
                        :do (let ((entry (car handoff)))
                              (when entry
                                (ignore-errors
                                 (rt:scope-spawn (car entry)
                                                 (lambda ()
                                                   (when (cadr entry)
                                                     (backend:atomic-incf (car late)))))))))))))
        (dotimes (i 20000)
          (let ((returned (list nil)))
            (rt:call-with-scope (lambda (s)
                                  (setf (car handoff) (cons s returned))
                                  (backend:thread-yield)))
            (setf (car returned) t)))
        (setf (car stop) t)
        (backend:join-thread spawner)
        (is (zerop (car late)))))))

(deftest threads-stale-copies-do-not-accumulate ()
  ;; Tasks spawned into a scope from another domain are also handed to
  ;; the workers at the top level; the copies that the scope's owner
  ;; made stale must not pile up while no worker is at the top level.
  (with-worker-count (1)
    (with-deadline (60)
      (is (rt:call-in-pool
           (lambda ()
             (dotimes (i 200)
               (rt:call-with-scope
                (lambda (s)
                  (rt:await-future
                   (rt:spawn-future (lambda () (rt:scope-spawn s (lambda () nil))))))))
             (< (backend:queue-count (rt::pool-top-level-queue rt::**pool**))
                10)))))))

(deftest threads-scope-inboxes-do-not-accumulate ()
  ;; Tasks spawned into a scope from another domain go to the scope's
  ;; inbox and to the workers at the top level. The inbox entries of the
  ;; tasks that the latter run must not pile up while the scope's owner
  ;; is busy with the scope's body.
  (with-worker-count (2)
    (with-deadline (60)
      (let ((ran (list 0))
            (entries (list nil)))
        (rt:call-in-pool
         (lambda ()
           (rt:call-with-scope
            (lambda (scope)
              (dotimes (i 200)
                (rt:await-future
                 (rt:spawn-future
                  (lambda ()
                    (rt:scope-spawn scope (lambda ()
                                            (backend:atomic-incf (car ran))
                                            nil)))))
                ;; Let the other worker, at the top level, run the task,
                ;; which makes its inbox entry stale before the next one
                ;; is posted.
                (let ((count (1+ i)))
                  (wait-until (lambda () (>= (car ran) count)))))
              (setf (car entries) (backend:queue-count (rt::scope-inbox scope)))))))
        (is (= 200 (car ran)))
        (is (< (car entries) 10))))))

(deftest threads-waiting-workers-release-stolen-jobs ()
  ;; A worker that keeps waiting for jobs that thieves took from its
  ;; deque must not keep those jobs, and their values, alive.
  (with-worker-count (2)
    (with-deadline (60)
      (let ((stolen (list 0))
            (weak '()))
        (rt:call-in-pool
         (lambda ()
           (let ((me rt::*worker*))
             (dotimes (i 40)
               (let ((future (rt:spawn-future
                              (lambda ()
                                (unless (eq rt::*worker* me)
                                  (backend:atomic-incf (car stolen)))
                                (make-array 10000)))))
                 ;; Give the other worker time to steal the future.
                 (sleep 0.002)
                 (push (make-weak-pointer (rt:await-future future)) weak))))
           (collect-garbage)))
        (is (plusp (car stolen)))
        (is (> 3 (count-if #'weak-pointer-value weak)))))))

(deftest threads-awaited-futures-are-released ()
  ;; A future that its awaiter runs inline must not stay in the deque,
  ;; keeping its value alive, whether it was spawned last or not.
  (with-worker-count (1)
    (with-deadline (60)
      (is (> 3 (rt:call-in-pool
                (lambda ()
                  (let ((weak '())
                        (later '()))
                    (dotimes (i 64)
                      (push (make-weak-pointer
                             (rt:await-future
                              (rt:spawn-future (lambda () (make-array 10000)))))
                            weak)
                      ;; Awaited beneath a future that is awaited later.
                      (let ((a (rt:spawn-future (lambda () (make-array 10000)))))
                        (push (rt:spawn-future (lambda () 0)) later)
                        (push (make-weak-pointer (rt:await-future a)) weak)))
                    (collect-garbage)
                    (prog1 (count-if #'weak-pointer-value weak)
                      (mapc #'rt:await-future later))))))))))

(defun await-futures-run-elsewhere (scope elsewhere)
  "Spawn futures, have tasks of SCOPE run them inline on another worker,
then await them here. Returns weak pointers to their values."
  (let ((me rt::*worker*)
        (weak '()))
    ;; Keeps the scope's owner from stealing from this worker's deque.
    (rt:spawn-future (lambda () nil))
    (let ((futures (loop :repeat 32
                         :collect (rt:spawn-future
                                   (lambda ()
                                     (unless (eq rt::*worker* me)
                                       (backend:atomic-incf (car elsewhere)))
                                     (make-array 10000))))))
      ;; Tasks that the scope's owner claims in place, and that run the
      ;; futures inline.
      (dolist (future futures)
        (let ((future future))
          (rt:scope-spawn scope (lambda () (rt:await-future future)))))
      (sleep 0.1)
      (dolist (future futures)
        (push (make-weak-pointer (rt:await-future future)) weak)))
    weak))

(deftest threads-futures-run-elsewhere-are-released ()
  ;; A future that another worker runs inline must not stay in the deque
  ;; of the worker that spawned it either.
  (with-worker-count (2)
    (with-deadline (60)
      (let ((live (list nil))
            (elsewhere (list 0)))
        (rt:call-with-scope
         (lambda (s)
           (rt:scope-spawn
            s (lambda ()
                (let ((weak (await-futures-run-elsewhere s elsewhere)))
                  (collect-garbage)
                  (setf (car live) (count-if #'weak-pointer-value weak)))))
           ;; Let the other worker steal the task above.
           (loop :repeat 2000000 :do (backend:spin-pause))))
        (is (plusp (car elsewhere)))
        (is (> 3 (car live)))))))

(deftest threads-awaiting-futures-in-any-order-is-cheap ()
  ;; Removing the entries of futures run inline takes constant time,
  ;; whichever order the futures are awaited in.
  (with-worker-count (1)
    (with-deadline (120)
      (flet ((time-awaits (oldest-first)
               ;; Returns the sum of the values, and the time taken.
               (rt:call-in-pool
                (lambda ()
                  (let* ((futures (loop :repeat 50000
                                        :collect (rt:spawn-future (lambda () 1))))
                         (start (get-internal-real-time))
                         (sum (reduce #'+ (if oldest-first futures (reverse futures))
                                      :key #'rt:await-future)))
                    (values sum (- (get-internal-real-time) start)))))))
        (multiple-value-bind (oldest-sum oldest-first) (time-awaits t)
          (multiple-value-bind (newest-sum newest-first) (time-awaits nil)
            (is (= 50000 oldest-sum newest-sum))
            (is (< oldest-first
                   (+ (* 10 newest-first) (floor internal-time-units-per-second 20))))))))))

(defun spawn-stolen-future ()
  "Spawn a future on a worker, and give another worker time to take it."
  (rt:call-in-pool
   (lambda ()
     (let ((future (rt:spawn-future (lambda () 1))))
       (sleep 0.01)
       future))))

(deftest threads-futures-do-not-retain-the-pool ()
  ;; Keeping the handle of a finished future must not keep the pool
  ;; that ran it alive.
  (let ((future nil)
        (weak '()))
    (with-worker-count (2)
      (with-deadline (60)
        (setf future (spawn-stolen-future))
        (is (= 1 (rt:await-future future)))
        (setf weak (map 'list (lambda (worker)
                                (make-weak-pointer (rt::worker-deque worker)))
                        (rt::pool-workers rt::**pool**)))))
    (collect-garbage)
    (is (notany #'weak-pointer-value weak))
    (is (= 1 (rt:await-future future)))))

(deftest threads-futures-from-outside-run-inline-are-released ()
  ;; Futures submitted from outside the pool, and run inline by a worker
  ;; that awaits them while no worker is at the top level, must not stay
  ;; in the pool's queues.
  (with-worker-count (2)
    (with-deadline (60)
      (let* ((release (list nil))
             (long (rt:spawn-future (lambda ()
                                      (loop :until (car release) :do (sleep 0.001))
                                      :long)))
             ;; Waits for LONG, running jobs from outside meanwhile.
             (waiter (backend:make-thread
                      (lambda () (rt:call-in-pool (lambda () (rt:await-future long))))))
             (weak '()))
        (sleep 0.05)
        (dotimes (i 64)
          (let ((future (rt:spawn-future (lambda () (make-array 10000)))))
            (push (make-weak-pointer
                   (rt:call-in-pool (lambda () (rt:await-future future))))
                  weak)))
        (let ((queued (backend:queue-count (rt::pool-top-level-queue rt::**pool**))))
          (collect-garbage)
          (let ((live (count-if #'weak-pointer-value weak)))
            (setf (car release) t)
            (is (eq :long (backend:join-thread waiter)))
            (is (> 10 queued))
            (is (> 3 live))))))))

(deftest threads-join-cancels-right-function ()
  ;; When the first function of a join fails before anyone has started
  ;; the second, the second never runs, even when a task of another
  ;; domain was pushed above it.
  (with-worker-count (1)
    (with-deadline (60)
      (let ((right-ran (list nil))
            (task-ran (list nil)))
        (is (eq :failed
                (handler-case
                    (rt:call-with-scope
                     (lambda (s)
                       (rt:await-future
                        (rt:spawn-future
                         (lambda ()
                           (rt:join2 (lambda ()
                                       (rt:scope-spawn s (lambda () (setf (car task-ran) t)))
                                       (error 'rt-test-error :tag :left))
                                     (lambda () (setf (car right-ran) t))))))))
                  (rt-test-error () :failed))))
        (is (car task-ran))
        (is (not (car right-ran)))))))

(defun tasks-overlap-p ()
  "Check that four tasks of a parallel loop run at the same time."
  (let ((running (list 0))
        (overlap (list nil)))
    (rt:parallel-for 0 4 1
                     (lambda (i)
                       (declare (ignore i))
                       (backend:atomic-incf (car running))
                       ;; Wait, up to two seconds, until two tasks have
                       ;; been seen running at the same time.
                       (loop :repeat 2000
                             :until (or (car overlap) (>= (car running) 2))
                             :do (sleep 0.001))
                       (when (>= (car running) 2)
                         (setf (car overlap) t))
                       (backend:atomic-decf (car running))
                       (values)))
    (car overlap)))

(defun sleep-bookkeeping-consistent-p (pool)
  "Check, once the workers of POOL have fallen asleep, that the sleep
counters agree with the idle lists."
  (let ((idle (append (rt::pool-idle-workers pool) (rt::pool-idle-waiters pool))))
    (and (= (rt::pool-sleeping pool) (length idle) (length (rt::pool-workers pool)))
         (= (length idle) (length (remove-duplicates idle)))
         (zerop (rt::pool-searching pool)))))

(defun workers-fall-asleep-consistently-p (pool)
  "Wait for the workers of POOL to fall asleep, and check that the sleep
counters then agree with the idle lists. Workers search for a while
before they sleep, longer on a busy machine."
  (wait-until (lambda () (sleep-bookkeeping-consistent-p pool)) 20))

(deftest threads-unwound-workers-recover ()
  ;; Workers unwound while they sleep or search, by an interrupt or by a
  ;; deadline, must leave the sleep protocol properly; otherwise the
  ;; pool would lose track of sleeping workers and run sequentially.
  (with-worker-count (4)
    (with-deadline (60)
      (is (= 6765 (rt-test-fib 20)))
      (let ((pool rt::**pool**))
        ;; Abort the idle workers, sleeping at the top level.
        (dotimes (round 3)
          (sleep 0.05)
          (loop :for worker :across (rt::pool-workers pool)
                :do (interrupt-thread (rt::worker-thread worker)
                                      (lambda () (abort))))
          (sleep 0.05)
          (is (workers-fall-asleep-consistently-p pool))
          (is (tasks-overlap-p)))
        ;; Abort a worker sleeping while it waits for a stolen job. The
        ;; task waiting is abandoned, but the stolen job still finishes.
        (let* ((released (list nil))
               (finished (list nil))
               (future (rt:spawn-future
                        (lambda ()
                          (rt:join2 (lambda () (sleep 0.05) :left)
                                    (lambda ()
                                      (loop :until (car released) :do (sleep 0.001))
                                      (setf (car finished) t)))))))
          (loop :repeat 2000
                :until (rt::pool-idle-waiters pool)
                :do (sleep 0.001))
          (let ((waiters (rt::pool-idle-waiters pool)))
            (is (= 1 (length waiters)))
            (dolist (worker waiters)
              (interrupt-thread (rt::worker-thread worker)
                                (lambda () (abort)))))
          (sleep 0.02)
          (setf (car released) t)
          (is (eq :aborted (handler-case (rt:await-future future)
                             (rt:task-aborted () :aborted)
                             (:no-error (value) value))))
          (is (car finished)))
        (sleep 0.05)
        (is (workers-fall-asleep-consistently-p pool))
        (is (tasks-overlap-p))
        ;; Deadlines do not apply to workers waiting for tasks.
        (let ((slow (rt:spawn-future (lambda () (sleep 0.1) :slow))))
          (is (eq :slow (rt:call-in-pool
                         (lambda ()
                           (with-blocking-deadline (0.01)
                             (rt:await-future slow)))))))
        (sleep 0.05)
        (is (workers-fall-asleep-consistently-p pool))
        (is (tasks-overlap-p))))))

(deftest threads-unwound-callers-wait-for-their-tasks ()
  (with-worker-count (2)
    (with-deadline (60)
      ;; A thread outside the pool unwound by a deadline while it waits
      ;; for a parallel operation waits for the operation's tasks first.
      ;; (If no worker started the operation before the deadline, it is
      ;; cancelled instead, and none of it runs.)
      (rt:join2 (lambda () nil) (lambda () nil))
      (let ((started (list nil))
            (finished (list nil)))
        (is (eq :unwound
                (handler-case
                    (with-blocking-deadline (0.1)
                      (rt:call-with-scope
                       (lambda (scope)
                         (setf (car started) t)
                         (rt:scope-spawn scope (lambda ()
                                                 (sleep 0.3)
                                                 (setf (car finished) t))))))
                  (serious-condition () :unwound))))
        (is (eq (car started) (car finished))))
      ;; An operation that no worker has started yet is cancelled.
      (let* ((started (list 0))
             (release (list nil))
             (ran (list nil))
             (blockers (loop :repeat 2
                             :collect (rt:spawn-future
                                       (lambda ()
                                         (backend:atomic-incf (car started))
                                         (loop :until (car release) :do (sleep 0.001)))))))
        (is (wait-until (lambda () (= 2 (car started)))))
        (is (eq :unwound
                (handler-case
                    (with-blocking-deadline (0.02)
                      (rt:call-in-pool (lambda () (setf (car ran) t))))
                  (serious-condition () :unwound))))
        (setf (car release) t)
        (mapc #'rt:await-future blockers)
        (sleep 0.05)
        (is (not (car ran)))))))

(deftest threads-aborted-scope-owners-wait-for-their-tasks ()
  ;; A worker aborted while it waits at the end of a scope still waits
  ;; for the scope's tasks before abandoning its own task. Aborting it
  ;; again during that wait abandons the scope's tasks.
  (with-worker-count (2)
    (with-deadline (60)
      (dolist (aborts '(1 2))
        (let* ((owner (list nil))
               (started (list nil))
               (release (list nil))
               (finished (list nil))
               (future
                 (rt:spawn-future
                  (lambda ()
                    (rt:call-with-scope
                     (lambda (scope)
                       (setf (car owner) rt::*worker*)
                       (rt:scope-spawn scope (lambda ()
                                               (setf (car started) t)
                                               (loop :until (car release) :do (sleep 0.001))
                                               (setf (car finished) t)))
                       ;; Let the other worker take the task, so that the
                       ;; owner sleeps at the end of the scope.
                       (wait-until (lambda () (car started)))))))))
          (flet ((owner-asleep-p ()
                   (and (car owner)
                        (member (car owner) (rt::pool-idle-waiters rt::**pool**))
                        t))
                 (abort-owner ()
                   (interrupt-thread (rt::worker-thread (car owner)) (lambda () (abort)))))
            (is (wait-until #'owner-asleep-p))
            (abort-owner)
            (sleep 0.05)
            (is (not (rt:future-done-p future)))
            ;; Give the owner time to go back to sleep, waiting for the
            ;; task while it unwinds.
            (sleep 0.05)
            (wait-until #'owner-asleep-p)
            (cond ((= aborts 1)
                   (setf (car release) t)
                   (is (eq :aborted (await-outcome future)))
                   ;; The task finished before the owner's task was abandoned.
                   (is (car finished)))
                  (t
                   (abort-owner)
                   (is (wait-until (lambda () (rt:future-done-p future))))
                   (is (not (car finished)))
                   (setf (car release) t)
                   (is (eq :aborted (await-outcome future)))
                   (is (wait-until (lambda () (car finished))))))))))))

(deftest threads-scope-spawn-from-outside-the-pool ()
  ;; A thread outside the pool may spawn into a scope, even when the
  ;; scope's owner is the only worker, waiting for that very task.
  (with-worker-count (1)
    (with-deadline (60)
      (let ((counter (list 0)))
        (rt:call-with-scope
         (lambda (scope)
           (backend:join-thread
            (backend:make-thread
             (lambda ()
               (rt:scope-spawn scope (lambda () (backend:atomic-incf (car counter)))))))))
        (is (= 1 (car counter)))))))

;;; Parallel loops

(deftest threads-parallel-for-visits-each-index-once ()
  (with-worker-count (4)
    (with-deadline (60)
      (dolist (grain '(0 1 7 1000))
        (dolist (range '((0 0) (0 1) (5 6) (0 2) (3 1000) (10 100000)))
          (destructuring-bind (start end) range
            (let ((visits (make-array (max end 1) :initial-element 0)))
              (rt:parallel-for start end grain
                               (lambda (i)
                                 (incf (svref visits i))
                                 (values)))
              (is (loop :for i :below (length visits)
                        :always (= (svref visits i) (if (<= start i (1- end)) 1 0)))))))))))

(deftest threads-parallel-for-chunks-partition-the-range ()
  (with-worker-count (4)
    (with-deadline (60)
      (dolist (grain '(0 1 10 100000))
        (dolist (range '((0 1) (7 8) (0 2) (3 1000) (0 100000)))
          (destructuring-bind (start end) range
            (let ((chunks (list nil)))
              (rt:parallel-for-chunks start end grain
                                      (lambda (lo hi)
                                        (atomic-push (cons lo hi) (car chunks))
                                        (values)))
              (let ((sorted (sort (car chunks) #'< :key #'car)))
                ;; Non-empty, contiguous, and covering [START, END).
                (is (every (lambda (chunk) (< (car chunk) (cdr chunk))) sorted))
                (is (= start (car (first sorted))))
                (is (= end (cdr (car (last sorted)))))
                (is (loop :for (a b) :on sorted
                          :while b
                          :always (= (cdr a) (car b))))
                (when (plusp grain)
                  (is (every (lambda (chunk) (<= (- (cdr chunk) (car chunk)) grain))
                             sorted))))))))
      (is (equal (loop :for i :below 1000 :collect i)
                 (rt:parallel-reduce-chunks 0 1000 0
                                            (lambda (lo hi)
                                              (loop :for i :from lo :below hi :collect i))
                                            #'append '()))))))

(deftest threads-parallel-reduce ()
  (with-worker-count (4)
    (with-deadline (60)
      (is (eq :empty (rt:parallel-reduce 5 5 0 #'identity #'+ :empty)))
      (is (= 499999500000 (rt:parallel-reduce 0 1000000 0 #'identity #'+ 0)))
      ;; The order of elements is preserved for non-commutative operations.
      (dolist (grain '(0 3))
        (is (equal (loop :for i :below 5000 :collect i)
                   (rt:parallel-reduce 0 5000 grain #'list #'append '()))))
      ;; A positive grain makes floating-point reductions reproducible.
      (let ((sums (loop :repeat 5
                        :collect (rt:parallel-reduce 0 100000 1000
                                                     (lambda (i) (/ 1d0 (1+ i)))
                                                     #'+ 0d0))))
        (is (every (lambda (sum) (= sum (first sums))) sums))))))

(deftest threads-parallel-sort ()
  (with-worker-count (4)
    (with-deadline (60)
      (flet ((check (n)
               (let ((vector (make-array n :adjustable t :fill-pointer n)))
                 (dotimes (i n)
                   ;; Many duplicate keys, tagged with their original position.
                   (setf (aref vector i) (cons (random 100) i)))
                 (rt:parallel-sort vector (lambda (a b) (< (car a) (car b))))
                 (is (loop :for i :from 1 :below n
                           :always (let ((a (aref vector (1- i)))
                                         (b (aref vector i)))
                                     (or (< (car a) (car b))
                                         (and (= (car a) (car b))
                                              (< (cdr a) (cdr b))))))))))
        (dolist (n '(0 1 2 15 16 17 2047 2048 2049 5000 100000))
          (check n)))
      ;; An error in the predicate leaves the vector unchanged.
      (let* ((original (cons 500 (loop :for i :below 10000 :collect (random 1000))))
             (vector (coerce original 'vector)))
        (is (eq :failed (handler-case (rt:parallel-sort vector
                                                        (lambda (a b)
                                                          (when (or (= a 500) (= b 500))
                                                            (error 'rt-test-error :tag :sort))
                                                          (< a b)))
                          (rt-test-error () :failed))))
        (is (equal original (coerce vector 'list)))))))

;;; The pool

(deftest threads-pool-lifecycle ()
  (with-worker-count (3)
    (is (= 3 (rt:worker-count)))
    (is (null (rt:current-worker-index)))
    (let ((indices (rt:call-in-pool
                    (lambda ()
                      (let ((seen (list nil)))
                        (rt:parallel-for 0 1000 1
                                         (lambda (i)
                                           (declare (ignore i))
                                           (atomic-push (rt:current-worker-index) (car seen))
                                           (values)))
                        (car seen))))))
      (is (every (lambda (i) (and (integerp i) (<= 0 i 2))) indices)))
    (rt:shutdown)
    (is (= 3 (rt:worker-count)))
    ;; Invalid counts are bugs, and change nothing.
    (is (eq :refused (handler-case (rt:set-worker-count 0)
                       (coalton/classes:panic () :refused))))
    (is (= 3 (rt:worker-count)))
    ;; The pool starts again on demand.
    (is (= 6765 (with-deadline (60) (rt-test-fib 20)))))
  (is (image-save-hook-p 'rt::shutdown-before-save)))

(deftest threads-pool-runs-tasks-in-parallel ()
  ;; Losing track of sleeping workers would not make results wrong, only
  ;; sequential. Check that tasks really run at the same time after the
  ;; workers have fallen asleep, and that the sleep bookkeeping agrees
  ;; with the lists of sleeping workers.
  (with-worker-count (4)
    (with-deadline (60)
      (dotimes (round 3)
        (sleep 0.05)
        (is (tasks-overlap-p)))
      (sleep 0.1)
      (is (workers-fall-asleep-consistently-p rt::**pool**)))))

(deftest threads-pool-sleeps-and-wakes ()
  ;; Alternate bursts of work with pauses long enough for the workers
  ;; to fall asleep, from inside and outside the pool.
  (with-worker-count (4)
    (with-deadline (120)
      (dotimes (round 10)
        (is (= 610 (rt-test-fib 15)))
        (sleep 0.005)
        (is (= 610 (rt:call-in-pool (lambda () (rt-test-fib 15)))))
        (sleep 0.005)))))
