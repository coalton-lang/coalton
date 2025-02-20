(coalton-library/utils:defstdlib-package #:coalton-library/threads
    (:use
     #:coalton
     #:coalton-library/system
     #:coalton-library/classes)
  (:local-nicknames
   (#:cell #:coalton-library/cell))
  (:export
   ;; Threads
   #:Thread
   #:spawn
   #:make-thread
   #:current-thread
   #:all-threads
   #:join
   #:interrupt
   #:destroy
   #:alive?
   ;; Locks
   #:Lock
   #:RecursiveLock
   #:with-lock-held
   #:with-recursive-lock-held
   #:make-lock
   #:make-recursive-lock
   #:acquire-lock
   #:acquire-lock-no-wait
   #:acquire-recursive-lock
   #:acquire-recursive-lock-no-wait
   #:release-lock
   #:release-recursive-lock
   ;; Condition Variables
   #:ConditionVariable
   #:make-cv
   #:await-cv
   #:notify-cv
   #:broadcast-cv
   ;; Semaphores
   #:Semaphore
   #:make-semaphore
   #:signal-semaphore
   #:await-semaphore
   ;; Atomics
   #:AtomicInteger
   #:make-atomic
   #:cas-atomic!
   #:decf-atomic!
   #:incf-atomic!
   #:atomic-value))

(in-package #:coalton-library/threads)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;;
;;; This module implements bindings to bordeaux-threads.
;;; It will only be loaded if thread capabilities are
;;; detected in `cl:*features*'.
;;;

;;---------;;
;; Threads ;;
;;---------;;

(cl:defmacro spawn (cl:&body body)
  `(make-thread (fn () ,@body)))

(coalton-toplevel
  (repr :native bt2:thread)
  (define-type (Thread :a)
    "A thread that can be joined to yield the result of the thunk used to create it.")

  (repr :native bt2:thread)
  (define-type LispThread
    "A plain lisp thread.")

  (define-instance (Eq LispThread)
    (define (== a b)
      (lisp Boolean (a b) (to-boolean (cl:eq a b)))))

  (define-instance (Eq (Thread :a))
    (define (== a b)
      (lisp Boolean (a b) (to-boolean (cl:eq a b)))))

  (define-instance (Into (Thread :a) LispThread)
    (define (into thread)
      (lisp LispThread (thread)
        thread)))

  (declare make-thread ((Unit -> :a) -> Thread :a))
  (define (make-thread thunk)
    "Creates and returns a thread, which will call the function
`thunk' with no arguments: when `thunk' returns, the thread terminates."
    (lisp (Thread :a) (thunk)
      (bt2:make-thread
       (cl:lambda () (call-coalton-function thunk Unit)))))

  (declare current-thread (Unit -> LispThread))
  (define (current-thread)
    "Returns the thread object representing the calling thread."
    (lisp LispThread ()
      (bt2:current-thread)))

  (declare all-threads (Unit -> (List LispThread)))
  (define (all-threads)
    "Returns a fresh list of all running threads."
    (lisp (List LispThread) ()
      (bt2:all-threads)))

  (declare join (Thread :a -> (Result LispCondition :a)))
  (define (join thread)
    "Wait until `thread' terminates, or if it has already terminated, return immediately."
    (lisp (Result LispCondition :a) (thread)
      (cl:handler-case (Ok (bt2:join-thread thread))
        (cl:error (c) (Err c)))))

  (declare interrupt ((Into :a LispThread) => :a -> (Unit -> Unit) -> (Result LispCondition :a)))
  (define (interrupt thread thunk)
    "Interrupt thread and call `thunk' within its dynamic context,
then continue with the interrupted path of execution."
    (lisp (Result LispCondition :a) (thread thunk)
      (cl:handler-case
          (Ok (bt2:interrupt-thread
               thread
               (cl:lambda () (call-coalton-function thunk Unit))))
        (cl:error (c) (Err c)))))

  (declare destroy ((Into :a LispThread) => :a -> (Result LispCondition :a)))
  (define (destroy thread)
    "Terminates the thread `thread'."
    (lisp (Result LispCondition :a) (thread)
      (cl:handler-case (Ok (bt2:destroy-thread thread))
        (cl:error (c) (Err c)))))

  (declare alive? ((Into :a LispThread) => :a -> Boolean))
  (define (alive? thread)
    "Returns True if `thread' has not finished or `destroy' has not been called on it."
    (lisp Boolean (thread)
      (bt2:thread-alive-p thread))))

;;-------;;
;; Locks ;;
;;-------;;

(coalton-toplevel
  (repr :native bt2:lock)
  (define-type Lock
    "Wrapper for a native non-recursive lock.")

  (repr :native bt2:recursive-lock)
  (define-type RecursiveLock
    "Wrapper for a native recursive lock.")

  (declare make-lock (Unit -> Lock))
  (define (make-lock)
    "Creates a non-recursive lock."
    (lisp Lock ()
      (bt2:make-lock)))

  (declare acquire-lock (Lock -> Boolean))
  (define (acquire-lock lock)
    "Acquire `lock' for the calling thread."
    (lisp Boolean (lock)
      (bt2:acquire-lock lock)))

  (declare acquire-lock-no-wait (Lock -> Boolean))
  (define (acquire-lock-no-wait lock)
    "Acquire `lock' for the calling thread.
Returns Boolean immediately, True if `lock' was acquired, False otherwise."
    (lisp Boolean (lock)
      (bt2:acquire-lock lock :wait nil)))

  (declare release-lock (Lock -> (Result LispCondition Lock)))
  (define (release-lock lock)
    "Release `lock'. It is an error to call this unless
the lock has previously been acquired (and not released) by the same
thread. If other threads are waiting for the lock, the
`acquire-lock' call in one of them will now be able to continue.

Returns the lock."
    (lisp (Result LispCondition Lock) (lock)
      (cl:handler-case (Ok (bt2:release-lock lock))
        (cl:error (c) (Err c)))))

  (declare with-lock-held (Lock -> (Unit -> :a) -> :a))
  (define (with-lock-held lock thunk)
    (acquire-lock lock)
    (let ((result (thunk)))
      (release-lock lock)
      result))

  (declare make-recursive-lock (Unit -> RecursiveLock))
  (define (make-recursive-lock)
    "Creates a recursive lock."
    (lisp RecursiveLock ()
      (bt2:make-recursive-lock)))

  (declare acquire-recursive-lock (RecursiveLock -> Boolean))
  (define (acquire-recursive-lock lock)
    "Acquire `lock' for the calling thread."
    (lisp Boolean (lock)
      (bt2:acquire-recursive-lock lock)))

  (declare acquire-recursive-lock-no-wait (Lock -> Boolean))
  (define (acquire-recursive-lock-no-wait lock)
    "Acquire `lock' for the calling thread.
Returns Boolean immediately, True if `lock' was acquired, False otherwise."
    (lisp Boolean (lock)
      (bt2:acquire-recursive-lock lock :wait nil)))

  (declare release-recursive-lock (RecursiveLock -> (Result LispCondition RecursiveLock)))
  (define (release-recursive-lock lock)
    "Release `lock'. It is an error to call this unless
the lock has previously been acquired (and not released) by the same
thread. If other threads are waiting for the lock, the
`acquire-lock' call in one of them will now be able to continue.

Returns the lock."
    (lisp (Result LispCondition RecursiveLock) (lock)
      (cl:handler-case (Ok (bt2:release-recursive-lock lock))
        (cl:error (c) (Err c)))))

  (declare with-recursive-lock-held (RecursiveLock -> (Unit -> :a) -> :a))
  (define (with-recursive-lock-held lock thunk)
    (acquire-recursive-lock lock)
    (let ((result (thunk)))
      (release-recursive-lock lock)
      result)))

;;---------------------;;
;; Condition Variables ;;
;;---------------------;;

(coalton-toplevel
  (repr :native bt2:condition-variable)
  (define-type ConditionVariable
    "Wrapper for a native condition variable.")

  (declare make-cv (Unit -> ConditionVariable))
  (define (make-cv)
    "Creates a condition variable."
    (lisp ConditionVariable ()
      (bt2:make-condition-variable)))

  (declare await-cv (ConditionVariable -> Lock -> Unit))
  (define (await-cv cv lock)
    "Atomically release `lock' and enqueue the calling thread waiting for `cv'.
The thread will resume when another thread has notified it using `notify-cv';
it may also resume if interrupted by some external event or in other
implementation-dependent circumstances: the caller must always test on waking
that there is threading to be done, instead of assuming that it can go ahead."
    (lisp Unit (cv lock)
      (bt2:condition-wait cv lock)
      Unit))

  (declare notify-cv (ConditionVariable -> Unit))
  (define (notify-cv cv)
    "Notify one of the threads waiting for `cv'."
    (lisp Unit (cv)
      (bt2:condition-notify cv)
      Unit))

  (declare broadcast-cv (ConditionVariable -> Unit))
  (define (broadcast-cv cv)
    "Notify all of the threads waiting for `cv'."
    (lisp Unit (cv)
      (bt2:condition-broadcast cv)
      Unit)))

;;------------;;
;; Semaphores ;;
;;------------;;

(coalton-toplevel
  (repr :native bt2:semaphore)
  (define-type Semaphore
    "Wrapper for a native semaphore.")

  (declare make-semaphore (Unit -> Semaphore))
  (define (make-semaphore)
    "Creates a semaphore with initial count 0."
    (lisp Semaphore ()
      (bt2:make-semaphore)))

  (declare signal-semaphore (Semaphore -> UFix -> Unit))
  (define (signal-semaphore sem count)
    "Increment `sem' by `count'.
If there are threads awaiting this semaphore, then `count' of them are woken up."
    (lisp Unit (sem count)
      (bt2:signal-semaphore sem :count count)
      Unit))

  (declare await-semaphore (Semaphore -> Unit))
  (define (await-semaphore sem)
    "Decrement the count of `sem' by 1 if the count is larger than zero.
If the count is zero, blocks until `sem' can be decremented."
    (lisp Unit (sem)
      (bt2:wait-on-semaphore sem)
      Unit)))

;;---------;;
;; Atomics ;;
;;---------;;

(coalton-toplevel
  (repr :native bt2:atomic-integer)
  (define-type AtomicInteger
    "An unsigned machine word that allows atomic increment, decrement and swap.")

  (declare make-atomic (#+32-bit U32 #+64-bit U64 -> AtomicInteger))
  (define (make-atomic value)
    "Creates an `AtomicInteger' with initial value `value'."
    (lisp AtomicInteger (value)
      (bt2:make-atomic-integer :value value)))

  (declare cas-atomic! (AtomicInteger -> #+32-bit U32 #+64-bit U64 -> #+32-bit U32 #+64-bit U64 -> Boolean))
  (define (cas-atomic! atomic old new)
    "If the current value of `atomic' is equal to `old', replace it with `new'.
Returns True if the replacement was successful, otherwise False."
    (lisp Boolean (atomic old new)
      (bt2:atomic-integer-compare-and-swap atomic old new)))

  (declare decf-atomic! (AtomicInteger -> #+32-bit U32 #+64-bit U64 -> #+32-bit U32 #+64-bit U64))
  (define (decf-atomic! atomic delta)
    "Decrements the value of `atomic' by `delta'."
    (lisp #+32-bit U32 #+64-bit U64 (atomic delta)
      (bt2:atomic-integer-decf atomic delta)))

  (declare incf-atomic! (AtomicInteger -> #+32-bit U32 #+64-bit U64 -> #+32-bit U32 #+64-bit U64))
  (define (incf-atomic! atomic delta)
    "Increments the value of `atomic' by `delta'."
    (lisp #+32-bit U32 #+64-bit U64 (atomic delta)
      (bt2:atomic-integer-incf atomic delta)))

  (declare atomic-value (AtomicInteger -> #+32-bit U32 #+64-bit U64))
  (define (atomic-value atomic)
    "Returns the current value of `atomic'."
    (lisp #+32-bit U32 #+64-bit U64 (atomic)
      (bt2:atomic-integer-value atomic))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/THREADS")
