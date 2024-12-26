(coalton-library/utils:defstdlib-package #:coalton-library/threads
  (:use
   #:coalton
   #:coalton-library/system
   #:coalton-library/classes)
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
   #:atomic-cmp-and-swap
   #:decf-atomic
   #:incf-atomic
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
  `(make-thread (fn () ,@body Unit)))

(coalton-toplevel
  (repr :native bt2:thread)
  (define-type Thread)

  (define-instance (Eq Thread)
    (define (== a b)
      (lisp Boolean (a b) (to-boolean (cl:equal a b)))))

  (declare make-thread ((Unit -> Unit) -> Thread))
  (define (make-thread thunk)
    "Creates and returns a thread, which will call the function
`thunk' with no arguments: when `thunk' returns, the thread terminates."
    (lisp Thread (thunk)
      (bt2:make-thread
       (cl:lambda () (call-coalton-function thunk Unit)))))

  (declare current-thread (Unit -> Thread))
  (define (current-thread)
    "Returns the thread object representing the calling thread."
    (lisp Thread ()
      (bt2:current-thread)))

  (declare all-threads (Unit -> (List Thread)))
  (define (all-threads)
    "Returns a fresh list of all running threads."
    (lisp (List Thread) ()
      (bt2:all-threads)))

  (declare join (Thread -> Unit))
  (define (join thread)
    "Wait until `thread' terminates, or if it has already terminated, return immediately."
    (lisp Unit (thread)
      (bt2:join-thread thread)
      Unit))

  (declare interrupt (Thread -> (Unit -> Unit) -> Thread))
  (define (interrupt thread thunk)
    "Interrupt thread and call `thunk' within its dynamic context,
then continue with the interrupted path of execution."
    (lisp Thread (thread thunk)
      (bt2:interrupt-thread
       thread
       (cl:lambda () (call-coalton-function thunk Unit)))))

  (declare destroy (Thread -> (Result LispCondition Thread)))
  (define (destroy thread)
    "Terminates the thread `thread'."
    (lisp (Result LispCondition Thread) (thread)
      (cl:handler-case (Ok (bt2:destroy-thread thread))
        (cl:error (c) (Err c)))))

  (declare alive? (Thread -> Boolean))
  (define (alive? thread)
    "Returns True if `thread' has not finished or `destroy' has not been called on it."
    (lisp Boolean (thread)
      (bt2:thread-alive-p thread))))

;;-------;;
;; Locks ;;
;;-------;;

;; TODO these should have some kind of unwind protection

(cl:defmacro with-lock-held ((lock) cl:&body body)
  `(progn
     (acquire-lock ,lock)
     ,@body
     (release-lock ,lock)))

(cl:defmacro with-recursive-lock-held ((lock) cl:&body body)
  `(progn
     (acquire-recursive-lock ,lock)
     ,@body
     (release-recursive-lock ,lock)))

(coalton-toplevel
  (repr :native bt2:lock)
  (define-type Lock)

  (repr :native bt2:recursive-lock)
  (define-type RecursiveLock)

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
        (cl:error (c) (Err c))))))

;;---------------------;;
;; Condition Variables ;;
;;---------------------;;

(coalton-toplevel
  (repr :native bt2:condition-variable)
  (define-type ConditionVariable)

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
  (define-type Semaphore)

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
  (define-type AtomicInteger)

  (declare make-atomic (#+32-bit U32 #+64-bit U64 -> AtomicInteger))
  (define (make-atomic value)
    "Creates an `AtomicInteger' with initial value `value'."
    (lisp AtomicInteger (value)
      (bt2:make-atomic-integer :value value)))

  (declare atomic-cmp-and-swap (AtomicInteger -> #+32-bit U32 #+64-bit U64 -> #+32-bit U32 #+64-bit U64 -> Boolean))
  (define (atomic-cmp-and-swap atomic old new)
    "If the current value of `atomic' is equal to `old', replace it with `new'.
Returns True if the replacement was successful, otherwise False."
    (lisp Boolean (atomic old new)
      (bt2:atomic-integer-compare-and-swap atomic old new)))

  (declare decf-atomic (AtomicInteger -> #+32-bit U32 #+64-bit U64 -> #+32-bit U32 #+64-bit U64))
  (define (decf-atomic atomic delta)
    "Decrements the value of `atomic' by `delta'."
    (lisp #+32-bit U32 #+64-bit U64 (atomic delta)
      (bt2:atomic-integer-decf atomic delta)))

  (declare incf-atomic (AtomicInteger -> #+32-bit U32 #+64-bit U64 -> #+32-bit U32 #+64-bit U64))
  (define (incf-atomic atomic delta)
    "Increments the value of `atomic' by `delta'."
    (lisp #+32-bit U32 #+64-bit U64 (atomic delta)
      (bt2:atomic-integer-incf atomic delta)))

  (declare atomic-value (AtomicInteger -> #+32-bit U32 #+64-bit U64))
  (define (atomic-value atomic)
    "Returns the current value of `atomic'."
    (lisp #+32-bit U32 #+64-bit U64 (atomic)
      (bt2:atomic-integer-value atomic))))
