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
   #:wait-cv
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
  (define (make-thread f)
    (lisp Thread (f)
      (bt2:make-thread
       (cl:lambda () (call-coalton-function f Unit)))))

  (declare current-thread (Unit -> Thread))
  (define (current-thread)
    (lisp Thread ()
      (bt2:current-thread)))

  (declare all-threads (Unit -> (List Thread)))
  (define (all-threads)
    (lisp (List Thread) ()
      (bt2:all-threads)))

  (declare join (Thread -> Unit))
  (define (join thread)
    (lisp Unit (thread)
      (bt2:join-thread thread)
      Unit))

  (declare interrupt (Thread -> (Unit -> Unit) -> Unit))
  (define (interrupt thread f)
    (lisp Unit (thread f)
      (bt2:interrupt-thread
       thread
       (cl:lambda () (call-coalton-function f Unit)))))

  (declare destroy (Thread -> (Result LispCondition Thread)))
  (define (destroy thread)
    (lisp (Result LispCondition Thread) (thread)
      (cl:handler-case (Ok (bt2:join-thread thread))
        (cl:error (c) (Err c)))))

  (declare alive? (Thread -> Boolean))
  (define (alive? thread)
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
    (lisp Lock ()
      (bt2:make-lock)))

  (declare acquire-lock (Lock -> Boolean))
  (define (acquire-lock lock)
    (lisp Boolean (lock)
      (bt2:acquire-lock lock)))

  (declare acquire-lock-no-wait (Lock -> Boolean))
  (define (acquire-lock-no-wait lock)
    (lisp Boolean (lock)
      (bt2:acquire-lock lock :wait nil)))

  (declare release-lock (Lock -> Lock))
  (define (release-lock lock)
    (lisp Lock (lock)
      (bt2:release-lock lock)))

  (declare make-recursive-lock (Unit -> RecursiveLock))
  (define (make-recursive-lock)
    (lisp RecursiveLock ()
      (bt2:make-recursive-lock)))

  (declare acquire-recursive-lock (RecursiveLock -> Boolean))
  (define (acquire-recursive-lock lock)
    (lisp Boolean (lock)
      (bt2:acquire-recursive-lock lock)))

  (declare acquire-recursive-lock-no-wait (Lock -> Boolean))
  (define (acquire-recursive-lock-no-wait lock)
    (lisp Boolean (lock)
      (bt2:acquire-recursive-lock lock :wait nil)))

  (declare release-recursive-lock (RecursiveLock -> RecursiveLock))
  (define (release-recursive-lock lock)
    (lisp RecursiveLock (lock)
      (bt2:release-recursive-lock lock))))

;;---------------------;;
;; Condition Variables ;;
;;---------------------;;

(coalton-toplevel
  (repr :native bt2:condition-variable)
  (define-type ConditionVariable)

  (declare make-cv (Unit -> ConditionVariable))
  (define (make-cv)
    (lisp ConditionVariable ()
      (bt2:make-condition-variable)))

  (declare wait-cv (ConditionVariable -> Lock -> Unit))
  (define (wait-cv cv lock)
    (lisp Unit (cv lock)
      (bt2:condition-wait cv lock)
      Unit))

  (declare notify-cv (ConditionVariable -> Unit))
  (define (notify-cv cv)
    (lisp Unit (cv)
      (bt2:condition-notify cv)
      Unit))

  (declare broadcast-cv (ConditionVariable -> Unit))
  (define (broadcast-cv cv)
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
    (lisp Semaphore ()
      (bt2:make-semaphore)))

  (declare signal-semaphore (Semaphore -> UFix -> Unit))
  (define (signal-semaphore sem count)
    (lisp Unit (sem count)
      (bt2:signal-semaphore sem :count count)
      Unit))

  (declare await-semaphore (Semaphore -> Unit))
  (define (await-semaphore sem)
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
    (lisp AtomicInteger (value)
      (bt2:make-atomic-integer :value value)))

  (declare atomic-cmp-and-swap (AtomicInteger -> #+32-bit U32 #+64-bit U64 -> #+32-bit U32 #+64-bit U64 -> Boolean))
  (define (atomic-cmp-and-swap atomic old new)
    (lisp Boolean (atomic old new)
      (bt2:atomic-integer-compare-and-swap atomic old new)))

  (declare decf-atomic (AtomicInteger -> Unit))
  (define (decf-atomic atomic)
    (lisp Unit (atomic)
      (bt2:atomic-integer-decf atomic)
      Unit))

  (declare incf-atomic (AtomicInteger -> Unit))
  (define (incf-atomic atomic)
    (lisp Unit (atomic)
      (bt2:atomic-integer-incf atomic)
      Unit))

  (declare atomic-value (AtomicInteger -> #+32-bit U32 #+64-bit U64))
  (define (atomic-value atomic)
    (lisp #+32-bit U32 #+64-bit U64 (atomic)
      (bt2:atomic-integer-value atomic))))
