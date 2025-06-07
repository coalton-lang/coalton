(coalton-library/utils:defstdlib-package #:coalton-library/monad/resultt
  (:use
   #:coalton
   #:coalton-library/functions
   #:coalton-library/classes
   #:coalton-library/result)
  (:export
   #:ResultT
   #:run-resultT
   #:map-resultT
   #:map-errT
   #:err-ifT
   #:do-resultT))

(in-package #:coalton-library/monad/resultt)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;;
;;;  ResultT
;;;

(coalton-toplevel
  (define-type (ResultT :err :m :ok)
    "A monadic computation that returns a Result."
    (ResultT (:m (Result :err :ok))))

  (inline)
  (declare run-resultT (ResultT :err :m :ok -> :m (Result :err :ok)))
  (define (run-resultT (ResultT m))
    m)

  (inline)
  (declare map-resultT ((:m (Result :e1 :a) -> :n (Result :e2 :b))
                        -> ResultT :e1 :m :a
                        -> ResultT :e2 :n :b))
  (define (map-resultT f (ResultT m))
    (ResultT (f m)))

  (inline)
  (declare map-errT (Functor :m => (:a -> :b) -> ResultT :a :m :c -> ResultT :b :m :c))
  (define (map-errT ferr) (map-resultT (map (map-err ferr))))

  (inline)
  (declare err-ifT (Monad :m => Boolean -> :err -> ResultT :err :m Unit))
  (define (err-ifT passed failure)
    (ResultT (pure (err-if passed failure)))))

;;;
;;; Instances
;;;

(coalton-toplevel
  (define-instance (Functor :m => Functor (ResultT :err :m))
    (inline)
    (define (map fa->b (ResultT m))
      (ResultT (map (map fa->b) m))))

  (define-instance (Monad :m => Applicative (ResultT :err :m))
    (inline)
    (define (pure a)
      (ResultT (pure (Ok a))))
    (define (liftA2 fa->b->c (ResultT ma) (ResultT mb))
      ;; NOTE: This could be written more cleanly in pure do notation. This method
      ;; short circuits immediately if `ma` contains a `None`, which avoids doing
      ;; some function calls and allocations.
      (ResultT
       (do
        (resa <- ma)
        (match resa
          ((Err er) (pure (Err er)))
          ((Ok a)
           (do
            (resb <- mb)
            (match resb
              ((Err er) (pure (Err er)))
              ((Ok b)
               (pure (Ok (fa->b->c a b))))))))))))

  (define-instance (Monad :m => Monad (ResultT :err :m))
    (inline)
    (define (>>= (ResultT ma) fa->resmb)
      (ResultT
       (do
        (resa <- ma)
        (match resa
          ((Err er) (pure (Err er)))
          ((Ok a)
           (run-resultT (fa->resmb a))))))))

  (define-instance (MonadTransformer (ResultT :err))
    (inline)
    (define (lift m)
      (ResultT (map Ok m)))))

;;
;; Macros
;;

(cl:defmacro do-resultT (cl:&body body)
  "Perform a series of ResultT's, returning the value of the last one
if all of them succeed. Each step of the `do-resultT` block must return
a monad that contains a Result value.

Example:

(coalton
 (run
  (do-resultT
    reset ;; state = 0
    (a <- (add-and-return 1)) ;; a = 1, state = 1
    (add-and-return -3) ;; state = -2
    (let threshold = (* a 2))
    (err-if-less threshold) ;; this will Err and cancel, with state = -2, because state < 1 * 2
    (pure (Ok a))) ;; attempts to return (Ok 1) (will fail)
  0))
===>
(coalton
 (run
  (run-resultT
   (do
     (resultT reset)
     (a <- (resultT (add-and-return 1)))
     (resultT (add-and-return -3))
     (let threshold = (* a 2))
     (resultT (err-if-less threshold))
     (resultT (pure (ok a)))))
  0))

with these function definitions:

(coalton-toplevel
  (declare reset (ST Integer (Result String Unit)))
  (define reset
    (do
     (put 0)
     (pure (Ok Unit))))

  (declare add-and-return (Integer -> ST Integer (Result String Integer)))
  (define (add-and-return x)
    (do
     (current <- get)
     (put (+ x current))
     (pure (Ok (+ x current)))))

  (declare err-if-less (Integer -> St Integer (Result String Unit)))
  (define (err-if-less minimum)
    (do
     (current <- get)
     (pure (if (< current minimum)
               (Err (<> \"Current state must be less than \" (into minimum)))
               (Ok Unit))))))
"
  `(run-resultT
    (do
     ,@(cl:mapcar
        (cl:lambda (form)
          (cl:cond
            ;; (doresultT my-op) => (do (ResultT my-op))
            ((cl:symbolp form)
             `(ResultT ,form))
            ;; Don't modify: (let x = (foo ...))
            ((cl:equalp (cl:first form) 'let)
             form)
            ;; (x <- operation) =>   (x <- (ResultT operation))
            ;; (x <- (operation)) => (x <- (ResultT (operation)))
            ((cl:equalp (cl:second form) '<-)
             `(,(cl:first form) ,(cl:second form) (ResultT ,(cl:third form))))
            ;; (operation) => (ResultT operation)
            (cl:t
             `(ResultT ,form))))
        body))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MONAD/RESULTT")
