(coalton-library/utils:defstdlib-package #:coalton-library/result
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/optional)
  (:local-nicknames
   (#:classes #:coalton-library/classes))
  (:export
   #:ok?
   #:err?
   #:map-err
   #:flatten))

(in-package #:coalton-library/result)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;;
  ;; Result
  ;;

  (declare ok? (Result :a :b -> Boolean))
  (define (ok? x)
    "Returns TRUE if X is ERR"
    (lisp Boolean (x)
      (cl:etypecase x
        (classes::Result/Ok True)
        (classes::Result/Err False))))

  (declare err? (Result :a :b -> Boolean))
  (define (err? x)
    "Returns TRUE if X is ERR"
    (lisp Boolean (x)
      (cl:etypecase x
        (classes::Result/Err True)
        (classes::Result/Ok False))))

  (declare map-err ((:a -> :b) -> Result :a :c -> Result :b :c))
  (define (map-err f x)
    "Map over the ERR case"
    (match x
      ((Err x) (Err (f x)))
      ((Ok x) (Ok x))))

  (declare flatten ((Result :a :a) -> :a))
  (define (flatten x)
    (match x
      ((Ok x) x)
      ((Err x) x)))

  ;;
  ;; Result instances
  ;;

  (define-instance ((Eq :a) (Eq :b) => Eq (Result :a :b))
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Ok a) (Ok b)) (== a b))
        ((Tuple (Err a) (Err b)) (== a b))
        (_ False))))

  (define-instance ((Ord :a) (Ord :b) => Ord (Result :a :b))
    (define (<=> a b)
      (match (Tuple a b)
        ((Tuple (Ok a) (Ok b)) (<=> a b))
        ((Tuple (Err a) (Err b)) (<=> a b))
        ((Tuple (Err _) (Ok _)) LT)
        ((Tuple (Ok _) (Err _)) GT))))

  (define-instance (Semigroup :b => Semigroup (Result :a :b))
    (define (<> a b)
      (match (Tuple a b)
        ((Tuple (Ok x) (Ok y))
         (Ok (<> x y)))
        ((Tuple (Err _) _) a)
        (_ b))))

  (define-instance (Monoid :b => Monoid (Result :a :b))
    (define mempty (Ok mempty)))

  (define-instance (Functor (Result :a))
    (define (map f x)
      (match x
        ((Ok x) (Ok (f x)))
        ((Err e) (Err e)))))

  (define-instance (Applicative (Result :a))
    (define (pure x) (Ok x))
    (define (liftA2 f a b)
      (match (Tuple a b)
        ((Tuple (Ok a) (Ok b))
         (Ok (f a b)))
        ((Tuple (Err e) _) (Err e))
        ((Tuple _ (Err e)) (Err e)))))

  (define-instance (Monad (Result :a))
    (define (>>= m f)
      (match m
        ((Ok x) (f x))
        ((Err e) (Err e)))))

  (define-instance (Bifunctor Result)
    (define (bimap f g res)
      (match res
        ((Ok x) (Ok (g x)))
        ((Err e) (Err (f e))))))

  (define-instance (Into (Result :a :b) (Optional :b))
    (define (into res)
      (match res
        ((Ok x) (Some x))
        ((Err _) None))))

  (define-instance (Into (Optional :b) (Result Unit :b))
    (define (into opt)
      (match opt
        ((Some x) (Ok x))
        ((None) (Err Unit)))))

  (define-instance (Iso (Result Unit :a) (Optional :a)))

  (define-instance (Unwrappable (Result :a))
    (define (unwrap-or-else succeed fail res)
      (match res
        ((Ok elt) (succeed elt))
        ((Err _) (fail))))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/RESULT")
