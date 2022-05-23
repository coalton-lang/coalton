(coalton-library/utils:defstdlib-package #:coalton-library/optional
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:local-nicknames
   (#:classes #:coalton-library/classes))
  (:export
   #:fromSome
   #:isSome
   #:isNone))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(in-package #:coalton-library/optional)

(coalton-toplevel
  ;;
  ;; Optional
  ;;

  (declare fromSome (String -> (Optional :a) -> :a))
  (define (fromSome str opt)
    "Get the value of OPT, erroring with the provided string if it is None."
    (match opt
      ((Some x) x)
      ((None) (lisp :a (str) (cl:error str)))))

  (declare isSome ((Optional :a) -> Boolean))
  (define (isSome x)
    "Is X Some?"
    (lisp Boolean (x)
      (cl:etypecase x
        (classes::Optional/Some True)
        (classes::Optional/None False))))

  (declare isNone ((Optional :a) -> Boolean))
  (define (isNone x)
    "Is X None?"
    (lisp Boolean (x)
      (cl:etypecase x
        (classes::Optional/None True)
        (classes::Optional/Some False))))

  ;;
  ;; Optional instances
  ;;

  (define-instance (Eq :a => (Eq (Optional :a)))
    (define (== x y)
      (match (Tuple x y)
        ((Tuple (Some x) (Some y)) (== x y))
        ((Tuple (None) (None)) True)
        (_ False))))

  (define-instance (Ord :a => (Ord (Optional :a)))
    (define (<=> x y)
      (match x
        ((Some a)
         (match y
           ((Some b) (<=> a b))
           ((None) GT)))
        ((None)
         (match y
           ((Some _) LT)
           ((None) EQ))))))

  (define-instance (Num :a => Num (Optional :a))
    (define (+ a b) (liftA2 + a b))
    (define (- a b) (liftA2 - a b))
    (define (* a b) (liftA2 * a b))
    (define (fromInt x) (pure (fromInt x))))

  (define-instance (Semigroup :a => (Semigroup (Optional :a)))
    (define (<> a b)
      (match (Tuple a b)
        ((Tuple (Some a) (Some b)) (Some (<> a b)))
        (_ None))))

  (define-instance (Monoid :a => (Monoid (Optional :a)))
    (define mempty (Some mempty)))

  (define-instance (Functor Optional)
    (define (map f x)
      (match x
        ((Some a) (Some (f a)))
        ((None) None))))

  (define-instance (Applicative Optional)
    (define (pure x)
      (Some x))
    (define (liftA2 f x y)
      (match x
        ((None) None)
        ((Some x)
         (match y
           ((None) None)
           ((Some y)
            (Some (f x y))))))))

  (define-instance (Monad Optional)
    (define (>>= x f)
      (match x
        ((Some x) (f x))
        ((None) None))))

  (define-instance (MonadFail Optional)
    (define (fail str)
      None))

  (define-instance (Alternative Optional)
    (define empty None)
    (define (alt x y)
      (match x
        ((Some _) x)
        (_ y))))

  (define-instance (Unwrappable Optional)
    (define (unwrap-or-else fail opt)
      (match opt
        ((Some elt) elt)
        ((None) (fail))))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/OPTIONAL")
