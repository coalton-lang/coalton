(in-package #:coalton-library)

(coalton-toplevel
  ;;
  ;; Optional
  ;;

  ;; Definition is in types.lisp

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
        (Optional/Some True)
        (Optional/None False))))

  (declare isNone ((Optional :a) -> Boolean))
  (define (isNone x)
    "Is X None?"
    (lisp Boolean (x)
      (cl:etypecase x
        (Optional/None True)
        (Optional/Some False))))

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
    (define (withDefault default opt)
      (match opt
        ((Some x) x)
        ((None) default)))
    (define (unwrap opt)
      (match opt
        ((Some x) x)
        ((None) (error "unexpected None in unwrap"))))))
