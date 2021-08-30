(in-package #:coalton-user)

(coalton-toplevel
  ;;
  ;; Optional
  ;;

  ;; Definition is in types.lisp

  (declare fromSome (String -> (Optional :a) -> :a))
  (define (fromSome str opt)
    (match opt
      ((Some x) x)
      ((None) (lisp :a (cl:error str)))))

  (declare isSome ((Optional :a) -> Boolean))
  (define (isSome x)
    (lisp Boolean
      (cl:etypecase x
        (Optional/Some True)
	(Optional/None False))))

  (declare isNone ((Optional :a) -> Boolean))
  (define (isNone x)
    (lisp Boolean
      (cl:etypecase x
	(Optional/None True)
	(Optional/Some False))))
  
  ;;
  ;; Optional instances
  ;;

  (define-instance (Show :a => (Show (Optional :a)))
    (define (show x)
      (match x
        ((Some a) (concat-string "Some " (show a)))
        ((None) "None"))))

  (define-instance (Eq :a => (Eq (Optional :a)))
    (define (== x y)
      (match (Tuple x y)
        ((Tuple (Some x) (Some y)) (== x y))
	((Tuple (None) (None)) True)
	(_ False)))
    (define (/= x y)
      (not (== x y))))

  (define-instance (Ord :a => (Ord (Optional :a)))
    (define (<=> x y)
      (match x
        ((Some a)
         (match y
           ((Some b) (<=> a b))
           ((None) GT)))
        ((None)
         (match y
           ((Some b) LT)
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
        ((None) None)))
    (define (>> m k)
      (>>= m
           (fn (_)
             k))))

  (define-instance (MonadFail Optional)
    (define (fail str)
      None))

  (define-instance (Alternative Optional)
    (define empty None)
    (define (alt x y)
      (match x
        ((Some _) x)
        (_ y)))))
