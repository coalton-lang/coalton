(in-package #:coalton-user)

(coalton-toplevel

  ;;
  ;; Show
  ;;

  (define-class (Show :a)
    (show (:a -> String)))


  ;;
  ;; Eq
  ;;
  
  (define-class (Eq :a)
    (== (:a -> :a -> Boolean))
    (/= (:a -> :a -> Boolean)))

  
  ;;
  ;; Ord
  ;;
  
  (define-type Ord
    LT
    EQ
    GT)

  (define-class ((Eq :a) => (Ord :a))
    (<=> (:a -> :a -> Ord)))

  (declare > (Ord :a => (:a -> :a -> Boolean)))
  (define (> x y)
    (match (<=> x y)
      ((GT) True)
      (_ False)))
  
  (declare < (Ord :a => (:a -> :a -> Boolean)))
  (define (< x y)
    (match (<=> x y)
      ((LT) True)
      (_ False)))
  
  (declare >= (Ord :a => (:a -> :a -> Boolean)))
  (define (>= x y)
    (match (<=> x y)
      ((LT) False)
      (_ True)))
  
  (declare <= (Ord :a => (:a -> :a -> Boolean)))
  (define (<= x y)
    (match (<=> x y)
      ((GT) False)
      (_ True)))

  (declare max (Ord :a => (:a -> :a -> :a)))
  (define (max a b)
    (if (> a b)
	a
	b))

  (declare min (Ord :a => (:a -> :a -> :a)))
  (define (min a b)
    (if (< a b)
	a
	b))

  ;;
  ;; Num
  ;;
  
  (define-class ((Eq :a) (Show :a) => (Num :a))
    (+ (:a -> :a -> :a))
    (- (:a -> :a -> :a))
    (* (:a -> :a -> :a))
    (fromInt (Int -> :a)))

  ;;
  ;; Haskell
  ;;

  (define-class (Semigroup :a)
    (<> (:a -> :a -> :a)))

  (define-class (Semigroup :a => (Monoid :a))
    (mempty (:a)))

  (define-class (Functor :f)
    (map ((:a -> :b) -> (:f :a) -> (:f :b))))

  (define-class (Functor :f => (Applicative :f))
    (pure (:a -> (:f :a)))
    (liftA2 ((:a -> :b -> :c) -> (:f :a) -> (:f :b) -> (:f :c))))

  (define-class (Applicative :m => (Monad :m))
    (>>= ((:m :a) -> (:a -> (:m :b)) -> (:m :b)))
    (>> ((:m :a) -> (:m :b) -> (:m :b))))

  (define-class (Monad :m => (MonadFail :m))
    (fail (String -> (:m :a))))

  (define-class (Applicative :f => (Alternative :f))
    (alt ((:f :a) -> (:f :a) -> (:f :a)))
    (empty (:f :a)))

  ;;
  ;; Conversions
  ;;

  (define-class (Into :a :b)
    (into (:a -> :b)))


  ;; Opting into this marker typeclass imples that the instances for
  ;; (Into :a :b) and (Into :b :a) form an isomorphism
  (define-class ((Into :a :b) (Into :b :a) => (Iso :a :b)))

  (define-instance (Into :a :a)
    (define (into x) x))

  (define-class (TryInto :a :b :c)
    (tryInto (:a -> (Result :b :c))))

  (define-instance (Iso :a :a))

  (define-class (WithDefault :f)
    (withDefault (:a -> (:f :a) -> (:a)))))


