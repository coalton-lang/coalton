(in-package #:coalton-library)

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
    (fromInt (Integer -> :a)))

  ;;
  ;; Dividable
  ;;

  (define-class ((Num :arg-type)
                 (Num :res-type)
                 => (Dividable :arg-type :res-type))
    "The representation of a type such that division within that type possibly results in another type. For instance,


    (Dividable Integer Fraction)


establishes that division of two `Integer`s can result in a `Fraction`, whereas


    (Dividable Single-Float Single-Float)


establishes that division of two `Single-Float`s can result in a `Single-Float`.

Note that `Dividable` does *not* establish a default result type; you must constrain the result type yourself.

See also: `/`
"
    ;; This is a type that is more pragmatic and less mathematical in
    ;; nature. It expresses a division relationship between one input
    ;; type and one output type.
    ;;
    ;; UNSAFE-/ does the division unsafely. This may mean that an
    ;; error is signaled or that undefined results may occur.
    (unsafe-/ (:arg-type -> :arg-type -> :res-type)))

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

  (define-class (Into :from :to)
    "INTO imples *every* element of :FROM can be represented by an element of :TO. This conversion might not be injective (i.e., there may be elements in :TO that don't correspond to any in :FROM)."
    (into (:from -> :to)))


  ;; Opting into this marker typeclass imples that the instances for
  ;; (Into :a :b) and (Into :b :a) form a bijection.
  (define-class ((Into :a :b) (Into :b :a) => (Iso :a :b)))

  (define-instance (Into :a :a)
    (define (into x) x))

  (define-class (TryInto :from :to)
    "TRY-INTO implies *most* elements of :FROM can be represented exactly by an element of :TO, but sometimes not. If not, an error string is returned."
    ;; Ideally we'd have an associated-type here instead of locking in
    ;; on String.
    (tryInto (:from -> (Result String :to))))

  (define-instance (Iso :a :a))

  (define-class (WithDefault :f)
    (withDefault (:a -> (:f :a) -> (:a))))
  )
