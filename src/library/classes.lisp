(in-package #:coalton-library)

(coalton-toplevel

  ;;
  ;; Eq
  ;;

  (define-class (Eq :a)
    "Types which have equality defined."
    (== (:a -> :a -> Boolean)))

  (declare /= (Eq :a => (:a -> :a -> Boolean)))
  (define (/= a b)
    (boolean-not (== a b)))

  ;;
  ;; Ord
  ;;

  (define-type Ord
    LT
    EQ
    GT)

  (define-class ((Eq :a) => (Ord :a))
    "Types whose values can be ordered."
    (<=> (:a -> :a -> Ord)))

  (declare > (Ord :a => (:a -> :a -> Boolean)))
  (define (> x y)
    "Is X greater than Y?"
    (match (<=> x y)
      ((GT) True)
      (_ False)))

  (declare < (Ord :a => (:a -> :a -> Boolean)))
  (define (< x y)
    "Is X less than Y?"
    (match (<=> x y)
      ((LT) True)
      (_ False)))

  (declare >= (Ord :a => (:a -> :a -> Boolean)))
  (define (>= x y)
    "Is X greater than or equal to Y?"
    (match (<=> x y)
      ((LT) False)
      (_ True)))

  (declare <= (Ord :a => (:a -> :a -> Boolean)))
  (define (<= x y)
    "Is X less than or equal to Y?"
    (match (<=> x y)
      ((GT) False)
      (_ True)))

  (declare max (Ord :a => (:a -> :a -> :a)))
  (define (max x y)
    "Returns the greater element of X and Y."
    (if (> x y)
        x
        y))

  (declare min (Ord :a => (:a -> :a -> :a)))
  (define (min x y)
    "Returns the lesser element of X and Y."
    (if (< x y)
        x
        y))

  ;;
  ;; Num
  ;;

  (define-class ((Eq :a) => (Num :a))
    "Types which have numeric operations defined."
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
    "Types with an associative binary operation defined."
    (<> (:a -> :a -> :a)))

  (define-class (Semigroup :a => (Monoid :a))
    "Types with an associative binary operation and identity defined."
    (mempty (:a)))

  (define-class (Functor :f)
    "Types which can map an inner type where the mapping adheres to the identity and composition laws."
    (map ((:a -> :b) -> (:f :a) -> (:f :b))))

  (define-class (Functor :f => (Applicative :f))
    "Types which are a functor which can embed pure expressions and sequence operations."
    (pure (:a -> (:f :a)))
    (liftA2 ((:a -> :b -> :c) -> (:f :a) -> (:f :b) -> (:f :c))))

  (define-class (Applicative :m => (Monad :m))
    "Types which are monads as defined in Haskell. See https://wiki.haskell.org/Monad for more information."
    (>>= ((:m :a) -> (:a -> (:m :b)) -> (:m :b)))
    (>> ((:m :a) -> (:m :b) -> (:m :b))))

  (define-class (Monad :m => (MonadFail :m))
    (fail (String -> (:m :a))))

  (define-class (Applicative :f => (Alternative :f))
    "Types which are monoids on applicative functors."
    (alt ((:f :a) -> (:f :a) -> (:f :a)))
    (empty (:f :a)))

  ;;
  ;; Conversions
  ;;

  (define-class (Into :from :to)
    "INTO imples *every* element of :FROM can be represented by an element of :TO. This conversion might not be injective (i.e., there may be elements in :TO that don't correspond to any in :FROM)."
    (into (:from -> :to)))

  (define-class ((Into :a :b) (Into :b :a) => (Iso :a :b))
    "Opting into this marker typeclass imples that the instances for (Into :a :b) and (Into :b :a) form a bijection.")

  (define-instance (Into :a :a)
    (define (into x) x))

  (define-class (TryInto :from :to)
    "TRY-INTO implies *most* elements of :FROM can be represented exactly by an element of :TO, but sometimes not. If not, an error string is returned."
    ;; Ideally we'd have an associated-type here instead of locking in
    ;; on String.
    (tryInto (:from -> (Result String :to))))

  (define-instance (Iso :a :a))

  (define-class (Unwrappable :f)
    "Types which might be able to be unwrapped, otherwise returning a default value."
    (withDefault (:a -> (:f :a) -> :a))
    (unwrap ((:f :a) -> :a))))
