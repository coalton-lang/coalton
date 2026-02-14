(coalton/utils:defstdlib-package #:coalton/classes
  (:use
   #:coalton)
  (:local-nicknames
   (#:types #:coalton/types))
  (:export
   #:Signalable
   #:error
   #:Tuple
   #:Optional #:Some #:None
   #:Result #:Ok #:Err
   #:Eq #:==
   #:Hash
   #:Ord #:LT #:EQ #:GT
   #:<=> #:> #:< #:>= #:<=
   #:max
   #:min
   #:Num #:+ #:- #:* #:fromInt
   #:Semigroup #:<>
   #:Monoid #:mempty
   #:Functor #:map
   #:Applicative #:pure #:liftA2
   #:Monad #:>>=
   #:MonadTransformer #:lift
   #:>> #:join
   #:MonadFail #:fail
   #:Alternative #:alt #:empty
   #:Foldable #:fold #:foldr #:mconcat #:mconcatmap
   #:mempty? #:mcommute?
   #:Traversable #:traverse
   #:Bifunctor #:bimap #:map-fst #:map-snd
   #:sequence
   #:Into
   #:TryInto
   #:Iso
   #:Unwrappable #:unwrap-or-else #:with-default #:unwrap #:unwrap-into #:expect #:as-optional
   #:default #:defaulting-unwrap #:default?))

(in-package #:coalton/classes)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;;
;;; Signaling errors and warnings
;;;

(coalton-toplevel

 ;;
 ;; Signalling errors on supported types
 ;;
 (define-class (Signalable :a)
   "Signals errors or warnings by calling their respective lisp conditions."
   (error "Signal an error with a type-specific error string." (:a -> :b)))

  (define-instance (Signalable String)
    (define (error str)
      (lisp :a (str)
        (cl:error str)))))


(coalton-toplevel

  ;;
  ;; Base Types
  ;;

  (define-struct (Tuple :a :b)
    "A heterogeneous collection of items."
    (first :a)
    (second :b))

  (define-type (Result :bad :good)
    "Represents something that may have failed."
    ;; We write (Result :bad :good) instead of (Result :good :bad)
    ;; because of the limitations of how we deal with higher-kinded
    ;; types; we want to implement Functor on this.
    (Ok :good)
    (Err :bad))

  ;;
  ;; Eq
  ;;

  (define-class (Eq :a)
    "Types which have equality defined."
    (== (:a -> :a -> Boolean)))

  (define-instance (Eq types:LispType)
    (define (== a b)
      (lisp Boolean (a b)
        (cl:equalp a b))))

  (define-class (Eq :a => Num :a)
    "Types which have numeric operations defined."
    (+ (:a -> :a -> :a))
    (- (:a -> :a -> :a))
    (* (:a -> :a -> :a))
    (fromInt (Integer -> :a)))

  (define-instance (Eq Unit)
    (define (== _ _) True))

  ;;
  ;; Hash
  ;;

  #+sbcl
  (repr :native (cl:unsigned-byte 62))

  #+allegro
  (repr :native (cl:unsigned-byte 0 32))

  ;; https://github.com/Clozure/ccl/blob/ff51228259d9dbc8a9cc7bbb08858ef4aa9fe8d0/level-0/l0-hash.lisp#L1885
  #+ccl
  (repr :native (cl:and cl:fixnum cl:unsigned-byte))

  #-(or sbcl allegro ccl)
  #.(cl:error "hashing is not supported on ~A" (cl:lisp-implementation-type))

  (define-type Hash
    "Implementation dependent hash code.")

  (define-class (Eq :a => Hash :a)
    "Types which can be hashed for storage in hash tables.

The hash function must satisfy the invariant that `(== left right)` implies `(== (hash left) (hash right))`."
    (hash (:a -> Hash)))

  ;;
  ;; Ord
  ;;

  (repr :enum)
  (define-type Ord
    "The result of an ordered comparison."
    LT "Less than"
    EQ "Equal to"
    GT "Greater than")

  (define-instance (Eq Ord)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (LT) (LT)) True)
        ((Tuple (EQ) (EQ)) True)
        ((Tuple (GT) (GT)) True)
        (_                 False))))

  (define-instance (Ord Ord)
    (define (<=> a b)
      (match (Tuple a b)
        ((Tuple (LT) (LT)) EQ)
        ((Tuple (LT) (EQ)) LT)
        ((Tuple (LT) (GT)) LT)
        ((Tuple (EQ) (LT)) GT)
        ((Tuple (EQ) (EQ)) EQ)
        ((Tuple (EQ) (GT)) LT)
        ((Tuple (GT) (LT)) GT)
        ((Tuple (GT) (EQ)) GT)
        ((Tuple (GT) (GT)) EQ))))

  (define-class (Eq :a => Ord :a)
    "Types whose values can be ordered. Requires `Eq`."
    (<=>
     "Given two objects, return their comparison (as an `Ord` object)."
     (:a -> :a -> Ord)))

  (declare > (Ord :a => :a -> :a -> Boolean))
  (define (> x y)
    "Is `x` greater than `y`?"
    (match (<=> x y)
      ((GT) True)
      (_ False)))

  (declare < (Ord :a => :a -> :a -> Boolean))
  (define (< x y)
    "Is `x` less than `y`?"
    (match (<=> x y)
      ((LT) True)
      (_ False)))

  (declare >= (Ord :a => :a -> :a -> Boolean))
  (define (>= x y)
    "Is `x` greater than or equal to `y`?"
    (match (<=> x y)
      ((LT) False)
      (_ True)))

  (declare <= (Ord :a => :a -> :a -> Boolean))
  (define (<= x y)
    "Is `x` less than or equal to `y`?"
    (match (<=> x y)
      ((GT) False)
      (_ True)))

  (declare max (Ord :a => :a -> :a -> :a))
  (define (max x y)
    "Returns the greater element of `x` and `y`."
    (if (> x y)
        x
        y))

  (declare min (Ord :a => :a -> :a -> :a))
  (define (min x y)
    "Returns the lesser element of `x` and `y`."
    (if (< x y)
        x
        y))

  ;;
  ;; Haskell
  ;;

  (define-class (Semigroup :a)
    "Types with an associative binary operation defined."
    (<> (:a -> :a -> :a)))

  (define-class (Semigroup :a => Monoid :a)
    "Types with an associative binary operation and identity defined."
    (mempty :a))

  (define-class (Functor :f)
    "Types which can map an inner type where the mapping adheres to the identity and composition laws."
    (map ((:a -> :b) -> :f :a -> :f :b)))

  (define-class (Functor :f => Applicative :f)
    "Types which are a functor which can embed pure expressions and sequence operations."
    (pure (:a -> (:f :a)))
    (liftA2 ((:a -> :b -> :c) -> :f :a -> :f :b -> :f :c)))

  (define-class (Applicative :m => Monad :m)
    "Types which are monads as defined in Haskell. See https://wiki.haskell.org/Monad for more information."
    (>>= (:m :a -> (:a -> :m :b) -> :m :b)))

  (define-class (MonadTransformer :t)
    "Types which are monads that wrap another monad, allowing you to use - for example - State and Result
together."
    (lift (Monad :m => :m :a -> :t :m :a)))

  (declare >> (Monad :m => (:m :a) -> (:m :b) -> (:m :b)))
  (define (>> a b)
    "Equivalent to `(>>= a (fn (_) b))`."
    (>>= a (fn (_) b)))

  (declare join (Monad :m => :m (:m :a) -> :m :a))
  (define (join m)
    "Equivalent to `(>>= m id)`."
    (>>= m (fn (x) x)))

  (define-class (Monad :m => MonadFail :m)
    (fail (String -> :m :a)))

  (define-class (Applicative :f => Alternative :f)
    "Types which are monoids on applicative functors."
    (alt (:f :a -> :f :a -> :f :a))
    (empty (:f :a)))

  (define-class (Foldable :container)
    "Types which can be folded into a single element."
    (fold  "A left tail-recursive fold."       ((:accum -> :elt -> :accum) -> :accum -> :container :elt -> :accum))
    (foldr "A right non-tail-recursive fold."  ((:elt -> :accum -> :accum) -> :accum -> :container :elt -> :accum)))

  (declare mempty? ((Eq :a) (Monoid :a) => :a -> Boolean))
  (define (mempty? a)
    "Does `a` equal `(the Type mempty)`?"
    (== mempty a))

  (declare mconcat ((Foldable :f) (Monoid :a) => :f :a -> :a))
  (define (mconcat a)
    "Fold a container of monoids into a single element."
    (fold <> mempty a))

  (declare mconcatmap ((Foldable :f) (Monoid :a) => (:b -> :a) -> :f :b -> :a))
  (define (mconcatmap f a)
    "Map a container to a container of monoids, and then fold that container into a single element."
    (fold (fn (a b) (<> a (f b))) mempty a))

  (declare mcommute? ((Eq :a) (Semigroup :a) => :a -> :a -> Boolean))
  (define (mcommute? a b)
    "Does `a <> b` equal `b <> a`?"
    (== (<> a b) (<> b a)))

  (define-class (Traversable :t)
    (traverse (Applicative :f => (:a -> :f :b) -> :t :a -> :f (:t :b))))

  (declare sequence ((Traversable :t) (Applicative :f) => :t (:f :b) -> :f (:t :b)))
  (define sequence (traverse (fn (x) x)))

  (define-class (Bifunctor :f)
    "Types which take two type arguments and are functors on both."
    (bimap ((:a -> :b) -> (:c -> :d) -> :f :a :c -> :f :b :d)))

  (declare map-fst (Bifunctor :f => (:a -> :b) -> :f :a :c -> :f :b :c))
  (define (map-fst f b)
    "Map over the first argument of a `Bifunctor`."
    (bimap f (fn (x) x) b))

  (declare map-snd (Bifunctor :f => (:b -> :c) -> :f :a :b -> :f :a :c))
  (define (map-snd f b)
    "Map over the second argument of a `Bifunctor`."
    (bimap (fn (x) x) f b))

  ;;
  ;; Conversions
  ;;

  (define-class (Into :a :b)
    "`INTO` implies *every* element of `:a` can be represented by an element of `:b`. This conversion might not be bijective (i.e., there may be elements in `:b` that don't correspond to any in `:a`)."
    (into (:a -> :b)))

  (define-class ((Into :a :b) (Into :b :a) => Iso :a :b)
    "Opting into this marker typeclass implies that the instances for `(Into :a :b)` and `(Into :b :a)` form a bijection.")

  (define-instance (Into :a :a)
    (define (into x) x))

  (define-class (TryInto :a :b :c (:a :b -> :c))
    "`TRY-INTO` implies some elements of `:a` can be represented exactly by an element of `:b`, but sometimes not. If not, an error of type `:c` is returned."
    (tryInto (:a -> (Result :c :b))))

  (define-instance (Iso :a :a))

  ;;
  ;; Unwrappable for fallible unboxing
  ;;

  (define-class (Unwrappable :container)
    "Containers which can be unwrapped to get access to their contents.

`(unwrap-or-else succeed fail container)` should invoke the `succeed` continuation on the unwrapped contents of
`container` when successful, or invoke the `fail` continuation with no arguments (i.e., with `Unit` as an argument)
when unable to unwrap a value.

The `succeed` continuation will often, but not always, be the identity function. `as-optional` passes `Some` to
construct an `Optional`.

Typical `fail` continuations are:
- Return a default value, or
- Signal an error."
    (unwrap-or-else ((:elt -> :result)
                     -> (Unit -> :result)
                     -> (:container :elt)
                     -> :result)))

  (define-instance (Unwrappable (Result :a))
  (define (unwrap-or-else succeed fail res)
    (match res
      ((Ok elt) (succeed elt))
      ((Err _) (fail)))))

  (declare expect ((Unwrappable :container) =>
                   String
                   -> (:container :element)
                   -> :element))
  (define (expect reason container)
    "Unwrap `container`, signaling an error with the description `reason` on failure."
    (unwrap-or-else (fn (elt) elt)
                    (fn () (error reason))
                    container))

  (declare unwrap ((Unwrappable :container) =>
                   (:container :element)
                   -> :element))
  (define (unwrap container)
    "Unwrap `container`, signaling an error on failure."
    (unwrap-or-else (fn (elt) elt)
                    (fn () (error (lisp String (container)
                                    (cl:format cl:nil "Unexpected ~a in UNWRAP"
                                               container))))
                    container))

  (declare unwrap-into (TryInto :a :b :c => :a -> :b))
  (define (unwrap-into x)
    "Same as `tryInto` followed by `unwrap`."
    (unwrap (tryinto x)))

  (declare with-default ((Unwrappable :container) =>
                         :element
                         -> (:container :element)
                         -> :element))
  (define (with-default default container)
    "Unwrap `container`, returning `default` on failure."
    (unwrap-or-else (fn (elt) elt)
                    (fn () default)
                    container))

  (declare as-optional ((Unwrappable :container) => (:container :elt) -> (Optional :elt)))
  (define (as-optional container)
    "Convert any Unwrappable container into an `Optional`, constructing Some on a successful unwrap and None on a failed unwrap."
    (unwrap-or-else Some
                    (fn () None)
                    container))


  ;;
  ;; Default
  ;;

  (define-class (Default :a)
    "Types which have default values."
    (default (Unit -> :a)))

  (declare defaulting-unwrap ((Unwrappable :container) (Default :element) =>
                              (:container :element) -> :element))
  (define (defaulting-unwrap container)
    "Unwrap an `unwrappable`, returning `(default)` of the wrapped type on failure."
    (unwrap-or-else (fn (elt) elt)
                    (fn () (default))
                    container))

  (declare default? ((Default :a) (Eq :a) => :a -> Boolean))
  (define (default? x)
      "Is `x` the default item of its type?"
      (== x (default))))


#+sb-package-locks
(sb-ext:lock-package "COALTON/CLASSES")
