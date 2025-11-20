(coalton-library/utils:defstdlib-package #:coalton-library/tuple
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/hash)
  (:export
   #:fst
   #:snd
   #:sequence-tuple
   #:Tuple3
   #:.first
   #:.second
   #:.third
   #:Tuple4
   #:.fourth
   #:Tuple5
   #:.fifth))

(in-package #:coalton-library/tuple)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

  ;;
  ;; Tuple
  ;;

  (declare fst ((Tuple :a :b) -> :a))
  (define (fst (Tuple a _))
    "Get the first element of a tuple."
    a)

  (declare snd ((Tuple :a :b) -> :b))
  (define (snd (Tuple _ b))
    "Get the second element of a tuple."
    b)

  (declare sequence-tuple (Monad :m => Tuple (:m :a) (:m :b) -> :m (Tuple :a :b)))
  (define (sequence-tuple (Tuple a? b?))
    "Flatten a Tuple of wrapped-values. Particularly useful for types like
(Tuple (Optional :a) (Optional :b)), etc."
    (do
     (a <- a?)
     (b <- b?)
     (pure (Tuple a b))))

  (derive Eq Hash Default)
  (define-struct (Tuple3 :a :b :c)
    (first :a)
    (second :b)
    (third :c))

  (derive Eq Hash Default)
  (define-struct (Tuple4 :a :b :c :d)
    (first :a)
    (second :b)
    (third :c)
    (fourth :d))

  (derive Eq Hash Default)
  (define-struct (Tuple5 :a :b :c :d :e)
    (first :a)
    (second :b)
    (third :c)
    (fourth :d)
    (fifth :e))

  ;;
  ;; Tuple instances
  ;;

  (define-instance ((Eq :a) (Eq :b) => Eq (Tuple :a :b))
    (define (== (Tuple a1 b1) (Tuple a2 b2))
      (and (== a1 a2)
           (== b1 b2))))

  (define-instance ((Ord :a) (Ord :b) => Ord (Tuple :a :b))
    (define (<=> a b)
      (let (Tuple a1 a2) = a)
      (let (Tuple b1 b2) = b)
      (match (<=> a1 b1)
        ((LT) LT)
        ((GT) GT)
        ((EQ) (<=> a2 b2)))))

  (define-instance (Into (Tuple :a :b) (Tuple :b :a))
    (define (into (Tuple a b))
      (Tuple b a)))

  (define-instance (Iso (Tuple :a :b) (Tuple :b :a)))

  (define-instance ((Hash :a) (Hash :b) => Hash (Tuple :a :b))
    (define (hash item)
      (combine-hashes
       (hash (.first item))
       (hash (.second item)))))

  (define-instance (Bifunctor Tuple)
    (define (bimap f g (Tuple a b))
      (Tuple (f a) (g b))))

  (define-instance (Traversable (Tuple :a))
    (define (traverse f (Tuple a b))
      (map (Tuple a)
           (f b))))

  (define-instance ((Default :a) (Default :b) => (Default (Tuple :a :b)))
    (define (default) (Tuple (default) (default)))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/TUPLE")
