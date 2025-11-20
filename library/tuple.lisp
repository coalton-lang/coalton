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
   #:sequence-tuple3
   #:Tuple4
   #:.fourth
   #:sequence-tuple4
   #:Tuple5
   #:.fifth
   #:sequence-tuple5))

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

  (declare sequence-tuple3 (Monad :m
                            => Tuple3 (:m :a) (:m :b) (:m :c)
                            -> :m (Tuple3 :a :b :c)))
  (define (sequence-tuple3 (Tuple3 a? b? c?))
    "Flatten a Tuple of wrapped-values. Particularly useful for types like
(Tuple (Optional :a) (Optional :b)), etc."
    (do
     (a <- a?)
     (b <- b?)
     (c <- c?)
     (pure (Tuple3 a b c))))

  (derive Eq Hash Default)
  (define-struct (Tuple4 :a :b :c :d)
    (first :a)
    (second :b)
    (third :c)
    (fourth :d))

  (declare sequence-tuple4 (Monad :m
                            => Tuple4 (:m :a) (:m :b) (:m :c) (:m :d)
                            -> :m (Tuple4 :a :b :c :d)))
  (define (sequence-tuple4 (Tuple4 a? b? c? d?))
    "Flatten a Tuple of wrapped-values. Particularly useful for types like
(Tuple (Optional :a) (Optional :b)), etc."
    (do
     (a <- a?)
     (b <- b?)
     (c <- c?)
     (d <- d?)
     (pure (Tuple4 a b c d))))

  (derive Eq Hash Default)
  (define-struct (Tuple5 :a :b :c :d :e)
    (first :a)
    (second :b)
    (third :c)
    (fourth :d)
    (fifth :e))

  (declare sequence-tuple5 (Monad :m
                            => Tuple5 (:m :a) (:m :b) (:m :c) (:m :d) (:m :e)
                            -> :m (Tuple5 :a :b :c :d :e)))
  (define (sequence-tuple5 (Tuple5 a? b? c? d? e?))
    "Flatten a Tuple of wrapped-values. Particularly useful for types like
(Tuple (Optional :a) (Optional :b)), etc."
    (do
     (a <- a?)
     (b <- b?)
     (c <- c?)
     (d <- d?)
     (e <- e?)
     (pure (Tuple5 a b c d e))))

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
