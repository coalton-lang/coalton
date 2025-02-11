(coalton-library/utils:defstdlib-package #:coalton-library/tuple
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/hash)
  (:export
   #:fst
   #:snd
   #:Tuple3
   #:Tuple4
   #:Tuple5))

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

  (define-struct (Tuple3 :a :b :c)
    (first :a)
    (second :b)
    (third :c))

  (define-struct (Tuple4 :a :b :c :d)
    (first :a)
    (second :b)
    (third :c)
    (fourth :d))

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

  ;;
  ;; Larger Tuple Instances
  ;;

  (define-instance ((Eq :a) (Eq :b) (Eq :c) => Eq (Tuple3 :a :b :c))
    (define (== a b)
      (and (== (.first a)
               (.first b))
           (== (.second a)
               (.second b))
           (== (.third a)
               (.third b)))))

  (define-instance ((Hash :a) (Hash :b) (Hash :c) => Hash (Tuple3 :a :b :c))
    (define (hash item)
      (combine-hashes
       (hash (.first item))
       (combine-hashes
        (hash (.second item))
        (hash (.third item))))))

  (define-instance ((Eq :a) (Eq :b) (Eq :c) (Eq :d) => Eq (Tuple4 :a :b :c :d))
    (define (== a b)
      (and (== (.first a)
               (.first b))
           (== (.second a)
               (.second b))
           (== (.third a)
               (.third b))
           (== (.fourth a)
               (.fourth b)))))

  (define-instance ((Hash :a) (Hash :b) (Hash :c) (Hash :d) => Hash (Tuple4 :a :b :c :d))
    (define (hash item)
      (combine-hashes
       (hash (.first item))
       (combine-hashes
        (hash (.second item))
        (combine-hashes
         (hash (.third item))
         (hash (.fourth item)))))))

  (define-instance ((Eq :a) (Eq :b) (Eq :c) (Eq :d) (Eq :e) => Eq (Tuple5 :a :b :c :d :e))
    (define (== a b)
      (and (== (.first a)
               (.first b))
           (== (.second a)
               (.second b))
           (== (.third a)
               (.third b))
           (== (.fourth a)
               (.fourth b))
           (== (.fifth a)
               (.fifth b)))))

  (define-instance ((Hash :a)
                    (Hash :b)
                    (Hash :c)
                    (Hash :d)
                    (Hash :e)
                    => Hash (Tuple5 :a :b :c :d :e))
    (define (hash item)
      (combine-hashes
       (hash (.first item))
       (combine-hashes
        (hash (.second item))
        (Combine-hashes
         (hash (.third item))
         (combine-hashes
          (hash (.fourth item))
          (hash (.fifth item))))))))

  ;;
  ;; Default instances
  ;;

  (define-instance ((Default :a) (Default :b) => (Default (Tuple :a :b)))
    (define (default) (Tuple (default) (default))))

  (define-instance ((Default :a) (Default :b) (Default :c) =>
                    (Default (Tuple3 :a :b :c)))
    (define (default) (Tuple3 (default) (default) (default))))

  (define-instance ((Default :a) (Default :b) (Default :c) (Default :d) =>
                    (Default (Tuple4 :a :b :c :d)))
    (define (default) (Tuple4 (default) (default) (default) (default))))

  (define-instance ((Default :a) (Default :b) (Default :c) (Default :d) (Default :e) =>
                    (Default (Tuple5 :a :b :c :d :e)))
    (define (default) (Tuple5 (default) (default) (default) (default) (default)))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/TUPLE")
