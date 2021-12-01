(in-package #:coalton-library)

(coalton-toplevel

  ;;
  ;; Tuple
  ;;

  (declare fst ((Tuple :a :b) -> :a))
  (define (fst t)
    "Get the first element of a tuple."
    (match t
      ((Tuple a _) a)))

  (declare snd ((Tuple :a :b) -> :b))
  (define (snd t)
    "Get the second element of a tuple."
    (match t
      ((Tuple _ b) b)))


  ;;
  ;; Tuple instances
  ;;

  (define-instance ((Eq :a) (Eq :b) => (Eq (Tuple :a :b)))
    (define (== a b)
      (and (== (fst a) (fst b))
           (== (snd a) (snd b))))
    (define (/= a b)
      (not (== a b))))

  (define-instance ((Ord :a) (Ord :b) => (Ord (Tuple :a :b)))
    (define <=> undefined))

  (define-instance (Into (Tuple :a :b) (Tuple :b :a))
    (define (into t)
      (match t
        ((Tuple a b) (Tuple b a)))))

  (define-instance (Iso (Tuple :a :b) (Tuple :b :a)))

  ;;
  ;; Larger Tuple Instances
  ;;

  (define-instance ((Eq :a) (Eq :b) (Eq :c) => (Eq (Tuple3 :a :b :c)))
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Tuple3 a1 b1 c1)
                (Tuple3 a2 b2 c2))
         (and (== a1 a2)
              (== b1 b2)
              (== c1 c2)))))
    (define (/= a b)
      (not (== a b))))

  (define-instance ((Eq :a) (Eq :b) (Eq :c) (Eq :d) => (Eq (Tuple4 :a :b :c :d)))
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Tuple4 a1 b1 c1 d1)
                (Tuple4 a2 b2 c2 d2))
         (and (== a1 a2)
              (== b1 b2)
              (== c1 c2)
              (== d1 d2)))))
    (define (/= a b)
      (not (== a b))))

  (define-instance ((Eq :a) (Eq :b) (Eq :c) (Eq :d) (Eq :e) => (Eq (Tuple5 :a :b :c :d :e)))
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Tuple5 a1 b1 c1 d1 e1)
                (Tuple5 a2 b2 c2 d2 e2))
         (and (== a1 a2)
              (== b1 b2)
              (== c1 c2)
              (== d1 d2)
              (== e1 e2)))))
    (define (/= a b)
      (not (== a b)))))
