(in-package #:coalton-user)

(coalton-toplevel

  ;;
  ;; Tuple
  ;;
  
  (declare fst ((Tuple :a :b) -> :a))
  (define (fst t)
    (match t
      ((Tuple a _) a)))
  
  (declare snd ((Tuple :a :b) -> :b))
  (define (snd t)
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

  (define-instance (Iso (Tuple :a :b) (Tuple :b :a))))
