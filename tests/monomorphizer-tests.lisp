(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define (gh-624a x y)
    (+ x y))

  (declare gh-624b (Num :a => :a -> :a))
  (define gh-624b
    (gh-624a 2))

  (monomorphize)
  (declare gh-624c (Integer -> Integer))
  (define (gh-624c y)
    (gh-624b y)))

(define-test test-monomorphizer-over-application ()
  (is (== 4 (gh-624c 2))))

(coalton-toplevel
  (define (underapplication-a x y) (+ x y))

  (monomorphize)
  (declare underapplication-b (Integer -> Integer))
  (define underapplication-b
    (underapplication-a 2)))

(define-test test-monomorphizer-under-application ()
  (is (== 4 (underapplication-b 2))))


(coalton-toplevel
  (define (partial-monomorphization-a x y z)
    (== x y)
    (+ 1 z))

  (monomorphize)
  (declare partial-monomorphization-b (Eq :a => :a -> :a -> Integer -> Integer))
  (define (partial-monomorphization-b x y z)
    (partial-monomorphization-a x y z)))

(define-test test-partial-monomorphization ()
  (is (== 3 (partial-monomorphization-b "x" "x" 2))))
