;;; dyadic.lisp
;;;
;;; Dyadic rationals meant for implementing big floats
;;; It is not exported into the math package as it is a niche numeric type

(coalton-library/utils::defstdlib-package #:coalton-library/math/dyadic
    (:use
     #:coalton
     #:coalton-library/builtin
     #:coalton-library/classes
     #:coalton-library/math/arith
     #:coalton-library/math/integral
     #:coalton-library/math/real)
  (:local-nicknames
   (#:bits #:coalton-library/bits))
  (:export
   #:Dyadic
   #:integer
   #:simplify
   #:simplify-integer
   #:scale
   #:shift))

(in-package #:coalton-library/math/dyadic)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (define-type Dyadic
    "`(Dyadic n k)` represents the rational n*2^k."
    (Dyadic Integer Integer))

  (declare exact-ilog (Integer -> Integer -> (Optional Integer)))
  (define (exact-ilog b x)
    "Computes the logarithm with base B of X only if the result is an integer."
    (if (== 0 (mod b x))
        (Some (ilog b x))
        None))

  (declare dyadic-compare ((Integer -> Integer -> :a)
                           -> Dyadic -> Dyadic -> :a))
  (define (dyadic-compare f a b)
    "Return the result of a comparision function F on two dyadics A and B."
    (match (Tuple a b)
      ((Tuple (Dyadic n k)
              (Dyadic m j))
       (match (<=> j k)
         ((GT) (f n (lsh m (- j k))))
         ((LT) (f (lsh n (- k j)) m))
         (_ (f n m))))))

  (define-instance (Eq Dyadic)
    (define (== a b)
      (dyadic-compare == a b)))

  (define-instance (Ord Dyadic)
    (define (<=> a b)
      (dyadic-compare <=> a b)))

  (declare dyadic-group ((Integer -> Integer -> Integer)
                         -> Dyadic -> Dyadic -> Dyadic))
  (define (dyadic-group f a b)
    "Apply an operation F on A and B with matching exponents"
    (match (Tuple a b)
      ((Tuple (Dyadic n k)
              (Dyadic m j))
       (if (< k j)
           (Dyadic (f n (lsh m (- j k))) k)
           (Dyadic (f (lsh n (- k j)) m) j)))))

  (define-instance (Num Dyadic)
    (define (+ a b)
      (dyadic-group + a b))
    (define (- a b)
      (dyadic-group - a b))
    (define (* a b)
      (match (Tuple a b)
        ((Tuple (Dyadic n k)
                (Dyadic m j))
         (Dyadic (* n m) (+ j k)))))
    (define (fromInt x) (Dyadic x 0)))

  (define (simplify-integer n)
    "Finds the simplest dyadic given an integer"
    (if (== n 0)
        (Dyadic 0 0)
        (match (divMod n 2)
          ((Tuple d m)
           (if (== m 0)
               (* (Dyadic 1 1)
                  (simplify-integer d))
               (Dyadic n 0))))))

  (define (simplify d)
    "Simplifies a Dyadic by maximizing the absolute value of the exponent."
    (match d
      ((Dyadic m k)
       (* (Dyadic 1 k) (simplify-integer m)))))

  (define-instance (Into Dyadic Fraction)
    (define (into a)
      (match a
        ((Dyadic n k)
         (cond
           ((== k 0) (fromInt n))
           ((> k 0) (fromInt (* n (lsh 1 k))))
           (True
            (exact/ n (rsh 1 k))))))))

  (define-instance (Quantizable Dyadic)
    (define (proper x)
      (match x
        ((Dyadic n k)
         (if (>= k 0)
             (Tuple (* n (lsh 1 k)) 0)
             (match (quotRem n (rsh 1 k))
               ((Tuple q r)
                (Tuple q (Dyadic r k))))))))
    (define (floor x)
      (match x
        ((Dyadic n k)
         (lsh n k))))
    (define (ceiling x)
      (negate (floor (negate x)))))

  (specialize round dyadic-round (Dyadic -> Integer))
  (define (dyadic-round x)
    "Rounds a dyadic to the nearest integer with ties going to even numbers."
    (let (Tuple n r) = (proper x))
    (let m = (if (< r 0)
                 (- n 1)
                 (+ n 1)))
    (match (<=> (- (abs r) (Dyadic 1 -1)) 0)
      ((LT) n)
      ((GT) m)
      ((EQ) (if (even? n) n m))))

  (define-instance (Real Dyadic)
    (define (real-approx a x)
      (real-approx a (the Fraction (into x)))))

  (define-instance (Rational Dyadic)
    (define (to-fraction x) (into x))
    (define (best-approx x) (into x)))

  (define-instance (Into Integer Dyadic)
    (define into fromInt))

  (define (scale x j)
    "Scales the exponent of a dyadic X by J."
    (match x
      ((Dyadic n k) (Dyadic n (+ k j)))))

  (declare shift (UFix -> Dyadic -> Dyadic))
  (define (shift k a)
    "Shift dyadic A to its floor with K+1 bits of precision."
    (let (Dyadic m e) = a)
    (let j = (ilog 2 (abs m)))
    (let delta = (- j (toInteger k)))
    (if (<= delta 0)
        a
        (Dyadic (rsh m delta) (+ delta e)))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/DYADIC")
