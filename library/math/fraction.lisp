;;;; fraction.lisp
;;;;
;;;; Reduced ratios of integers

(coalton-library/utils:defstdlib-package #:coalton-library/math/fraction
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/functions
   #:coalton-library/math/arith)
  (:export
   #:mkFraction
   #:numerator
   #:denominator))

(in-package #:coalton-library/math/fraction)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;; We avoid "Rational" or "Ratio" since those might be a more
  ;; generic concept than a humble fraction of integers. This
  ;; fraction is always assumed to be in reduced terms.

  (declare mkFraction (Integer -> Integer -> Fraction))
  (define (mkFraction a b)
    (lisp Fraction (a b)
      (cl:/ a b)))

  (declare numerator (Fraction -> Integer))
  (define (numerator q)
    "The numerator of a fraction."
    (lisp Integer (q)
      (cl:numerator q)))

  (declare denominator (Fraction -> Integer))
  (define (denominator q)
    "The denominator of a fraction."
    (lisp Integer (q)
      (cl:denominator q)))

  (define-instance (Eq Fraction)
    (define (== a b)
      (lisp Boolean (a b)
        (cl:= a b))))

  (define-instance (Ord Fraction)
    (define (<=> a b)
      (lisp Ord (a b)
        (cl:cond
          ((cl:< a b)
           LT)
          ((cl:> a b)
           GT)
          (cl:t
           EQ)))))

  (define-instance (Num Fraction)
    (define (+ p q)
      (lisp Fraction (p q)
        (cl:+ p q)))
    (define (- p q)
      (lisp Fraction (p q)
        (cl:- p q)))
    (define (* p q)
      (lisp Fraction (p q)
        (cl:* p q)))
    (define (fromInt z)
      (lisp Fraction (z) z)))

  (define-instance (Reciprocable Fraction)
    (define (/ a b)
      (lisp Fraction (a b)
        (cl:/ a b)))
    (define (reciprocal q)
      (lisp Fraction (q)
        (cl:/ q))))

  (define-instance (Dividable Integer Fraction)
    (define (general/ x y)
      (mkFraction x y))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/FRACTION")
