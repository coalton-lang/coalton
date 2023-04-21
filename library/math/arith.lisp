;;;; arith.lisp
;;;;
;;;; Number types and basic arithmetic.

(coalton-library/utils:defstdlib-package #:coalton-library/math/arith
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/functions
   #:coalton-library/utils)
  (:export
   #:Transfinite
   #:infinity
   #:infinite?
   #:finite?
   #:negative-infinity
   #:nan
   #:nan?
   #:negate
   #:abs
   #:sign
   #:ash
   #:1+
   #:1-
   #:1/
   #:positive?
   #:negative?
   #:nonpositive?
   #:nonnegative?
   #:zero?
   #:nonzero?))

(in-package #:coalton-library/math/arith)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (define-class (Transfinite :a)
    "Numeric type with a value for (positive) 'infinity' and/or 'NaN'"
    (infinity :a)
    (infinite? (:a -> Boolean))
    (nan :a)
    (nan? (:a -> Boolean)))

  (declare finite? ((Transfinite :a) => :a -> Boolean))
  (define (finite? x)
    "Neither infinite or NaN."
    (or (infinite? x) (nan? x)))

  (declare negative-infinity ((Transfinite :a) (Num :a) => :a))
  (define negative-infinity
    (negate infinity))

  (define-instance (Transfinite Single-Float)
    (define infinity
      (lisp Single-Float ()
        float-features:single-float-positive-infinity))
    (define nan
      (lisp Single-Float ()
        float-features:single-float-nan))
    (define (nan? x)
      (Lisp Boolean (x)
        #+(not allegro)
        (float-features:float-NaN-p x)
        #+allegro
        (cl:and (float-features:float-NaN-p x) cl:t)))
    (define (infinite? x)
      (Lisp Boolean (x)
        (float-features:float-infinity-p x))))

  (define-instance (Transfinite Double-Float)
    (define infinity
      (lisp Double-Float ()
        float-features:double-float-positive-infinity))
    (define nan
      (lisp Double-Float ()
        float-features:double-float-nan))
    (define (nan? x)
      (Lisp Boolean (x)
        #+(not allegro)
        (float-features:float-NaN-p x)
        #+allegro
        (cl:and (float-features:float-NaN-p x) cl:t)))
    (define (infinite? x)
      (Lisp Boolean (x)
        (float-features:float-infinity-p x))))

  (declare negate (Num :a => :a -> :a))
  (define (negate x)
    "The negation, or additive inverse, of `x`."
    (- 0 x))

  (declare abs ((Ord :a) (Num :a) => :a -> :a))
  (define (abs x)
    "Absolute value of `x`."
    (if (< x 0)
        (negate x)
        x))

  (declare sign ((Ord :a) (Num :a) (Num :b) => :a -> :b))
  (define (sign x)
    "The sign of `x`, where `(sign 0) = 1`."
    (if (< x 0)
        -1
        1))

  (declare ash (Integer -> Integer -> Integer))
  (define (ash x n)
    "Compute the \"arithmetic shift\" of `x` by `n`. "
    (lisp Integer (x n) (cl:ash x n)))

  (declare 1+ ((Num :num) => :num -> :num))
  (define (1+ num)
    "Increment `num`."
    (+ num 1))

  (declare 1- ((Num :num) => :num -> :num))
  (define (1- num)
    "Decrement `num`."
    (- num 1))

  (declare 1/ (Num :num => :num -> :num))
  (define (1/ num)
    "Compute the reciprocal of NUM."
    (/ 1 num))

  (declare positive? ((Num :a) (Ord :a) => :a -> Boolean))
  (define (positive? x)
    "Is `x` positive?"
    (> x 0))

  (declare negative? ((Num :a) (Ord :a) => :a -> Boolean))
  (define (negative? x)
    "Is `x` negative?"
    (< x 0))

  (declare nonpositive? ((Num :a) (Ord :a) => :a -> Boolean))
  (define (nonpositive? x)
    "Is `x` not positive?"
    (<= x 0))

  (declare nonnegative? ((Num :a) (Ord :a) => :a -> Boolean))
  (define (nonnegative? x)
    "Is `x` not negative?"
    (>= x 0))

  (declare zero? (Num :a => :a -> Boolean))
  (define (zero? x)
    "Is `x` zero?"
    (== x 0))

  (declare nonzero? (Num :a => :a -> Boolean))
  (define (nonzero? x)
    "Is `x` not zero?"
    (/= x 0)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/ARITH")
