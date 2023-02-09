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
   #:Reciprocable
   #:/
   #:reciprocal
   #:Dividable
   #:general/
   #:/
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
   #:positive?
   #:negative?
   #:nonpositive?
   #:nonnegative?
   #:nonzero?))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(in-package #:coalton-library/math/arith)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;;
  ;; Division
  ;;

  (define-class (Num :a  => Reciprocable :a)
    "Any number with a multiplicative inverse (reciprocal) where:


    1 = (* (reciprocal x) x) = (* x (reciprocal x))
    (/ x y) = (* x (reciprocal y))


If no reciprocal exists for an element, produce a run-time error (e.g. zero).
"
    (/ (:a -> :a -> :a))
    (reciprocal (:a -> :a)))

  (define-class (Dividable :arg-type :res-type)
    "The representation of a type such that division within that type possibly results in another type. For instance,


    (Dividable Integer Fraction)


establishes that division of two `Integer`s can result in a `Fraction`, whereas


    (Dividable Single-Float Single-Float)


establishes that division of two `Single-Float`s can result in a `Single-Float`.

Note that `Dividable` does *not* establish a default result type; you must constrain the result type yourself.

The function general/ is partial, and will error produce a run-time error if the divisor is zero.
"
    ;; This is a type that is more pragmatic and less mathematical in
    ;; nature. It expresses a division relationship between one input
    ;; type and one output type.
    (general/ (:arg-type -> :arg-type -> :res-type)))

  (define-instance (Reciprocable :a => Dividable :a :a)
    (define (general/ a b) (/ a b)))

  (define-class (Transfinite :a)
    "Numberic type with a value for (positive) 'infinity' and/or 'NaN'"
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
        (float-features:float-NaN-p x)))
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
        (float-features:float-NaN-p x)))
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
    "Compute the \"arithmetic shift\" of X by N. "
    (lisp Integer (x n) (cl:ash x n)))

  (declare 1+ ((Num :num) => :num -> :num))
  (define (1+ num)
    "Increment `num`."
    (+ num 1))

  (declare 1- ((Num :num) => :num -> :num))
  (define (1- num)
    "Decrement `num`."
    (- num 1))

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

  (declare nonzero? (Num :a => :a -> Boolean))
  (define (nonzero? x)
    "Is `x` not zero?"
    (/= x 0)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/ARITH")
