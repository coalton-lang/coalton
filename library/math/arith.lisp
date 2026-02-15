;;;; arith.lisp
;;;;
;;;; Number types and basic arithmetic.

(coalton/utils:defstdlib-package #:coalton/math/arith
  (:use
   #:coalton
   #:coalton/builtin
   #:coalton/classes
   #:coalton/functions
   #:coalton/utils)
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
   #:zero?
   #:nonzero?))

(in-package #:coalton/math/arith)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;;
  ;; Division
  ;;

  (define-class (Num :a  => Reciprocable :a)
    "Any number with a multiplicative inverse (reciprocal) where:


    1 = (* (reciprocal x) x) = (* x (reciprocal x))
    (/ x y) = (* x (reciprocal y))


If no reciprocal exists for an element, produce a run-time error (e.g., zero).
"
    (/ (:a -> :a -> :a))
    (reciprocal (:a -> :a)))

  (define-class (Dividable :arg-type :res-type)
    "The representation of a type such that division within that type possibly results in another type. For instance,


    (Dividable Integer Fraction)


establishes that division of two `Integer`s can result in a `Fraction`, whereas


    (Dividable F32 F32)


establishes that division of two `F32`s can result in a `F32`.

Note that `Dividable` does *not* establish a default result type; you must constrain the result type yourself.

The function `general/` is partial, and will error produce a run-time error if the divisor is zero.
"
    ;; This is a type that is more pragmatic and less mathematical in
    ;; nature. It expresses a division relationship between one input
    ;; type and one output type.
    (general/ (:arg-type -> :arg-type -> :res-type)))

  (define-instance (Reciprocable :a => Dividable :a :a)
    (inline)
    (define (general/ a b) (/ a b)))

  (define-class (Transfinite :a)
    "Numeric type with a value for (positive) infinity and/or NaN."
    (infinity :a)
    (infinite? (:a -> Boolean))
    (nan :a)
    (nan? (:a -> Boolean)))

  (declare finite? ((Transfinite :a) => :a -> Boolean))
  (define (finite? x)
    "Neither infinite or NaN."
    (not (or (infinite? x) (nan? x))))

  (declare negative-infinity ((Transfinite :a) (Num :a) => :a))
  (define negative-infinity
    (negate infinity))

  (define-instance (Transfinite F32)
    (define infinity
      (lisp F32 ()
        float-features:single-float-positive-infinity))
    (define nan
      (lisp F32 ()
        float-features:single-float-nan))
    (inline)
    (define (nan? x)
      (Lisp Boolean (x)
        #+(not allegro)
        (float-features:float-NaN-p x)
        #+allegro
        (cl:and (float-features:float-NaN-p x) cl:t)))
    (inline)
    (define (infinite? x)
      (Lisp Boolean (x)
        (float-features:float-infinity-p x))))

  (define-instance (Transfinite F64)
    (define infinity
      (lisp F64 ()
        float-features:double-float-positive-infinity))
    (define nan
      (lisp F64 ()
        float-features:double-float-nan))
    (inline)
    (define (nan? x)
      (Lisp Boolean (x)
        #+(not allegro)
        (float-features:float-NaN-p x)
        #+allegro
        (cl:and (float-features:float-NaN-p x) cl:t)))
    (inline)
    (define (infinite? x)
      (Lisp Boolean (x)
        (float-features:float-infinity-p x))))

  (inline)
  (declare negate (Num :a => :a -> :a))
  (define (negate x)
    "The negation, or additive inverse, of `x`."
    (- 0 x))

  (inline)
  (declare abs ((Ord :a) (Num :a) => :a -> :a))
  (define (abs x)
    "Absolute value of `x`."
    (if (< x 0)
        (negate x)
        x))

  (inline)
  (declare sign ((Ord :a) (Num :a) (Num :b) => :a -> :b))
  (define (sign x)
    "The sign of `x`, where `(sign 0) = 1`."
    (if (< x 0)
        -1
        1))

  (inline)
  (declare ash (Integer -> Integer -> Integer))
  (define (ash x n)
    "Compute the \"arithmetic shift\" of `x` by `n`."
    (lisp Integer (x n) (cl:ash x n)))

  (inline)
  (declare 1+ ((Num :num) => :num -> :num))
  (define (1+ num)
    "Increment `num`."
    (+ num 1))

  (inline)
  (declare 1- ((Num :num) => :num -> :num))
  (define (1- num)
    "Decrement `num`."
    (- num 1))

  (inline)
  (declare positive? ((Num :a) (Ord :a) => :a -> Boolean))
  (define (positive? x)
    "Is `x` positive?"
    (> x 0))

  (inline)
  (declare negative? ((Num :a) (Ord :a) => :a -> Boolean))
  (define (negative? x)
    "Is `x` negative?"
    (< x 0))

  (inline)
  (declare nonpositive? ((Num :a) (Ord :a) => :a -> Boolean))
  (define (nonpositive? x)
    "Is `x` not positive?"
    (<= x 0))

  (inline)
  (declare nonnegative? ((Num :a) (Ord :a) => :a -> Boolean))
  (define (nonnegative? x)
    "Is `x` not negative?"
    (>= x 0))

  (inline)
  (declare zero? (Num :a => :a -> Boolean))
  (define (zero? x)
    "Is `x` zero?"
    (== x 0))

  (inline)
  (declare nonzero? (Num :a => :a -> Boolean))
  (define (nonzero? x)
    "Is `x` not zero?"
    (/= x 0)))

(cl:defmacro %define-abs-native (type)
  (cl:let ((abs (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-ABS"))))
    `(cl:progn
       (coalton-toplevel
         (specialize abs ,abs (,type -> ,type))
         (inline)
         (declare ,abs (,type -> ,type))
         (define (,abs n)
           (lisp ,type (n)
             (cl:abs n)))))))

(%define-abs-native Integer)
(%define-abs-native I8)
(%define-abs-native I16)
(%define-abs-native I32)
(%define-abs-native I64)
(%define-abs-native IFix)
(%define-abs-native U8)
(%define-abs-native U16)
(%define-abs-native U32)
(%define-abs-native U64)
(%define-abs-native UFix)
(%define-abs-native Fraction)
(%define-abs-native F32)
(%define-abs-native F64)

#+sb-package-locks
(sb-ext:lock-package "COALTON/MATH/ARITH")
