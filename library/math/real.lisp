;;;; real.lisp
;;;;
;;;; Numbers that exist on the real number line

(coalton-library/utils::defstdlib-package #:coalton-library/math/real
    (:use
     #:coalton
     #:coalton-library/math/arith
     #:coalton-library/math/fraction
     #:coalton-library/math/integral
     #:coalton-library/classes
     #:coalton-library/functions)
  (:export
   #:Quantizable
   #:proper
   #:floor
   #:ceiling
   #:truncate
   #:round
   #:Real
   #:real-approx
   #:Rational
   #:to-fraction
   #:best-approx
   #:Quantization
   #:quantize
   #:round-half-up
   #:round-half-down
   #:safe/
   #:exact/
   #:inexact/
   #:floor/
   #:ceiling/
   #:round/
   #:fromfrac))

(in-package #:coalton-library/math/real)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (define-class (Quantizable :a)
    "The representation of a type that allows for rounding operations


    max x such that (floor x) <= x
    min x such that (ceiling x) <= x

And


    (proper x) = (Tuple (truncate x) (- x (truncate x)))


where


    (truncate x) = (* (sign x) (floor (abs x))
"
    (proper (:a -> (Tuple Integer :a)))
    (floor (:a -> Integer))
    (ceiling (:a -> Integer)))

  (define-class ((Quantizable :a) (Num :a) => Real :a)
    "A real number that can be approximated with abs(real-approx x - x) < 2^-n."
    (real-approx (UFix -> :a -> Fraction)))

  (define-class ((Real :a) (Ord :a) => Rational :a)
    "Any number that can be exactly represented by a fraction, or is not finite.

If a rational can be converted from a fraction it must satisfy:


    (into (to-fraction x)) = x
    (into (best-approx x)) = x


Furthermore, `best-approx` returns the simplest fraction, and both functions may be partial.
"
    (to-fraction (:a -> Fraction))
    (best-approx (:a -> Fraction)))

  (declare truncate ((Quantizable :a) => :a -> Integer))
  (define (truncate x)
    "Returns the integer closest/equal to `x` that is within `0` and `x`."
    (match (proper x)
      ((Tuple t _) t)))

  (declare round ((Quantizable :a) (Num :a) => :a -> Integer))
  (define (round x)
    "Return the nearest integer to X, with ties breaking towards even numbers."
    (match (proper x)
      ((Tuple n r)
       (match (<=> (abs (floor r)) (abs (ceiling r)))
         ;; Negative r
         ((GT)
          ;; r <=> -0.5
          (let s = (+ (* 2 r) 1))
          (match (<=> (abs (floor s)) (abs (ceiling s)))
            ((LT) n)
            ((GT) (- n 1))
            ;; r = -0.5
            ((EQ) (if (even? n)
                      n
                      (- n 1)))))
         ;; Positive r
         ((LT)
          ;; r <=> 0.5
          (let s = (- (* 2 r) 1))
          (match (<=> (abs (floor s)) (abs (ceiling s)))
            ((LT) (+ n 1))
            ((GT) n)
            ;; r = 0.5
            ((EQ) (if (even? n)
                      n
                      (+ n 1)))))
         ;; Zero r
         ((EQ) n)))))

  (define (rational-approx precision x)
    "Implemention of `real-approx' for rationals."
    ;; See figure 3 in https://doi.org/10.1145/142675.142726
    (let epsilon = (^ (reciprocal 2) precision))
    (when (== epsilon 0)
      (return (to-fraction x)))
    (let ((approximate-rec
            (fn (e0 p0 q0 e1 p1 q1)
              (cond
                ((and (/= q1 0)
                      (< (abs (- (/ (fromInt p1) (fromInt q1)) x)) epsilon))
                 (mkFraction p1 q1))
                ((/= 0 e1)
                 (let r = (floor (/ e0 e1)))
                 (approximate-rec e1 p1 q1
                                  (- e0 (* (fromInt r) e1))
                                  (- p0 (* r p1))
                                  (- q0 (* r q1))))
                (True (mkFraction p1 q1))))))
      (approximate-rec x 0 1 -1 1 0))))

(cl:defmacro %define-integer-roundings (coalton-type)
  `(coalton-toplevel
     (define-instance (Quantizable ,coalton-type)
       (define (floor x) (fromInt (toInteger x)))
       (define (ceiling x) (fromInt (toInteger x)))
       (define (proper x)
         (Tuple (fromInt (toInteger x)) 0)))

     (define-instance (Real ,coalton-type)
       (define (real-approx _ x) (fromInt (toInteger x))))

     (define-instance (Rational ,coalton-type)
       (define (to-fraction x) (fromint (tointeger x)))
       (define (best-approx x) (fromint (tointeger x))))))

(cl:dolist (ty '(U8 U32 U64 UFix I8 I32 I64 IFix Integer))
  (cl:eval `(%define-integer-roundings ,ty)))

(cl:defmacro %define-native-rationals (type)
  (cl:let
      ((round (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-ROUND")))
       (trunc (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-TRUNCATE"))))
    `(coalton-toplevel
       (define-instance (Quantizable ,type)
         (inline)
         (define (floor q)
           (lisp Integer (q)
             (cl:nth-value 0 (cl:floor q))))
         (inline)
         (define (ceiling q)
           (lisp Integer (q)
             (cl:nth-value 0 (cl:ceiling q))))
         (inline)
         (define (proper q)
           (lisp (Tuple Integer ,type) (q)
             (cl:multiple-value-bind (n r)
                 (cl:truncate q)
               (Tuple n r)))))

       (specialize truncate ,trunc (,type -> Integer))
       (inline)
       (declare ,trunc (,type -> Integer))
       (define (,trunc x)
         (lisp Integer (x)
           (cl:nth-value 0 (cl:truncate x))))

       (define-instance (Real ,type)
         (inline)
         (define (real-approx prec x)
           (rational-approx prec x)))

       (define-instance (Rational ,type)
         (inline)
         (define (to-fraction x)
           (lisp Fraction (x)
             (cl:rational x)))
         (inline)
         (define (best-approx x)
           (lisp Fraction (x)
             (cl:rationalize x))))

       (specialize round ,round (,type -> Integer))
       (inline)
       (declare ,round (,type -> Integer))
       (define (,round x)
         (lisp Integer (x)
           (cl:nth-value 0 (cl:round x)))))))

(%define-native-rationals Fraction)
(%define-native-rationals Single-Float)
(%define-native-rationals Double-Float)

(coalton-toplevel
  (define-struct (Quantization :a)
    "Represents an integer quantization of `:a`."
    (value       "A value of type `:a`."
                 :a)
    (floor       "The greatest integer less than or equal to a particular value."
                 Integer)
    (floor-rem   "The remainder of the floor operation as type `:a`."
                 :a)
    (ceiling     "The least integer greater than or equal to a particular value."
                 Integer)
    (ceiling-rem "The remainder of the ceiling operation as type `:a`."
                 :a))

  (declare quantize (Real :a => (:a -> (Quantization :a))))
  (define (quantize x)
    "Given X, (QUANTIZE X) will return the least integer greater or equal to X,
and the greatest integer less than or equal to X, along with their respective
remainders expressed as values of type of X."
    (let x-floor = (floor x))
    (let x-ceiling = (ceiling x))
    (let x-rem-floor = (- x (fromInt x-floor)))
    (let x-rem-ceiling = (- x (fromInt x-ceiling)))
    (Quantization x x-floor x-rem-floor x-ceiling x-rem-ceiling))

  (declare round-half-up ((Quantizable :a) (Num :a) => :a -> INteger))
  (define (round-half-up x)
    "Return the nearest integer to X, with ties breaking toward positive infinity."
    (ceiling/ (floor (* 2 x)) 2))

  (declare round-half-down ((Quantizable :a) (Num :a) => :a -> INteger))
  (define (round-half-down x)
    "Return the nearest integer to X, with ties breaking toward positive infinity."
    (floor/ (ceiling (* 2 x)) 2))

  (declare safe/ ((Num :a) (Dividable :a :b) => (:a -> :a -> (Optional :b))))
  (define (safe/ x y)
    "Safely divide X by Y, returning None if Y is zero."
    (if (== y 0)
        None
        (Some (general/ x y))))

  (inline)
  (declare exact/ (Integer -> Integer -> Fraction))
  (define (exact/ a b)
    "Exactly divide two integers and produce a fraction."
    (general/ a b))

  (inline)
  (declare inexact/ (Integer -> Integer -> Double-Float))
  (define (inexact/ a b)
    "Compute the quotient of integers as a double-precision float.

Note: This does *not* divide double-float arguments."
    (general/ a b))

  (declare floor/ (Integer -> Integer -> Integer))
  (define (floor/ a b)
    "Divide two integers and compute the floor of the quotient."
    (floor (exact/ a b)))

  (declare ceiling/ (Integer -> Integer -> Integer))
  (define (ceiling/ a b)
    "Divide two integers and compute the ceiling of the quotient."
    (ceiling (exact/ a b)))

  (declare round/ (Integer -> Integer -> Integer))
  (define (round/ a b)
    "Divide two integers and round the quotient."
    (round (exact/ a b)))

  (declare fromfrac (Dividable Integer :a => Fraction -> :a))
  (define (fromfrac q)
    "Converts a fraction to a target type.

Specifically, target types must have an instance of `Dividable Integer :a`.

This conversion may result in loss of fidelity."
    (general/ (numerator q) (denominator q))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/REAL")
