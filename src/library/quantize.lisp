;;;; quantize.lisp
;;;;
;;;; Non-trivial rounding operations.

(cl:in-package #:coalton-library)

(coalton-toplevel
  (define-instance (Quantizable Integer)
    (define (quantize x)
      (Quantization x x 0 x 0))))

(cl:macrolet ((define-integer-quantizations (cl:&rest int-types)
                `(coalton-toplevel
                   ,@(cl:loop :for ty :in int-types :collect
                        `(define-instance (Quantizable ,ty)
                           (define (quantize x)
                             (let ((n (into x)))
                               (Quantization x n (fromInt 0) n (fromInt 0)))))))))
  (define-integer-quantizations I32 I64 U8 U32 U64))

(coalton-toplevel
  (define-instance (Quantizable Single-Float)
    (define (quantize f)
      (lisp (Quantization Single-Float) (f)
        (uiop:nest
         (cl:multiple-value-bind (fl-quo fl-rem) (cl:floor f))
         (cl:multiple-value-bind (ce-quo ce-rem) (cl:ceiling f))
         (Quantization f fl-quo fl-rem ce-quo ce-rem)))))

  (define-instance (Quantizable Double-Float)
    (define (quantize f)
      (lisp (Quantization Double-Float) (f)
        (uiop:nest
         (cl:multiple-value-bind (fl-quo fl-rem) (cl:floor f))
         (cl:multiple-value-bind (ce-quo ce-rem) (cl:ceiling f))
         (Quantization f fl-quo fl-rem ce-quo ce-rem))))))

(coalton-toplevel
  (define-instance (Quantizable Fraction)
    (define (quantize q)
      (let ((n (numerator q))
            (d (denominator q)))
        (lisp (Quantization Fraction) (n d)
          ;; Not the most efficient... just relying on CL to do the
          ;; work.
          (cl:flet ((to-frac (f)
                      (%Fraction (cl:numerator f) (cl:denominator f))))
            (cl:let ((f (cl:/ n d)))
              (uiop:nest
               (cl:multiple-value-bind (fl-quo fl-rem) (cl:floor f))
               (cl:multiple-value-bind (ce-quo ce-rem) (cl:ceiling f))
               (Quantization f
                             fl-quo (to-frac fl-rem)
                             ce-quo (to-frac ce-rem))))))))))

(coalton-toplevel
  (define (floor x)
    "Return the greatest integer less than or equal to X."
    (match (quantize x)
      ((Quantization _ z _ _ _) z)))

  (define (ceiling x)
    "Return the least integer greater than or equal to X."
    (match (quantize x)
      ((Quantization _ _ _ z _) z)))

  (define (round x)
    "Return the nearest integer to X, with ties breaking toward positive infinity."
    (match (quantize x)
      ((Quantization _ a ar b br)
       (match (<=> (abs ar) (abs br))
         ((LT) a)
         ((GT) b)
         ((EQ) (max a b))))))
  )                                     ; Coalton-Toplevel

(coalton-toplevel
  (declare safe/ ((Dividable :a :b) => (:a -> :a -> (Optional :b))))
  (define (safe/ x y)
    "Safely divide X by Y, returning None if Y is zero."
    (if (== y (fromInt 0))
        None
        (Some (/ x y))))
  )

(coalton-toplevel
  (declare exact/ (Integer -> Integer -> Fraction))
  (define (exact/ a b)
    "Exactly divide two integers and produce a fraction."
    ;; BUG: I don't know why I *have* to specify (the Integer *) here,
    ;; but the type checker fails otherwise.
    (the Fraction (/ (the Integer a) (the Integer b))))

  (declare inexact/ (Integer -> Integer -> Double-Float))
  (define (inexact/ a b)
    "Compute the quotient of integers A and B as a double-precision float.

Note: This does *not* divide double-float arguments."
    (/ a b))

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

  (declare single/ (Single-Float -> Single-Float -> Single-Float))
  (define (single/ a b)
    "Compute the quotient of single-precision floats A and B as a single-precision float."
    (/ a b))

  (declare double/ (Double-Float -> Double-Float -> Double-Float))
  (define (double/ a b)
    "Compute the quotient of single-precision floats A and B as a single-precision float."
    (/ a b))
  )                                     ; Coalton-Toplevel
