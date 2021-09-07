(in-package #:coalton-library)

(coalton-toplevel
  (declare numerator (Fraction -> Integer))
  (define (numerator q)
    "The numerator of a fraction Q."
    (match q
      ((%Fraction n _) n)))

  (declare denominator (Fraction -> Integer))
  (define (denominator q)
    "The denominator of a fraction Q."
    (match q
      ((%Fraction _ d) d)))

  (define-instance (Show Fraction)
    (define (show q)
      (lisp String (q)
        (cl:format cl:nil "~D/~D" (numerator q) (denominator q))))))
