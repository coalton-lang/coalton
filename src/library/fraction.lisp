(in-package #:coalton-library)

(coalton-toplevel
  (declare numerator (Fraction -> Integer))
  (define (numerator q)
    "The numerator of a fraction."
    (match q
      ((%Fraction n _) n)))

  (declare denominator (Fraction -> Integer))
  (define (denominator q)
    "The denominator of a fraction."
    (match q
      ((%Fraction _ d) d)))

  (declare reciprocal (Fraction -> Fraction))
  (define (reciprocal q)
    "The reciprocal of a fraction."
    (match q
      ;; n/d and d/n will always be reduced
      ((%Fraction n d) (%Fraction d n)))))
