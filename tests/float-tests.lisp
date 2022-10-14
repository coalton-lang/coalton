(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class (LooseCompare :a)
    "Loosely compares floats"
    (~ (:a -> :a -> Boolean)))

  (define-instance (LooseCompare Single-Float)
    (define (~ a b)
      (if (and (math:nan? a) (math:nan? b))
          True
          (or (== a b) (< (abs (- a b)) 0.0001f0)))))

  (define-instance (LooseCompare Double-Float)
    (define (~ a b)
      (if (and (math:nan? a) (math:nan? b))
          True
          (or (== a b) (< (abs (- a b)) 1d-6)))))

  (define-instance (LooseCompare big-float:Big-Float)
    (define (~ a b)
      (if (and (math:nan? a) (math:nan? b))
          True
          (or (< (abs (- a b))
                 (^ (math:reciprocal 2) (math:div (big-float:get-precision) 2)))
              (== a b) ))))

  (define-instance ((LooseCompare :a) (Complex :a) => LooseCompare (Complex :a))
    (define (~ a b)
      (and (~ (real-part a) (real-part b))
           (~ (imag-part a) (imag-part b)))))

  (define (test-identity l f g)
    "Check the identity (F x) = (G x) for all x in L"
    (fold (fn (x y) (and x y)) True (zipWith ~ (map f l) (map g l))))

  (declare test-list-single (List Single-Float))
  (define test-list-single
    (make-list math:infinity 0f0 1 math:pi math:ee (negate math:pi)
               (math:sqrt 2) (/ math:pi (math:sqrt 3)) (math:general/ 1 2) 10))

  (declare test-list-double (List Double-Float))
  (define test-list-double
    (make-list math:infinity 0d0 1 math:pi math:ee (negate math:pi)
               (math:sqrt 2) (/ math:pi (math:sqrt 3)) (math:general/ 1 2) 10 100))

  (declare test-list-complex-single (List (Complex Single-Float)))
  (define test-list-complex-single
    (zipWith complex test-list-single (reverse test-list-single)))

  (declare test-list-complex-double (List (Complex Double-Float)))
  (define test-list-complex-double
    (zipWith complex test-list-double (reverse test-list-double)))

  (define (test-identities l)
    ;; Basic Trig
    (is (test-identity
         l
         (fn (x) (+ 1 (math:tan x)))
         (fn (x)
           (+ (+ (math:^ (math:sin x) 2) (math:^ (math:cos x) 2))
              (/ (math:sin x) (math:cos x))))))

    ;; Inverse Trig
    (is (test-identity
         l
         (fn (x) (math:acos (math:cos (math:asin (math:sin x)))))
         (fn (x) (math:asin (math:sin (math:acos (math:cos x)))))))

    (is (test-identity
         l
         (fn (x) (math:asin x))
         (fn (x) (* 2 (math:atan (/ x (+ 1 (math:sqrt (- 1 (math:^ x 2))))))))))

    ;; Logarithm and Exponential
    (is (test-identity
         l
         (fn (x) (math:log x (math:pow x x)))
         (fn (x) (/ (math:ln (math:exp (* x (math:ln x)))) (math:ln x)))))

    ;; Nans and Infinities
    (is (test-identity
         l
         (fn (x) (negate (/ x 0)))
         (fn (x) (/ (negate x) 0))))))

(define-test float-identities ()
  (test-identities test-list-single)
  (test-identities test-list-double)
  (test-identities test-list-complex-single)
  (test-identities test-list-complex-double)
  (test-identities
   (the (List big-float:Big-Float)
        (map into test-list-single))))

(coalton-toplevel
  (declare float-checklist ((math:Dividable Integer :a) => (List :a)))
  (define float-checklist
    (coalton-prelude:zipWith
     math:general/ (coalton:the (coalton:List coalton:Integer) (coalton-prelude:range -100 100)) (coalton-prelude:range 200 1))))

(coalton-toplevel
  (define (check-against-double x y)
    "Checks to see if a double-float within a tolerable error of a big-float thunk."
    (is (<= (abs (- (math:to-fraction (the Double-Float x))
                    (big-float:with-precision-rounding 53 big-float:rndn
                      (fn () (math:to-fraction (the big-float:Big-Float (y)))))))
            ;; 2^-48 is the worst a double-float will compare to a coerced fraction.
            (math:^^ 2 -48)))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro double-check (f)
    "Syntatic sugar for defining big-float  checks against double-floats"
    `(map (fn (x) (check-against-double (,f (fst x)) (fn () (,f (snd x)))))
          (zipWith Tuple float-checklist float-checklist))))

(define-test float-double-to-big ()
  (double-check (fn (x) x))

  (double-check (fn (x) (math:sqrt (abs x))))
  (double-check (fn (x) (math:ln (abs (+ 1 x)))))
  (double-check (fn (x) (math:exp (negate x))))

  (double-check (fn (x) (math:sin x)))
  (double-check (fn (x) (math:cos x)))
  (double-check (fn (x) (math:atan x)))
  (double-check
   (fn (x) (math:asin (* (math:sign x)
                         (min (math:reciprocal (abs x)) (abs x))))))
  (double-check
   (fn (x) (math:acos (* (math:sign x)
                         (min (math:reciprocal (abs x)) (abs x))))))

  Unit)

(coalton-toplevel
  (define (test-constant a b)
    "Checks a big-float against an integer representing the actual first 64 digits."
    (is (<= (abs (- (math:to-fraction
                     (the big-float:Big-Float
                          (big-float:with-precision-rounding 208 big-float:rndn a)))
                    (/ b (^ 10 63))))
            (^^ 2 -207)))))

(define-test float-constants ()
  (test-constant
   big-float:bf-ee 2718281828459045235360287471352662497757247093699959574966967627)
  (test-constant
   big-float:bf-pi 3141592653589793238462643383279502884197169399375105820974944592)
  (test-constant
   (fn () (math:ln 2)) 693147180559945309417232121458176568075500134360255254120680009))
