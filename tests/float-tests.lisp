(in-package #:coalton-native-tests)

(coalton-toplevel
  (define-class (LooseCompare :a)
    (~ (:a -> :a -> Boolean)))

  (define-instance (LooseCompare Single-Float)
    (define (~ a b)
      (if (and (math:nan? a) (math:nan? b))
          True
          (or (== a b) (< (abs (- a b)) 0.0001)))))

  (define-instance (LooseCompare Double-Float)
    (define (~ a b)
      (if (and (math:nan? a) (math:nan? b))
          True
          (or (== a b) (< (abs (- a b)) 1d-6)))))

  #+sbcl
  (define-instance (LooseCompare big-float:Big-Float)
    (define (~ a b)
      (if (and (math:nan? a) (math:nan? b))
          True
          (or (== a b) (< (abs (- a b)) (into 1d-6))))))

  (define-instance ((LooseCompare :a) (Complex :a) => LooseCompare (Complex :a))
    (define (~ a b)
      (and (~ (real-part a) (real-part b))
           (~ (imag-part a) (imag-part b)))))

  (define (test-identity l f g)
    "Check the identity (F x) = (G x) for all x in L"
    (fold (fn (x y) (and x y)) True (zipWith ~ (map f l) (map g l))))

  (declare test-list-single (List Single-Float))
  (define test-list-single
    (make-list math:infinity math:nan 0.0 1 math:pi math:ee (negate math:pi)
               (math:sqrt 2) (/ math:pi (math:sqrt 3)) (math:general/ 1 2) 10 100))

  (declare test-list-double (List Double-Float))
  (define test-list-double
    (make-list math:infinity 0.0d0 1 math:pi math:ee (negate math:pi)
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
           (+ (+ (math:pow (math:sin x) 2) (math:pow (math:cos x) 2))
              (/ (math:sin x) (math:cos x))))))

    ;; Inverse Trig
    (is (test-identity
         l
         (fn (x) (math:acos (math:cos (math:asin (math:sin x)))))
         (fn (x) (math:asin (math:sin (math:acos (math:cos x)))))))

    (is (test-identity
         l
         (fn (x) (math:asin x))
         (fn (x) (* 2 (math:atan (/ x (+ 1 (math:sqrt (- 1 (math:pow x 2))))))))))

    ;; Logarithm and Exponential
    (is (test-identity
         l
         (fn (x) (math:log x (math:pow x x)))
         (fn (x) (/ (math:ln (math:exp (* x (math:ln x)))) (math:ln x)))
         ))

    ;; Nans and Infinities
    (is (test-identity
         l
         (fn (x) (negate (/ x 0)))
         (fn (x) (/ (negate x) 0))))))

(define-test test-float ()
  (test-identities test-list-single)
  (test-identities test-list-double)
  (test-identities test-list-complex-single)
  (test-identities test-list-complex-double)
  #+sbcl (test-identities
          (the (List big-float:Big-Float)
               (map into test-list-single))))
