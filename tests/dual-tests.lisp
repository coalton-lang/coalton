(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

;;; We depend on some of the functionality of float-tests.lisp.

(coalton-toplevel
  (define-instance (LooseCompare :t => LooseCompare (math:Dual :t))
    (define (~ (math:Dual a1 b1) (math:Dual a2 b2))
      (and (~ a1 a2) (~ b1 b2))))

  ;; Some test functions.

  ;; Doubling
  (declare dual-test1 (Num :t => :t -> :t))
  (define (dual-test1 x) (+ x x))

  (define (dtest1 _) 2)

  ;; Squaring
  (declare dual-test2 (Num :t => :t -> :t))
  (define (dual-test2 x) (* x x))

  (define (dtest2 x) (* 2 x))

  ;; Example from docstring
  (declare dual-test3 (Num :t => :t -> :t))
  (define (dual-test3 x) (+ 2 (* 3 x)))

  (define (dtest3 _) 3)

  ;; More complicated example
  (declare dual-test4 ((Num :t) (math:Trigonometric :t) (math:Exponentiable :t) => :t -> :t))
  (define (dual-test4 x) (math:exp (math:sin (math:cos (+ x 1)))))

  (define (dtest4 x)
    (math:negate
     (* (math:sin (+ x 1))
        (* (math:cos (math:cos (+ x 1)))
           (math:exp (math:sin (math:cos (+ x 1))))))))

  ;; A derivative operator
  (declare deriv (Num :t => (math:Dual :t -> math:Dual :t) -> :t -> math:Dual :t))
  (define (deriv f a)
    (f (math:Dual a 1)))

  (define (test-dual dual-f f df a)
    ;; N.B. DUAL-F and F are always the "same function", but they need
    ;; to be instantiated separately on different types.
    (let ((dual-method (deriv dual-f a))
          (manual-method (math:Dual (f a) (df a))))
      (is (~ dual-method manual-method)))))

(define-test dual-tests ()
  (test-dual dual-test1 dual-test1 dtest1 0.0d0)
  (test-dual dual-test1 dual-test1 dtest1 1.0d0)
  (test-dual dual-test1 dual-test1 dtest1 -1.0d0)
  (test-dual dual-test1 dual-test1 dtest1 2.0d0)

  (test-dual dual-test2 dual-test2 dtest2 0.0d0)
  (test-dual dual-test2 dual-test2 dtest2 1.0d0)
  (test-dual dual-test2 dual-test2 dtest2 -1.0d0)
  (test-dual dual-test2 dual-test2 dtest2 2.0d0)

  (test-dual dual-test3 dual-test3 dtest3 4.0d0)

  (test-dual dual-test4 dual-test4 dtest4 0.0d0)
  (test-dual dual-test4 dual-test4 dtest4 1.0d0)
  (test-dual dual-test4 dual-test4 dtest4 -1.0d0)
  (test-dual dual-test4 dual-test4 dtest4 2.0d0)
  (test-dual dual-test4 dual-test4 dtest4 (the Double-Float math:pi)))
