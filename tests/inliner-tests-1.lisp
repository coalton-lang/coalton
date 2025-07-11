;; Issue on redefining inlinable functions
;; https://github.com/coalton-lang/coalton/issues/1499

;; This file needs to be loaded before inliner-tests.lisp, serving
;; "previous definitions".  See

(in-package #:coalton-native-tests)

(coalton-toplevel
  (inline)
  (define (test-fact n)
    (if (== 0 n)
        2
        (test-fact (1- n))))

  (inline)
  (define (test-inlinable-rec-1 n)
    (if (== n 0)
        333
        (test-inlinable-rec-2 (1- n))))
  (inline)
  (define (test-inlinable-rec-2 n)
    (if (== n 0)
        444
        (test-inlinable-rec-1 (1- n))))

  (define-instance (RecursiveInlineTestClass Integer)
    (inline)
    (define (test-fact-method n)
      (if (== 0 n)
          2
          (test-fact-method (1- n))))))

(coalton-toplevel
  (define (test-fact-caller-1)
    (test-fact 10)))

(define-test inlinable-function-redefinition-test ()
  "Ensure redefining inliable functions isn't affected by previous definition"
  (is (== (test-inlinable-rec-1 2) 333))
  (is (== (test-inlinable-rec-1 3) 444))
  (is (== (test-fact-caller-1) 2)))

(define-test inlinable-method-redefinition-test ()
  (is (== (test-fact-method 1) 2)))

(in-package #:coalton-tests)

(deftest limit-unroll-test-after-redefinition ()
  "Check unrolling recursive proc after redefinition"
  (unroll-limit-test-proc 'coalton-native-tests::test-fact-caller-1))
