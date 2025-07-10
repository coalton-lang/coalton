;; Issue on redefining inlinable functions
;; https://github.com/coalton-lang/coalton/issues/1499

;; This file needs to be loaded before inliner-tests.lisp, serving
;; "previous definitions".  See

(in-package #:coalton-native-tests)

(coalton-toplevel
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
          (test-fact-method (1- n)))))
  )

(define-test inlinable-function-redefinition-test ()
  "Ensure redefining inliable functions isn't affected by previous definition"
  (is (== (test-inlinable-rec-1 2) 333))
  (is (== (test-inlinable-rec-1 3) 444)))

(define-test inlinable-method-redefinition-test ()
  (is (== (test-fact-method 1) 2)))
