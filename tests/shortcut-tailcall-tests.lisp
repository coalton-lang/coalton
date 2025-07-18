(cl:in-package #:coalton-native-tests)

(coalton-toplevel
  (declare and-tailcall (Integer -> Boolean))
  (define (and-tailcall n) (and (> n 0) (and-tailcall (1- n))))

  (declare or-tailcall (Integer -> Boolean))
  (define (or-tailcall n) (or (== n 0) (or-tailcall (1- n))))
  )

(define-test test-shortcut-tailcall ()
  ;; This causes stack overflow with buggy compiler
  (is (== False (and-tailcall 1000000)))
  (is (== True (or-tailcall 1000000)))
  )
