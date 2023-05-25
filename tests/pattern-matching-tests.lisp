(in-package #:coalton-native-tests)

(define-test test-match-on-strings ()
  (is (match "red"
        ("red" True)
        (_ False))))
