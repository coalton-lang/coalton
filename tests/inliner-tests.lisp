(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(define-test test-inline-return ()
  ;; See gh #1202
  (is (== 1 (1+
             ((fn (x) (return x)) 0)))))
