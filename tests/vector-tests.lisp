(cl:in-package #:coalton-native-tests)

;; test that it's possible to treat a lisp `simple-vector' as a coalton `Vector'
(define-test vector-length-on-non-adjustable ()
  (is (== 3 (vector:length (lisp (Vector UFix) () (cl:vector 0 1 2))))))
