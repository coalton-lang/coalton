(cl:in-package #:coalton-native-tests)

;; test that it's possible to treat a lisp `simple-vector' as a coalton `Vector'
(define-test vector-length-on-non-adjustable ()
  (is (== 3 (vector:length (lisp (Vector UFix) () (cl:vector 0 1 2))))))

(define-test vector-constructor-equivalencies ()
  (let vec = (vector:with-capacity 10))
  (iter:for-each! (flip vector:push! vec)
                  (iter:up-to 10))
  (is (== (vector:make 0 1 2 3 4 5 6 7 8 9)
          vec))
  (is (== (iter:collect-vector! (iter:up-to 10))
          vec)))
