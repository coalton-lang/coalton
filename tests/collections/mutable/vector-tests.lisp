(cl:in-package #:coalton-native-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Collections Tests                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(collection-tests Vector)
(mutable-collection-tests Vector)
(linear-collection-tests Vector)
(mutable-linear-collection-tests Vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal & Vector-Specific Tests                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel
  (declare >-ord (Integer -> Integer -> Ord))
  (define (>-ord a b)
    (cond
      ((== a b)
       EQ)
      ((< a b)
       GT)
      ((> a b)
       LT))))

(define-test test-vector-constructor-equivalencies ()
  (let vec = (vector:with-capacity 10))
  (iter:for-each! (fn (x)
                    (cln:push-end! x vec)
                    Unit)
                  (iter:up-to 10))
  (is (== (vector:make 0 1 2 3 4 5 6 7 8 9)
          vec))
  (is (== (iter:collect! (iter:up-to 10))
          vec)))

(define-test test-vector-initial-element ()
  (== (vector:make "x" "x" "x") (cln:new-repeat 3 "x")))

