(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(define-test test-inline-return ()
  ;; See gh #1202
  (is (== 1 (1+
             ((fn (x) (return x)) 0)))))

(in-package #:coalton-tests)

;; See gh #1293
(deftest test-inliner-rename-bound-variables ()
  (check-coalton-types
   "(declare f (Integer -> Integer))
    (define (f n)
      (when (== n 0)
        (return 0))
      (when (== n 1)
        (return 1))
      (+ (f (- n 1))
         (f (- n 2))))"))
