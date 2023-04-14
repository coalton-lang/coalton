(in-package #:coalton-tests)

(defmacro coalton-example-with-gensym ()
  (let ((x (gensym)))
    `(coalton:coalton
      (coalton:let ((,x 5))
        ,x))))

(deftest test-uninterned-symbols ()
  (let ((y (coalton-example-with-gensym)))
    (is (= y 5))))
