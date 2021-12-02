(in-package #:coalton-tests)

(coalton:coalton-toplevel
  (coalton:define (f x)
    (coalton-library:if (coalton-library:== x 1)
                1
                (coalton-library:+ x 1)))

  (coalton:define x (f 0)))

;; Test that functions with typeclass constraints call themselfs recursively
;; This was bugged in the past #216 #147 #125
(deftest test-recursive-predicate-calls ()
  (is (equal x 1)))
