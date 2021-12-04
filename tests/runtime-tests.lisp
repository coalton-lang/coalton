(in-package #:coalton-tests)

(coalton:coalton-toplevel
    (coalton:define (f x inc goal)
      (coalton-library:if (coalton-library:== x goal)
                          goal
                          (f (coalton-library:+ x inc) inc goal)))

    (coalton:define x (f 0 1 5)))

;; Test that functions with typeclass constraints call themselfs recursively
;; This was bugged in the past #216 #147 #125
(deftest test-recursive-predicate-calls ()
  (is (equal x 5)))
