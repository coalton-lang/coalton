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


;; Test that predicates are correctly removed when deferred
(coalton:coalton-toplevel
  (coalton:define (gh-295-f a)
    (coalton:let ((g (coalton:fn (x)
                       (coalton-library:== x (coalton-library:make-list a)))))
      g))

  (coalton:define gh-295-x (gh-295-f 1 (coalton-library:make-list 2 3 4)))
  (coalton:define gh-295-y (gh-295-f 1 (coalton-library:make-list 1))))

;; See gh #295
(deftest test-deferred-predicate-removal ()
  (is (equal gh-295-x nil))
  (is (equal gh-295-y t)))
