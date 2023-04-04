(in-package #:coalton-tests)

(deftest test-unused-variables ()
  ;; Binding parameters can be unused
  (signals analysis:unused-variable-warning
    (check-coalton-types
     "(define (f x) 5)"))

  ;; Lambda parameters can be unused
  (signals analysis:unused-variable-warning
    (check-coalton-types
     "(define f (fn (x) 5))"))

  ;; Local variables can be unused
  (signals analysis:unused-variable-warning
    (check-coalton-types
     "(define (f x)
        (let x = (+ 1 x))
        5)"))

  ;; Pattern match variables can be unused
  (signals analysis:unused-variable-warning
    (check-coalton-types
     "(define (f x)
        (match x
          ((Tuple x y) 5)))")))

(deftest test-hidden-bindings ()
  (not-signals analysis:unused-variable-warning
    (check-coalton-types
     "(define (f) 5)"))

  (not-signals analysis:unused-variable-warning
    (check-coalton-types
     "(define f (fn () 5))")))
