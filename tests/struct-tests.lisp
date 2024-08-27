;;;; struct-tests.lisp

(in-package #:coalton-tests)

(deftest test-struct-definition ()
  (check-coalton-types
   "(define-struct Point
      (x \"The x value.\" Integer)
      (y Integer))")

  (check-coalton-types
   "(define-struct (Point :a)
      (x :a)
      (y \"The y value.\" :a))"))

(deftest test-struct-definition-parse-errors ()
  (signals source:source-error
    (check-coalton-types
     "(define-struct Point
        5
        (y Integer))"))

  (signals source:source-error
    (check-coalton-types
     "(define-struct Point
        (5 Integer)
        (y Integer))"))

  (signals source:source-error
    (check-coalton-types
     "(define-struct Point
        (x)
        (y Integer))"))

  (signals source:source-error
    (check-coalton-types
     "(define-struct Point
        (x \"the x value.\")
        (y Integer))"))

  (signals source:source-error
    (check-coalton-types
     "(define-struct Point
        (x Integer \"the x value\")
        (y Integer))"))

  (signals source:source-error
    (check-coalton-types
     "(define-struct Point
        (x \"the x value\" Integer \"also, it's the x value\")
        (y Integer))")))

(deftest test-struct-accessors ()
  (check-coalton-types
   "(define-struct Point
      (x Integer)
      (y Integer))

    (define p (Point 1 2))
    (define x (.x p))
    (define y (.y p))"

   '("p" . "Point")
   '("x" . "Integer")
   '("y" . "Integer"))

  (check-coalton-types
   "(define-struct (Wrapper :a)
      (inner :a))

    (define x (.inner (Wrapper #\\X)))"

   '("x" . "Char"))

  (check-coalton-types
   "(define-struct (Wrapper :a)
      (inner :a))

    (define x (.inner (.inner (.inner (Wrapper (Wrapper (Wrapper #\\X)))))))"

   '("x" . "Char")))

(deftest test-invalid-struct-accessors ()
  (signals tc:tc-error
    (check-coalton-types
     "(define x (.x #\\X))"))

  (signals tc:tc-error
    (check-coalton-types
     "(define-struct Point
        (x Integer)
        (y Integer))

      (define x (.q (Point 1 2)))")))

(deftest test-struct-accessors-as-functions ()
  (check-coalton-types
   "(define-struct Point
      (x Integer)
      (y Integer))

    (define xs (map .x (make-list (Point 1 2) (Point 3 4) (Point 5 6))))"

   '("xs" . "(List Integer)"))

  (check-coalton-types
   "(define-struct Point
      (x Integer)
      (y Integer))

    (declare f (Point -> Integer))
    (define f .x)"))

(deftest test-ambiguous-accessors ()
  (signals tc:tc-error
    (check-coalton-types
     "(define (f p)
      (.x p))"))

  (signals tc:tc-error
    (check-coalton-types
     "(define-type Point
       (x Integer)
       (y Integer))

      (define (f _)
        (let ((g (fn (p) (.x p))))
          (g (Point 1 2))))")))

;; See gh #959
(deftest test-accessor-on-argument-let-binding ()
  (check-coalton-types
   "(define-struct (Wrapper :a)
     (inner :a))

    (declare f (Wrapper :a -> :a))
    (define (f x)
      (let ((y (.inner x)))
       y))"))
