;;;; adt-accessor-tests.lisp
;;;;
;;;; Tests for ADT field accessors (named fields with dot-prefix syntax)

(in-package #:coalton-tests)

(deftest test-adt-basic-accessors ()
  "Test basic accessor usage on ADT named fields"
  (check-coalton-types
   "(define-type Point
      (Pt (.x Integer) (.y Integer)))

    (define p (Pt 1 2))
    (define x (.x p))
    (define y (.y p))"

   '("p" . "Point")
   '("x" . "Integer")
   '("y" . "Integer")))

(deftest test-adt-polymorphic-accessors ()
  "Test accessors with polymorphic ADT fields"
  (check-coalton-types
   "(define-type (Wrapper :a)
      (Wrap (.inner :a)))

    (define x (.inner (Wrap #\\X)))"

   '("x" . "Char"))

  (check-coalton-types
   "(define-type (Pair :a :b)
      (Pair2 (.fst :a) (.snd :b)))

    (define x (.fst (Pair2 42 \"hello\")))
    (define y (.snd (Pair2 42 \"hello\")))"

   '("x" . "Integer")
   '("y" . "String")))

(deftest test-adt-chained-accessors ()
  "Test chained accessor calls"
  (check-coalton-types
   "(define-type (Wrapper :a)
      (Wrap (.inner :a)))

    (define x (.inner (.inner (.inner (Wrap (Wrap (Wrap #\\X)))))))"

   '("x" . "Char")))

(deftest test-adt-accessors-as-functions ()
  "Test using accessors as first-class functions"
  (check-coalton-types
   "(define-type Point
      (Pt (.x Integer) (.y Integer)))

    (define xs (map .x (make-list (Pt 1 2) (Pt 3 4) (Pt 5 6))))"

   '("xs" . "(List Integer)"))

  (check-coalton-types
   "(define-type Point
      (Pt (.x Integer) (.y Integer)))

    (declare f (Point -> Integer))
    (define f .x)"))

(deftest test-adt-accessor-in-let-binding ()
  "Test accessor usage in let bindings"
  (check-coalton-types
   "(define-type (Wrapper :a)
      (Wrap (.inner :a)))

    (declare f (Wrapper :a -> :a))
    (define (f x)
      (let ((y (.inner x)))
       y))"))

(deftest test-adt-multiple-constructors-different-fields ()
  "Test that different constructors in same type can have different named fields"
  (check-coalton-types
   "(define-type Shape
      (Circle (.radius Integer))
      (Rectangle (.width Integer) (.height Integer)))

    (define r (.radius (Circle 5)))
    (define w (.width (Rectangle 10 20)))
    (define h (.height (Rectangle 10 20)))"

   '("r" . "Integer")
   '("w" . "Integer")
   '("h" . "Integer")))

(deftest test-adt-accessor-type-inference ()
  "Test that accessors properly constrain types during inference"
  (check-coalton-types
   "(define-type Point
      (Pt (.x Integer) (.y Integer)))

    (declare get-x (Point -> Integer))
    (define (get-x p)
      (.x p))"))

(deftest test-adt-accessor-with-pattern-match ()
  "Test combining accessors with pattern matching"
  (check-coalton-types
   "(define-type Point
      (Pt (.x Integer) (.y Integer)))

    (declare magnitude-squared (Point -> Integer))
    (define (magnitude-squared p)
      (+ (* (.x p) (.x p))
         (* (.y p) (.y p))))"))

(deftest test-adt-positional-no-accessors ()
  "Test that positional (non-named) constructor fields don't create accessors"
  (signals error
    (check-coalton-types
     "(define-type Point
        (Pt Integer Integer))

      (define p (Pt 1 2))
      (define x (.x p))")))

(deftest test-adt-accessor-nonexistent-field ()
  "Test error when accessing non-existent field"
  (signals error
    (check-coalton-types
     "(define-type Point
        (Pt (.x Integer) (.y Integer)))

      (define p (Pt 1 2))
      (define z (.z p))")))

(deftest test-adt-accessor-wrong-constructor ()
  "Test that accessing field from wrong constructor type-checks (partial accessor)"
  (check-coalton-types
   "(define-type Shape
      (Circle (.radius Integer))
      (Rectangle (.width Integer) (.height Integer)))

    (define c (Circle 5))
    (define w (.width c))"

   '("c" . "Shape")
   '("w" . "Integer")))
