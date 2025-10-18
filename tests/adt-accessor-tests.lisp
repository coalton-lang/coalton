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

(deftest test-adt-accessor-multiple-constructors-same-field-same-type ()
  "Test accessors when multiple constructors have same field name with same type"
  (check-coalton-types
   "(define-type MyResult
      (MyOk (.value Integer))
      (MyErr (.value Integer)))

    (define ok-val (.value (MyOk 42)))
    (define err-val (.value (MyErr 100)))"

   '("ok-val" . "Integer")
   '("err-val" . "Integer")))

(deftest test-adt-accessor-in-composition ()
  "Test accessor composition with chained accessors"
  (check-coalton-types
   "(define-type (Inner :a)
      (Inner (.val :a)))

    (define-type (Outer :a)
      (Outer (.inner (Inner :a))))

    (declare get-val ((Outer :a) -> :a))
    (define (get-val outer)
      (.val (.inner outer)))

    (define x (get-val (Outer (Inner 42))))"

   '("get-val" . "((Outer :a) -> :a)")
   '("x" . "Integer")))

(deftest test-adt-accessor-runtime-multiple-constructors ()
  "Test that accessors work at runtime when field exists in multiple constructors.
This is a RUNTIME test that actually executes the accessor code."

  (with-coalton-compilation (:package #:coalton-user)
    (coalton:coalton-toplevel
      (coalton:define-type (Point :num)
        (P2D (.x :num) (.y :num))
        (P3D (.x :num) (.y :num) (.z :num)))

      ;; Test accessor in polymorphic function
      (coalton:declare get-x ((Point :num) -> :num))
      (coalton:define (get-x p) (.x p))

      (coalton:declare get-y ((Point :num) -> :num))
      (coalton:define (get-y p) (.y p))

      (coalton:declare get-z ((Point :num) -> :num))
      (coalton:define (get-z p) (.z p))))

  ;; Test polymorphic accessor functions work on different constructors
  (is (= 10 (funcall 'coalton-user::get-x (funcall 'coalton-user::p2d 10 20)))
      "polymorphic get-x should work on P2D")
  (is (= 1 (funcall 'coalton-user::get-x (funcall 'coalton-user::p3d 1 2 3)))
      "polymorphic get-x should work on P3D")
  (is (= 20 (funcall 'coalton-user::get-y (funcall 'coalton-user::p2d 10 20)))
      "polymorphic get-y should work on P2D")
  (is (= 2 (funcall 'coalton-user::get-y (funcall 'coalton-user::p3d 1 2 3)))
      "polymorphic get-y should work on P3D")
  (is (= 3 (funcall 'coalton-user::get-z (funcall 'coalton-user::p3d 1 2 3)))
      "polymorphic get-z should work on P3D"))
