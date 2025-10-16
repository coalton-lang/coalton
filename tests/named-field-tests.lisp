;;;; named-field-tests.lisp
;;;;
;;;; Tests for ADT named fields (dot-prefix syntax)

(in-package #:coalton-tests)

(deftest test-parse-simple-named-field ()
  "Test parser accepts simple dot-prefix field name"
  (check-coalton-types
   "(define-type Point
      (Point (.x Integer) (.y Integer)))"))

(deftest test-parse-mixed-named-and-positional-constructors ()
  "Test parser accepts mix of named and positional constructors in same type"
  (check-coalton-types
   "(define-type Shape
      (Circle (.radius Integer) (.center Integer))
      (Triangle Integer Integer Integer))"))

(deftest test-parse-single-named-field ()
  "Test parser accepts constructor with single named field"
  (check-coalton-types
   "(define-type Wrapper
      (Wrapper (.value Integer)))"))

(deftest test-parse-named-field-with-type-variables ()
  "Test parser accepts named fields with type variables"
  (check-coalton-types
   "(define-type (Pair :a :b)
      (Pair (.fst :a) (.snd :b)))"))

(deftest test-reject-mixed-positional-and-named-in-constructor ()
  "Test parser rejects mixed positional and named fields in same constructor"
  (let ((error (collect-compiler-error
                "(define-type Bad
                   (Constructor (.named Integer) Integer))")))
    (is (not (null error))
        "Should reject constructor with mixed positional and named fields")))

(deftest test-reject-field-without-dot-prefix ()
  "Test parser rejects field name without dot prefix when other fields are named"
  (let ((error (collect-compiler-error
                "(define-type Bad
                   (Constructor (.named Integer) (unnamed Integer)))")))
    (is (not (null error))
        "Should reject field without dot prefix in named constructor")))

(deftest test-parse-named-field-with-complex-types ()
  "Test parser accepts named fields with complex types"
  (check-coalton-types
   "(define-type Data
      (Data (.items (List Integer))
            (.count Integer)))"))
