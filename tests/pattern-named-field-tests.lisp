(in-package #:coalton-tests)

;;;; Tests for named field pattern matching

(deftest test-basic-named-field-pattern ()
  "Test basic named field pattern matching"
  (check-coalton-types
   "(define-type Point
      (Pt (.x Integer) (.y Integer)))

    (define (sum-coords p)
      (match p
        ((Pt .x .y) (+ x y))))

    (define test-point (sum-coords (Pt 10 20)))"
   '("sum-coords" . "(Point -> Integer)")
   '("test-point" . "Integer")))

(deftest test-out-of-order-field-pattern ()
  "Test out-of-order named field pattern matching"
  (check-coalton-types
   "(define-type Point
      (Pt (.x Integer) (.y Integer)))

    (define (sum-reversed p)
      (match p
        ((Pt .y .x) (+ x y))))  ; Out of order

    (define test (sum-reversed (Pt 10 20)))"
   '("sum-reversed" . "(Point -> Integer)")
   '("test" . "Integer")))

(deftest test-partial-field-pattern ()
  "Test partial field pattern (extracting subset of fields)"
  (check-coalton-types
   "(define-type Point
      (Pt (.x Integer) (.y Integer)))

    (define (get-x-only p)
      (match p
        ((Pt .x) x)))  ; Only extract x, y is implicit wildcard

    (define test (get-x-only (Pt 10 20)))"
   '("get-x-only" . "(Point -> Integer)")
   '("test" . "Integer")))

(deftest test-mixed-constructors-named-positional ()
  "Test ADT with both named and positional constructors"
  (check-coalton-types
   "(define-type Shape
      (Circle (.radius Integer))
      (Square Integer))  ; Positional

    (define (area s)
      (match s
        ((Circle .radius) (* radius radius))
        ((Square side) (* side side))))

    (define test-circle (area (Circle 5)))
    (define test-square (area (Square 4)))"
   '("area" . "(Shape -> Integer)")
   '("test-circle" . "Integer")
   '("test-square" . "Integer")))

(deftest test-nested-named-field-patterns ()
  "Test nested structures with named field patterns"
  (check-coalton-types
   "(define-type Point
      (Pt (.x Integer) (.y Integer)))

    (define-type Segment
      (Seg (.start Point) (.end Point)))

    (define (get-start-x seg)
      (match seg
        ((Seg .start) (match start
                       ((Pt .x) x)))))

    (define test-segment (get-start-x (Seg (Pt 1 2) (Pt 3 4))))"
   '("get-start-x" . "(Segment -> Integer)")
   '("test-segment" . "Integer")))

(deftest test-polymorphic-named-field-pattern ()
  "Test named field patterns with polymorphic types"
  (check-coalton-types
   "(define-type (Pair :a :b)
      (Pair2 (.first :a) (.second :b)))

    (define (get-first p)
      (match p
        ((Pair2 .first) first)))

    (define test-int (get-first (Pair2 42 \"hello\")))
    (define test-str (get-first (Pair2 \"world\" 99)))"
   '("test-int" . "Integer")
   '("test-str" . "String")))

(deftest test-named-pattern-as-function-param ()
  "Test named field patterns in function parameters"
  (check-coalton-types
   "(define-type Point
      (Pt (.x Integer) (.y Integer)))

    (define (add-coords (Pt .x .y))
      (+ x y))

    (define test (add-coords (Pt 10 20)))"
   '("add-coords" . "(Point -> Integer)")
   '("test" . "Integer")))

(deftest test-multiple-constructors-partial-fields ()
  "Test pattern matching with multiple constructors and partial fields"
  (check-coalton-types
   "(define-type Shape
      (Circle (.radius Integer))
      (Rectangle (.width Integer) (.height Integer)))

    (define (get-first-dimension s)
      (match s
        ((Circle .radius) radius)
        ((Rectangle .width) width)))

    (define test-circle (get-first-dimension (Circle 5)))
    (define test-rect (get-first-dimension (Rectangle 10 20)))"
   '("get-first-dimension" . "(Shape -> Integer)")
   '("test-circle" . "Integer")
   '("test-rect" . "Integer")))

(deftest test-named-pattern-with-binding ()
  "Test named field pattern with @ binding"
  (check-coalton-types
   "(define-type Point
      (Pt (.x Integer) (.y Integer)))

    (define (process p)
      (match p
        ((= whole (Pt .x .y))
         (if (== x 0)
             whole
             (Pt y x)))))

    (define test (process (Pt 10 20)))"
   '("process" . "(Point -> Point)")
   '("test" . "Point")))

(deftest test-single-field-named-pattern ()
  "Test named pattern with single field"
  (check-coalton-types
   "(define-type Container
      (SimpleContainer (.contents Integer)))

    (define (get-contents c)
      (match c
        ((SimpleContainer .contents) contents)))

    (define test (get-contents (SimpleContainer 42)))"
   '("get-contents" . "(Container -> Integer)")
   '("test" . "Integer")))
