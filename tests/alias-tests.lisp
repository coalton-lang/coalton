;;;; alias-tests.lisp

(in-package #:coalton-tests)

(deftest test-alias-definition ()
  (check-coalton-types
   "(define-alias UnaryIntegerOperator (Integer -> Integer))")

  (check-coalton-types
   "(define-alias UnaryIntegerOperator (Integer -> Integer)
      \"An alias for functions mapping integers to integers.\")"))

(deftest test-alias-the ()
  (check-coalton-types
   "(define-alias Index UFix)

    (define i (the Index 5))"

   '("i" . "UFix"))

  (check-coalton-types
   "(define-alias Index UFix)
    (define-alias IndexList (List Index))

    (define indices (the IndexList (make-list 0 1 2 3 4 5)))"

   '("indices" . "(List UFix)")))

(deftest test-alias-declare ()
  (check-coalton-types
   "(define-alias Index UFix)

    (declare i Index)
    (define i 5)"

   '("i" . "UFix"))

  (check-coalton-types
   "(define-alias Index UFix)
    (define-alias IndexList (List Index))

    (declare indices IndexList)
    (define indices (make-list 0 1 2 3 4 5))"

   '("indices" . "(List UFix)")))

(deftest test-alias-constructors ()
  (check-coalton-types
   "(define-alias Coordinate IFix)

    (define-type Point
      (Point Coordinate Coordinate))

    (declare get-x-coordinate (Point -> Coordinate))
    (define (get-x-coordinate (Point x _)) x)

    (define p (Point 2 5))
    (define x (get-x-coordinate p))"

   '("get-x-coordinate" . "(Point -> IFix)")
   '("p" . "Point")
   '("x" . "IFix"))

  (check-coalton-types
   "(define-alias Coordinate IFix)

    (define-struct Point
      (x Coordinate)
      (y Coordinate))

    (define p (Point 2 5))
    (define x (.x p))"

   '("p" . "Point")
   '("x" . "IFix")))
