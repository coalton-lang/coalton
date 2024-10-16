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

(deftest test-parametric-alias-definition ()

  (check-coalton-types
   "(define-alias (UnaryOperator :a) (:a -> :a))")

  (check-coalton-types
   "(define-alias (Collapse :a :b :c :d) (:d -> :c -> :b -> :a))"))

(deftest test-parametric-alias-the ()
  (check-coalton-types
   "(define-alias Index UFix)
    (define-alias (Collection :a) (List :a))

    (define l (the (Collection Index) (make-list 1 2 3 4)))"

   '("l" . "(List UFix)")))

(deftest test-parametric-alias-declare ()
  (check-coalton-types
   "(define-alias (UnaryOperator :a) (:a -> :a))

    (declare f (UnaryOperator Integer))
    (define f 1+)"

   '("f" . "(Integer -> Integer)"))

  (check-coalton-types
   "(define-alias (UnaryOperator :a) (:a -> :a))

    (declare f ((Num :a) => (UnaryOperator :a)))
    (define f 1+)"))

(deftest test-parametric-alias-constructors ()
  (check-coalton-types
   "(define-alias (Pair :a) (Tuple :a :a))

    (define-type (Translation :a)
      (Translation (Pair (Pair :a))))

    (declare get-original-x-coordinate ((Translation :a) -> :a))
    (define (get-original-x-coordinate (Translation (Tuple (Tuple x _) _))) x)

    (define t (Translation (Tuple (Tuple 2 3) (Tuple 5 7))))
    (define x (get-original-x-coordinate t))"

   '("get-original-x-coordinate" . "(Translation :a -> :a)")
   '("t" . "(Translation Integer)")
   '("x" . "Integer"))

  (check-coalton-types
   "(define-alias (Pair :a) (Tuple :a :a))

    (define-struct (Translation :a)
      (from (Pair :a))
      (to (Pair :a)))

    (define t (Translation (Tuple 2 3) (Tuple 5 7)))
    (define from (.from t))"

   '("t" . "(Translation Integer)")
   '("from" . "(Tuple Integer Integer)")))
