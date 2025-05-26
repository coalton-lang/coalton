;;;; type-alias-tests.lisp

(in-package #:coalton-tests)

(deftest test-type-alias-definition ()

  (check-coalton-types
   "(define-type-alias UnaryIntegerOperator (Integer -> Integer))")

  (check-coalton-types
   "(define-type-alias UnaryIntegerOperator (Integer -> Integer)
      \"An alias for functions mapping integers to integers.\")"))

(deftest test-type-alias-the ()

  (check-coalton-types
   "(define-type-alias Index UFix)

    (define i (the Index 5))"

   '("i" . "UFix"))

  (check-coalton-types
   "(define-type-alias Index UFix)
    (define-type-alias IndexList (List Index))

    (define indices (the IndexList (make-list 0 1 2 3 4 5)))"

   '("indices" . "(List UFix)")))

(deftest test-type-alias-declare ()

  (check-coalton-types
   "(define-type-alias Index UFix)

    (declare i Index)
    (define i 5)"

   '("i" . "UFix"))

  (check-coalton-types
   "(define-type-alias Index UFix)
    (define-type-alias IndexList (List Index))

    (declare indices IndexList)
    (define indices (make-list 0 1 2 3 4 5))"

   '("indices" . "(List UFix)")))

(deftest test-type-alias-constructors ()

  (check-coalton-types
   "(define-type-alias Coordinate IFix)

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
   "(define-type-alias Coordinate IFix)

    (define-struct Point
      (x Coordinate)
      (y Coordinate))

    (define p (Point 2 5))
    (define x (.x p))"

   '("p" . "Point")
   '("x" . "IFix")))

(deftest test-parametric-type-alias-definition ()

  (check-coalton-types
   "(define-type-alias (UnaryOperator :a) (:a -> :a))")

  (check-coalton-types
   "(define-type-alias (Collapse :a :b :c :d) (:d -> :c -> :b -> :a))"))

(deftest test-complex-type-alias-definition ()

  (check-coalton-types
   "(define-type (T :a :b :c) (T (:a -> :b :c)))
    (define-struct S (x Integer))
    (define-type-alias A1 (Tuple Integer))
    (define-type-alias A2 (T S A1 Integer))")

  (check-coalton-types
   "(define-type-alias (A :a :b) (Tuple :a (Tuple :b :b)))
    (declare f ((A Integer Integer) -> Integer))
    (define (f (Tuple a (Tuple b c))) (+ a (+ b c)))"))

(deftest test-parametric-type-alias-the ()

  (check-coalton-types
   "(define-type-alias Index UFix)
    (define-type-alias (Collection :a) (List :a))

    (define l (the (Collection Index) (make-list 1 2 3 4)))"

   '("l" . "(List UFix)"))

  (check-coalton-types
   "(define-type-alias Index UFix)
    (define-type-alias Collection List)

    (define l (the (Collection Index) (make-list 1 2 3 4)))"

   '("l" . "(List UFix)")))

(deftest test-parametric-type-alias-declare ()

  (check-coalton-types
   "(define-type-alias (UnaryOperator :a) (:a -> :a))

    (declare f (UnaryOperator Integer))
    (define f 1+)"

   '("f" . "(Integer -> Integer)"))


  (check-coalton-types
   "(define-type-alias (FoldFunc :a :b) (:a -> :b -> :a))

    (declare f (FoldFunc (List Integer) Integer))
    (define (f xs x) (Cons x xs))"

   '("f" . "((List Integer) -> Integer -> (List Integer))"))

  (check-coalton-types
   "(define-type-alias (UnaryOperator :a) (:a -> :a))

    (declare f ((Num :a) => (UnaryOperator :a)))
    (define f 1+)"))

(deftest test-parametric-type-alias-constructors ()

  (check-coalton-types
   "(define-type-alias (Pair :a) (Tuple :a :a))

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
   "(define-type-alias (Pair :a) (Tuple :a :a))

    (define-struct (Translation :a)
      (from (Pair :a))
      (to (Pair :a)))

    (define t (Translation (Tuple 2 3) (Tuple 5 7)))
    (define from (.from t))"

   '("t" . "(Translation Integer)")
   '("from" . "(Tuple Integer Integer)"))

  (check-coalton-types
   "(define-type-alias S String)
    (define-class (C :a)
      (m (S -> :a)))
    (define-instance (C Integer)
      (define (m _) 5))
    (declare s S)
    (define s \"Hello, world!\")
    (define x (the Integer (m s)))"))
