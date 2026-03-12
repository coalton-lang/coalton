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
   "(define-type-alias (Collapse :a :b :c :d) (:d * :c * :b -> :a))"))

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
   "(define-type-alias (FoldFunc :a :b) (:a * :b -> :a))

    (declare f (FoldFunc (List Integer) Integer))
    (define (f xs x) (Cons x xs))"

   '("f" . "((List Integer) * Integer -> (List Integer))"))

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

(deftest test-mutually-recursive-type-alias-constructors ()

  (check-coalton-types
   "(define-type (MyParser :a)
      (MyParser :a))

    (declare my-pure (:a -> (MyParser :a)))
    (define (my-pure x) (MyParser x))

    (declare my-liftA2 ((:a * :b -> :c) * (MyParser :a) * (MyParser :b) -> (MyParser :c)))
    (define (my-liftA2 f (MyParser a) (MyParser b))
      (MyParser (f a b)))

    (declare my-liftA3 ((:a * :b * :c -> :d) * (MyParser :a) * (MyParser :b) * (MyParser :c) -> (MyParser :d)))
    (define (my-liftA3 f (MyParser a) (MyParser b) (MyParser c))
      (MyParser (f a b c)))

    (declare my-many ((MyParser :a) -> (MyParser (List :a))))
    (define (my-many (MyParser x))
      (MyParser (make-list x)))

    (declare my-must ((MyParser :a) -> (MyParser :a)))
    (define (my-must p) p)

    (define-type MyFormName
      (MyName))

    (define-type MyFormParam
      (MyParam))

    (define-type MyForm
      (MyForm MyFormName MyFormKind))

    (define-type-alias MyFormParams (List MyFormParam))
    (define-type-alias MyStructField (Tuple MyForm MyFormParams))

    (define-type MyFormKind
      (MyKStruct MyFormParams (List MyStructField)))

    (declare my-form-name (MyParser MyFormName))
    (define my-form-name (my-pure MyName))

    (declare my-form-param (MyParser MyFormParam))
    (define my-form-param (my-pure MyParam))

    (declare my-value-any (MyParser MyFormKind))
    (define my-value-any
      (my-pure (MyKStruct (make-list) (make-list))))

    (declare my-struct-field (MyParser MyStructField))
    (define my-struct-field
      (my-liftA3 (fn (name kind params) (Tuple (MyForm name kind) params))
                 my-form-name
                 my-value-any
                 (my-many my-form-param)))

    (declare my-struct (MyParser MyForm))
    (define my-struct
      (my-liftA2 (fn (_tag (Tuple3 name size fields))
                   (MyForm name (MyKStruct (make-list size) fields)))
                 (my-pure MyName)
                 (my-must
                  (my-liftA3 (fn (name size fields) (Tuple3 name size fields))
                             my-form-name
                             my-form-param
                             (my-many my-struct-field)))))"

   '("my-struct" . "(MyParser MyForm)")))

(deftest test-self-referential-type-alias-constructors ()

  ;; See https://github.com/coalton-lang/coalton/issues/1633.
  ;; `as1` is declared explicitly to isolate the alias-resolution bug
  ;; from the newer expansive-binding generalization check.
  (check-coalton-types
   "(define-type-alias (IO :a) (IOBind :a Unit))

    (define-type (IOBind :a :b)
      (IOPure :a)
      (IOBind (IO :a) (:a -> IO :b)))

    (define (make-iopure a) (IOPure a))
    (define (make-iobind io f) (IOBind io f))

    (declare as1 (IO Integer))
    (define as1 (make-iopure 1))
    (define cc (make-iobind as1 (fn (_x) (make-iopure \"a\"))))"

   '("make-iobind" . "((IOBind :a Unit) * (:a -> (IOBind :b Unit)) -> (IOBind :a :b))")
   '("cc" . "(IOBind Integer String)"))

  (check-coalton-types
   "(define-type-alias A S)

    (define-struct S
      (x A))

    (declare get-x (S -> S))
    (define get-x .x)"

   '("S" . "(S -> S)")
   '("get-x" . "(S -> S)")))

(deftest test-type-aliases-as-predicates ()

  ;; See https://github.com/coalton-lang/coalton/issues/1662
  (check-coalton-types
   "(define-class (C :t :u))
    (define-type T)

    (define-type-alias A T)
    (declare f (C :t A => :t -> String))
    (define f (const \"\"))

    (define-type-alias B (List T))
    (declare g (C :t B => :t -> String))
    (define g (const \"\"))"

   '("f" . "((C :t T) => (:t -> String))")
   '("g" . "((C :t (List T)) => (:t -> String))")))
