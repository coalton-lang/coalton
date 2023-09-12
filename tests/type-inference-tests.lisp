;;;; type-inference-tests.lisp

(in-package #:coalton-tests)

(deftest test-type-inference ()
  (check-coalton-types
   "(define f 5)"

   '("f" . "Integer"))

  (check-coalton-types
   "(define f (fn (x) x))
    (define g (f 5))"

   '("f" . "(:a -> :a)")
   '("g" . "Integer"))

  (check-coalton-types
   "(define f (fn (x _y) x))
    (define g (f 5 \"str\"))
    (define h (f \"str\" 5))"

   '("f" . "(:a -> :b -> :a))")
   '("g" . "Integer")
   '("h" . "String"))

  ;; Check that identity qualifies
  (check-coalton-types
   "(define (id_ a) a)
    (define x (id_ 3))
    (define y (id_ \"three\"))"

   '("id_" . "(:a -> :a)")
   '("x" . "Integer")
   '("y" . "String"))

  ;; Check that let bindings are polymorphic over kinds
  (check-coalton-types
   "(define x
     (let ((id (fn (a) a)))
       ((id id) 5)))"

   '("x" . "Integer"))

  ;; Check that let bindings can have explicit types
  (check-coalton-types
   "(define (f x)
      (let ((declare g (Integer -> Integer))
            (g id))
       (g x)))"

   '("f" . "(Integer -> Integer)"))

  ;; Check that you can only call callable things
  (signals tc:tc-error
    (check-coalton-types
     "(define x (0 1))"))

  (signals tc:tc-error
    (check-coalton-types
     "(define x 5)
      (define y (x 1))")))

(deftest test-recursive-type-inference ()
  ;; Check mutually recursive definitions
  (check-coalton-types
   "(define (f a) (g a))
    (define (g b) (f b))"

   '("f" . "(:a -> :b)")
   '("g" . "(:a -> :b)"))

  ;; Check unusual recursive definitions
  (check-coalton-types
   "(define f
       (fn (a) (f a)))"

   '("f" . "(:a -> :b)"))

  (check-coalton-types
   "(define f
       (fn (_a) (f 5)))"

   '("f" . "(Num :a => :a -> :b)")))

(deftest test-explicit-type-declarations ()
  ;; Check that explicit declarations can reduce the type of a definition
  (check-coalton-types
   "(declare f Integer)
    (define f (undefined \"hello\"))"

   '("f" . "Integer"))

  ;; Declarations cannot be less specific than their associated definition
  (signals tc:tc-error
    (check-coalton-types
     "(declare x :a)
      (define x Unit)"))

  ;; Missing explicit predicates cannot be defaulted
  (signals tc:tc-error
    (check-coalton-types
     "(declare x :a)
      (define x 1)"))

  ;; Implicitly typed functions should only infer types from the declared type signature of an explicitly typed functions
  ;; http://jeremymikkola.com/posts/2019_01_12_type_inference_for_haskell_part_12.html
  (check-coalton-types
   "(declare lst (List Integer))
    (define lst (undefined \"a list\"))

    (define (a x) (singleton (b x)))

    (declare b (:a -> :a))
    (define (b y)
      (let ((_foo (c 5)))
        y))

    (define (c z) (append lst (a z)))"

   '("a" . "(:a -> (List :a))")))

(deftest test-type-definitions ()
  ;; Test recursive type definitions
  (check-coalton-types
   "(define-type (Tree_ :a)
      (Leaf :a)
      (Branch (Tree_ :a) (Tree_ :a)))

    (define (f a)
      (Branch a (f a)))"

   '("Leaf" . "(:a -> Tree_ :a)")
   '("Branch" . "(Tree_ :a -> Tree_ :a -> Tree_ :a)")
   '("f" . "(Tree_ :a -> Tree_ :a)"))

  ;; Check mutually recursive type definitions
  (check-coalton-types
   "(define-type (A :a)
      (A (B :a)))

    (define-type (B :a)
      (B (A :a)))")

  ;; Check higher kinded type variables
  (check-coalton-types
   "(define-type (TFix :f)
      (InType (:f (TFix :f))))"

   '("InType" . "(:f (TFix :f) -> TFix :f)"))

  ;; Check that constructors are properly typed
  (signals tc:tc-error
    (check-coalton-types
     "(define-type (Tree_ :a)
        (Leaf :a)
        (Branch (Tree_ :a) (Tree_ :a)))

      (define x (Branch (Leaf 5) (Leaf \"string\")))")))

(deftest test-kind-system ()
  ;; Check that types of kind * cannot be applied to
  (signals tc:tc-error
    (check-coalton-types
     "(declare x (Integer Integer))
      (define x (undefined Unit))"))

  ;; Check that variables can not be declared to have kind (* -> *)
  (signals tc:tc-error
    (check-coalton-types
     "(define-type (Maybe :a)
        (Just :a)
        Nothing)

      (declare x Maybe)
      (define x (undefined Unit))")))

(deftest test-pattern-invariants ()
  ;; Match branches must return the same type
  (signals tc:tc-error
    (check-coalton-types
     "(define-type (Maybe :a)
        (Just :a)
        Nothing)

      (define (f x)
        (match x
          ((Just 5) 5)
          ((Just 6) \"hello\")))"))

  ;; Match branches must match on constructors
  (signals tc:tc-error
    (check-coalton-types
     "(define (g x) x)

      (define (f x)
        (match x
          ((g a) 5)))"))

  ;; Constructors in match branches must be fully applied
  (signals tc:tc-error
    (check-coalton-types
     "(define (g x)
        (match x
          ((Cons x) x)))")))

(deftest test-monomorphism-restriction ()
  ;; Check that functions defined as a lambda are not subject to the
  ;; monomorphism restriction
  (check-coalton-types
   "(define-class (Show :a))

    (declare show (Show :a => :a -> String))
    (define (show _x) \"not impl\")

    (define (f a)
      (show a))

    (define g
      (fn (b)
        (show b)))"

   '("f" . "(Show :a => :a -> String)")
   '("g" . "(Show :a => :a -> String)")))

(deftest test-type-classes ()
  ;; Check that type constraints are propagated
  (check-coalton-types
   "(define (f a b) (== a b))"

   '("f" . "(Eq :a => :a -> :a -> Boolean)"))


  (check-coalton-types
   "(define-class (Eq_ :a)
      (==_ (:a -> :a -> Boolean)))

    (define-type Color Red Green Blue)

    (define-instance (Eq_ Color)
      (define (==_ a b) False))

    (define a (==_ Red Green))

    (define (g x y)
      (==_ x (singleton y)))

    (define (h x y)
      (==_ (singleton x) (singleton y)))"

   '("a" . "Boolean")
   '("g" . "(Eq_ (List :a) => List :a -> :a -> Boolean)"))


  (signals tc:tc-error
    (check-coalton-types
     "(define-class (Eq_ :a)
        (== (:a -> :a -> coalton:Boolean)))

      (define-instance (Eq_ :a => Eq_ (List :a))
         (define (== a b) False))

      (define-type Color Red Blue Green)

      (declare f (List Color -> Boolean))
      (define (f a b)
         (== a b))")))

(deftest test-typeclass-polymorphic-recursion ()
  ;; Check that polymorphic recursion is possible
  (check-coalton-types
   "(declare f (Eq :a => :a -> :a -> Boolean))
    (define (f a b)
      (if (== a b)
        True
        (f (singleton a) (singleton b))))"

   '("f" . "(Eq :a => :a -> :a -> Boolean)"))

  ;; Check that polymorphic recursion is not possible without an explicit binding
  (signals tc:tc-error
    (check-coalton-types
     "(define-class (Eq_ :a)
        (== (:a -> :a -> Boolean)))

      (define-instance (Eq_ :a => Eq_ (List :a))
      (define (== a b) False))

      (define (f a b)
        (match (== a b)
          ((True) True)
          ((False)
           (f (singleton a)
             (singleton b)))))")))

(deftest test-typeclass-definition-constraints ()
  ;; Check that typeclasses cannot have additional constraints defined in a method
  ;;
  ;; this is a stylistic decision and not a technical limitation
  (signals tc:tc-error
    (check-coalton-types
     "(define-class (Test :a)
        (test (Eq :a => :a -> :a)))"))

  ;; Check that typeclass methods can constrain other variables
  (check-coalton-types
   "(define-class (Test :a)
      (test (Eq :b => :a -> :b)))"))


(deftest test-typeclass-flexible-instances ()
  (check-coalton-types
   "(define-class (Eq_ :a)
      (==? (:a -> :a -> Boolean)))

     (define-instance (Eq_ :a => Eq_ (Tuple :a Integer))
       (define (==? a b) False))

     (define-instance (Eq_ Integer)
       (define (==? a b) False))

     (declare f
      (Tuple Integer Integer ->
       Tuple Integer Integer ->
       Boolean))
     (define (f a b)
       (==? a b))"))

(deftest test-typeclass-overlapping-checks ()
  ;; Check than non overlapping instances can be defined
  (signals tc:tc-error
    (check-coalton-types
     "(define-class (Eq_ :a)
        (==? (:a -> :a -> Boolean)))

      (define-instance (Eq_ :a => Eq_ (Tuple :a Integer))
        (define (==? a b) False))

       (define-instance (Eq_ :a => Eq_ (Tuple String :a))
        (define (==? a b) False))")))

(deftest test-typeclass-cyclic-superclass-checks ()
  (signals tc:tc-error
    (check-coalton-types
     "(define-class (TestClassA :a => TestClassB :a))
      (define-class (TestClassB :a => TestClassA :a))"))

  (signals tc:tc-error
    (check-coalton-types
     "(define-class (TestClassB :b)
        (example-method (TestClassA :a => :a -> :b)))
      (define-class (TestClassB :a => TestClassA :a))"))

  (check-coalton-types
   "(define-class (TestClassA :a)
     (example-method (TestClassA :b => :a -> :b)))"))

(deftest test-the ()
  (check-coalton-types
   "(define (f a b)
      ((the (Integer -> Integer -> Boolean) ==) a b))"

   '("f" . "(Integer -> Integer -> Boolean)"))

  (check-coalton-types
   "(define x (the U32 (+ 1 2)))"

   '("x" . "U32")))

(deftest test-regression ()
  ;; Fixed in #283
  (check-coalton-types
   "(define (f a)
      (let ((g (fn (x) (Tuple x a))))
        (g 5)))"

   '("f" . "(Num :b => :a -> Tuple :b :a)")))

(deftest test-function-definition-shorthand ()
  (check-coalton-types
   "(define f (fn () 5))"
   '("f" . "(Num :a => Unit -> :a)")))

(deftest test-function-implicit-progn ()
  (check-coalton-types
   "(define (f a)
      (let _a = (+ a 1))
      a)"

   '("f" . "(Num :a => :a -> :a)")))

(deftest test-returns ()
  (check-coalton-types
   "(define (f a)
      (return \"hello\")
      a)"

   '("f" . "(String -> String)"))

  (signals tc:tc-error
    (check-coalton-types
     "(define (f a)
        (return \"hello\")
        Unit)"))

  (signals tc:tc-error
    (check-coalton-types
     "(define x (return \"hello\"))")))

(deftest test-defaulting ()
  ;; See gh #505
  (check-coalton-types
   "(declare a (Num :a => :a -> :a))
    (define (a x)
      (let ((y 2))
        (+ x y)))"

   '("a" . "(Num :a => :a -> :a)"))

  ;; See gh #505
  (check-coalton-types
   "(declare f (:a -> :a))
    (define (f x)
      2
      x)"

   '("f" . "(:a -> :a)"))
  
  ;; Check that bindings aren't defaulted too early
  (check-coalton-types
   "(define (f _x)
      (let ((y 1))
        (+ 0.5 y)))"

   '("f" . "(:a -> Single-Float)"))

  ;; Check that the monomorphism restriction still applies to defaulted bindings
  (signals tc:tc-error
    (check-coalton-types
     "(define (f x)
      (let ((y 1))
        (+ 0.5f0 y)
        (+ 0.5d0 y)))"))

  ;; Check that ambiguous predicates are detected
  (signals tc:tc-error
    (check-coalton-types
     "(define (f x)
        (into (into x)))"))

  ;; Check that superclasses of Num are defaulted
  (check-coalton-types
   "(define x (even? 2))"

   '("x" . "Boolean")))
