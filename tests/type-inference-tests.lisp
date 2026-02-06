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

   '("f" . "(Integer -> Integer)")))

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

   '("InType" . "(:f (TFix :f) -> TFix :f)")))

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
   '("g" . "(Eq_ (List :a) => List :a -> :a -> Boolean)")))

(deftest test-typeclass-polymorphic-recursion ()
  ;; Check that polymorphic recursion is possible
  (check-coalton-types
   "(declare f (Eq :a => :a -> :a -> Boolean))
    (define (f a b)
      (if (== a b)
        True
        (f (singleton a) (singleton b))))"

   '("f" . "(Eq :a => :a -> :a -> Boolean)")))

(deftest test-typeclass-definition-constraints ()

  ;; Check that typeclass methods can constrain other variables
  (check-coalton-types
   "(define-class (Test :a)
      (test (Eq :b => :a -> :b)))"))

(deftest test-typeclass-additional-constraints ()

  ;; Check that typeclass methods can provide additional constraints
  (check-coalton-types
   "(define-class (Test :a)
      (test-bare (:a -> :a))
      (test-addl (Eq :a => :a -> :a)))

    ;; Can define on types without Eq
    (define-type TestType A B)

    (define-instance (Test TestType)
      (define test-bare id)
      (define test-addl id))

    ;; Can define generic function that uses Eq if constrained
    (declare test-generic ((Eq :a) (Test :a) => :a -> :a))
    (define test-generic test-addl)

    ;; Can use test-addl on TestType if Eq defined
    (define-instance (Eq TestType)
      (define (== x y)
        (match (Tuple x y)
          ((Tuple (A) (A)) True)
          ((Tuple (B) (B)) True)
          (_ False))))

    (declare test-addl-testtype (TestType -> TestType))
    (define test-addl-testtype test-addl)")

  ;; Check that methods not requiring additional constraints can be used
  (check-coalton-types
   "(define-class (Test :a)
      (test-bare (:a -> :a))
      (test-addl (Eq :a => :a -> :a)))

    ;; Can define on types without Eq
    (define-type TestType A B)

    (define-instance (Test TestType)
      (define test-bare id)
      (define test-addl id))

    (declare test-bare-testtype (TestType -> TestType))
    (define test-bare-testtype test-bare)")

  ;; Check that it works with functional dependencies
  (check-coalton-types
   "(define-type (Box :a)
      (Box :a))

    (define-class (ClassA :m :a (:m -> :a))
      (get-a (:m -> :a)))

    (define-class (ClassB :m :a (:m -> :a))
      (convert (ClassA :n :a => :n -> :m)))

    (define-instance (ClassA (Box :a) :a)
      (define (get-a (Box a)) a))

    (define-instance (ClassB (Box :a) :a)
      (define (convert bx)
        (Box (get-a bx))))"))

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

(deftest test-typeclass-cyclic-superclass-checks ()
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

(deftest test-weak-type-variables ()
  ;; See gh #84
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define f
        (let ((x (coalton-library/vector:new)))
          (fn (n)
            (coalton-library/vector:push! n x)
            x)))"))

  ;; Allocating in a function body remains polymorphic because each call
  ;; gets a fresh allocation site.
  (check-coalton-types
   "(define (mk _x)
      (let ((x (coalton-library/vector:new)))
        (fn (n)
          (coalton-library/vector:push! n x)
          x)))"

   '("mk" . "(:a -> :b -> Vector :b)")))

(deftest test-weak-type-variables-advanced ()
  ;; Relaxed value restriction: expansive expressions can still generalize
  ;; weak variables that occur only covariantly.
  (check-coalton-types
   "(define wrapped-id
      ((fn (x) x) None))"

   '("wrapped-id" . "(Optional :a)"))

  (check-coalton-types
   "(define-type (Cov :a)
      (Cov :a))
    (define cov-app
      ((fn (x) x) (Cov None)))"

   '("cov-app" . "(Cov (Optional :a))"))

  ;; Contravariant weak occurrences are still blocked.
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define-type (Contra :a)
        (Contra (:a -> Unit)))
      (define contra-app
        ((fn (x) x) (Contra (fn (_x) Unit))))"))

  ;; Opaque mutable containers are treated as invariant.
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define boxed-vec
        ((fn (x) x) (coalton-library/vector:new)))"))

  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define boxed-cell
        ((fn (x) x) (coalton-library/cell:new None)))"))

  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define boxed-queue
        ((fn (x) x) (coalton-library/queue:new)))"))

  ;; User-defined opaque native types default to invariant.
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(repr :native cl:t)
      (define-type (Opaque :a))
      (declare opaque-wrap (:a -> Opaque :a))
      (define (opaque-wrap x)
        (lisp (Opaque :a) (x) x))
      (define opaque-top
        ((fn (x) x) (opaque-wrap None)))"))

  ;; Constructor wrappers are only non-expansive when their arguments are.
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define wrapped-new
        (Some (coalton-library/vector:new)))"))

  (check-coalton-types
   "(define wrapped-none
      (Some None))"

   '("wrapped-none" . "(Optional (Optional :a))"))

  ;; Capturing a top-level mutable cell in an implicit binding is expansive.
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define stash
        (let ((c (coalton-library/cell:new None)))
          (fn (x)
            (coalton-library/cell:write! c (Some x))
            (coalton-library/cell:read c))))"))

  ;; Explicit type declarations still allow expansive bindings to be monomorphic.
  (check-coalton-types
   "(declare int-vec (Vector Integer))
    (define int-vec (coalton-library/vector:new))

    (define (push-int n)
      (coalton-library/vector:push! n int-vec))

    (define peek-int
      (fn ()
        (coalton-library/vector:index 0 int-vec)))"

   '("push-int" . "(Integer -> UFix)")
   '("peek-int" . "(Unit -> Optional Integer)"))

  (check-coalton-types
   "(declare int-queue (coalton-library/queue:Queue Integer))
    (define int-queue (coalton-library/queue:new))

    (define (push-int-queue n)
      (coalton-library/queue:push! n int-queue))

    (define pop-int-queue
      (fn ()
        (coalton-library/queue:pop! int-queue)))"

   '("push-int-queue" . "(Integer -> Unit)")
   '("pop-int-queue" . "(Unit -> Optional Integer)"))

  ;; A single inferred queue binding must not be usable at multiple element
  ;; types; Queue remains invariant for relaxed value restriction.
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define shared-queue (coalton-library/queue:new))

      (define (push-shared-int)
        (coalton-library/queue:push! 1 shared-queue))

      (define (push-shared-string)
        (coalton-library/queue:push! \"oops\" shared-queue))"))

  ;; Mutable allocations inside function bodies remain polymorphic per call.
  (check-coalton-types
   "(define (mk-stash _seed)
      (let ((c (coalton-library/cell:new None)))
        (fn (x)
          (coalton-library/cell:write! c (Some x))
          (coalton-library/cell:read c))))"

   '("mk-stash" . "(:a -> :b -> Optional :b)")))

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

   '("f" . "(String -> String)")))

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

   '("f" . "(:a -> F32)"))

  ;; Check that superclasses of Num are defaulted
  (check-coalton-types
   "(define x (even? 2))"

   '("x" . "Boolean")))


(deftest test-nameless-overapplication ()
  ;; See gh #1208
  (check-coalton-types
   "(define f (fn (x) (fn (y) (+ x y))))"

   '("f" . "(Num :a => :a -> :a -> :a)")))
