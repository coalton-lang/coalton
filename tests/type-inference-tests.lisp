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
   "(define f (fn (x y) x))
    (define g (f 5 \"str\"))
    (define h (f \"str\" 5))"

   '("f" . "(:a -> (:b -> :a))")
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
  (signals coalton-impl/typechecker2/base:tc-error
    (check-coalton-types
     "(define x (0 1))"))

  (signals coalton-impl/typechecker2/base:tc-error
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
       (fn (a) (f 5)))"

   '("f" . "(Num :a => :a -> :b)")))

(deftest test-explicit-type-declerations ()
  ;; Check that explicit declerations can reduce the type of a definition
  (check-coalton-types
   "(declare f Integer)
    (define f (undefined \"hello\"))"

   '("f" . "Integer"))

  ;; Declerations cannot be less specefic than their associated definition
  (signals coalton-impl/typechecker2/base:tc-error
    (check-coalton-types
     "(declare x :a)
      (define x Unit)"))

  ;; Missing explicit predicates cannot be defualted
  (signals coalton-impl/typechecker2/base:tc-error
    (check-coalton-types
     "(declare x :a)
      (define x 1)"))

  ;; Implicitly typed functions should only infer types from the declared type signature of an explicitly typed functions
  ;; http://jeremymikkola.com/posts/2019_01_12_type_inference_for_haskell_part_12.html
  (check-coalton-types
   "(declare lst (List Integer))
    (define lst (make-list 1 2 3))

    (define (a x) (singleton (b x)))

    (declare b (:a -> :a))
    (define (b y)
      (let ((foo (c 5)))
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
      (In (:f (TFix :f))))"

   '("In" . "(:f (TFix :f) -> TFix :f)"))

  ;; Check that constructors are properly typed
  (signals coalton-impl/typechecker2/base:tc-error
    (check-coalton-types
     "(define-type (Tree_ :a)
        (Leaf :a)
        (Branch (Tree_ :a) (Tree_ :a)))

      (define x (Branch (Leaf 5) (Leaf \"string\")))")))

(deftest test-kind-system ()
  ;; Check that types of kind * cannot be applied to
  (signals coalton-impl/typechecker2/base:tc-error
    (check-coalton-types
     "(declare x (Integer Integer))
      (define x (undefined Unit))"))

  ;; Check that variables can not be declared to have kind (* -> *)
  (signals coalton-impl/typechecker2/base:tc-error
    (check-coalton-types
     "(define-type (Maybe :a)
        (Just :a)
        Nothing)

      (declare x Maybe)
      (define x (undefined Unit))")))

(deftest test-pattern-invariants ()
  ;; Match branches must return the same type
  (signals coalton-impl/typechecker2/base:tc-error
    (check-coalton-types
     "(define-type (Maybe :a)
        (Just :a)
        Nothing)

      (define (f x)
        (match x
          ((Just 5) 5)
          ((Just 6) \"hello\")))"))

  ;; Match branches must match on constructors
  (signals coalton-impl/typechecker2/base:tc-error
    (check-coalton-types
     "(define (g x) x)

      (define (f x)
        (match x
          ((g a) 5)))"))

  ;; Constructors in match branches must be fully applied
  (signals coalton-impl/typechecker2/base:tc-error
    (check-coalton-types
     "(define (g x)
        (match x
          ((Cons x) x)))")))

(deftest test-monomorphism-restriction ()
  ;; Check that functions defined as a lambda are not subject to the
  ;; monomorphism restriction
  #+broken
  (check-coalton-types
   "(define-class (Show :a))

    (declare show (Show :a => :a -> String))
    (define (show x) \"not impl\")

    (define (f a)
      (show a))

    (define g
      (fn (b)
        (show b)))"

   '("f" . "(Show :a => :a -> String)")
   '("g" . "(Show :a => :a -> String")))

(deftest test-type-classes ()
  ;; Check that type constrains are propegated
  (check-coalton-types
   "(define (f a b) (== a b))"

   '("f" . "(Eq :a => :a -> :a -> Boolean)"))


  #+broken
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
   '("g" . "(Eq (List :a) => List :a -> :a -> Boolean"))


  #+broken
  (signals tc:coalton-type-error
    (run-coalton-typechecker
     '((coalton:define-class (Eq_ :a)
         (== (:a -> :a -> coalton:Boolean)))

       (coalton:define-instance (Eq_ :a => (Eq_ (coalton:List :a)))
         (coalton:define (== a b) coalton:False))

       (coalton:define-type Color Red Blue Green)

       (coalton:declare f ((coalton:List Color) -> coalton:Boolean))
       (coalton:define (f a b)
         (== a b))))))

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
  #+broken
  (signals tc:coalton-type-error
    (run-coalton-typechecker
     '((coalton:define-class (Eq_ :a)
        (== (:a -> :a -> coalton:Boolean)))

       (coalton:define-instance (Eq_ :a => (Eq_ (coalton:List :a)))
        (coalton:define (== a b) coalton:False))

       (coalton:define (f a b)
         (coalton:match (== a b)
           ((coalton:True) coalton:True)
           ((coalton:False)
            (f (coalton-prelude:singleton a)
               (coalton-prelude:singleton b)))))))))

#+broken
(deftest test-typeclass-definition-constraints ()
  ;; Check that typeclasses cannot have additional constrains defined in a method
  ;;
  ;; this is a stylistic decision and not a technical limitation
  (signals ast:coalton-parse-error
    (run-coalton-typechecker
    '((coalton:define-class (Test :a)
       (test (coalton-prelude::Eq :a => (:a -> :a)))))))

  ;; Check that typeclass methods can constrain other variables
  (run-coalton-typechecker
   '((coalton:define-class (Test :a)
      (test (coalton-prelude::Eq :b => (:a -> :b)))))))


#+broken
(deftest test-typeclass-flexible-instances ()
  (run-coalton-typechecker
   '((coalton:define-class (Eq_ :a)
       (== (:a -> :a -> coalton:Boolean)))

     (coalton:define-instance (Eq_ :a => (Eq_ (coalton-prelude:Tuple :a Integer)))
       (coalton:define (== a b) coalton:False))

     (coalton:define-instance (Eq_ Integer)
       (coalton:define (== a b) coalton:False))

     (coalton:declare f
      ((coalton-prelude:Tuple Integer Integer) ->
       (coalton-prelude:Tuple Integer Integer) ->
       coalton:Boolean))
     (coalton:define (f a b)
       (== a b)))))

#+broken
(deftest test-typeclass-overlapping-checks ()
  ;; Check than non overlapping instances can be defined
  (signals tc:overlapping-instance-error
    (run-coalton-typechecker
     '((coalton:define-class (Eq_ :a)
        (== (:a -> :a -> coalton:Boolean)))

       (coalton:define-instance (Eq_ :a => (Eq_ (coalton-prelude:Tuple :a Integer)))
        (coalton:define (== a b) coalton:False))

       (coalton:define-instance (Eq_ :a => (Eq_ (coalton-prelude:Tuple String :a)))
        (coalton:define (== a b) coalton:False))))))

#+broken
(deftest test-typeclass-cyclic-superclass-checks ()
  (signals tc:cyclic-class-definitions-error
    (run-coalton-typechecker
     '((coalton:define-class ((TestClassA :a) => (TestClassB :a)))
       (coalton:define-class ((TestClassB :a) => (TestClassA :a))))))

  (signals tc:cyclic-class-definitions-error
    (run-coalton-typechecker
     '((coalton:define-class (TestClassB :b)
        (example-method ((TestClassA :a) => (:a -> :b))))
       (coalton:define-class ((TestClassB :a) => (TestClassA :a))))))

  ;; NOTE: This is allowed in Haskell 98
  (signals tc:cyclic-class-definitions-error
    (run-coalton-typechecker
     '((coalton:define-class (TestClassA :a)
        (example-method ((TestClassA :b) => (:a -> :b))))))))

#+broken
(deftest test-hkt ()
  (check-coalton-types
   '((coalton:define-class (Functor :f)
      (fmap ((:a -> :b) -> (:f :a) -> (:f :b))))

     (coalton:define-instance (Functor coalton:List)
      (coalton:define fmap coalton-prelude::undefined))
     (coalton:define-instance (Functor (coalton-prelude:Tuple :a))
      (coalton:define fmap coalton-prelude::undefined))

     (coalton:define xs (coalton:make-list 1 2 3))

     (coalton:declare print-int (Integer -> String))
     (coalton:define (print-int x)
       "")

     (coalton:define ys (fmap print-int xs)))

   '((ys . (coalton:List String)))))

#+broken
(deftest test-seq ()
  (check-coalton-types
   '((coalton:define-class (Show :a))

     (coalton:declare show (Show :a => (:a -> String)))
     (coalton:define (show x) "not impl")

     (coalton:define (f x)
       (coalton::seq
        (coalton-prelude:Ok "hello")
        (coalton-prelude:map (coalton-prelude:+ 1) (coalton:make-list 1 2 3 4))
        (show x))))
   '((f . (Show :a => (:a -> String))))))


(deftest test-the ()
  (check-coalton-types
   "(define (f a b)
      ((the (Integer -> Integer -> Boolean) ==) a b))"

   '("f" . "(Integer -> Integer -> Boolean)"))

  #+broken
  (check-coalton-types
   '((coalton:define (f a b)
       ((coalton:the ((coalton-prelude:Eq :a) => (:a -> :a -> coalton:Boolean)) coalton-prelude:==)
        a b)))
   '((f . ((coalton-prelude:Eq :a) => (:a -> :a -> coalton:Boolean)))))

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
  #+broken
  (check-coalton-types
   '((coalton:define f (fn () 5)))
   '((f . (coalton-library/classes:Num :a => coalton:Unit -> :a)))))

(deftest test-function-implicit-progn ()
  (check-coalton-types
   "(define (f a)
      (let a_ = (+ a 1))
      a)"

   '("f" . "(Num :a => :a -> :a)")))

(deftest test-returns ()
  (check-coalton-types
   "(define (f a)
      (return \"hello\")
      a)"

   '("f" . "(String -> String)"))

  (signals coalton-impl/typechecker2/base:tc-error
    (check-coalton-types
     "(define (f a)
        (return \"hello\")
        Unit)"))

  (signals coalton-impl/typechecker2/base:tc-error
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
      (traceObject \"2\" 2)
      x)"

   '("f" . "(:a -> :a)"))
  
  ;; Check that bindings aren't defaulted too early
  (check-coalton-types
   "(define (f x)
      (let ((y 1))
        (+ 0.5 y)))"

   '("f" . "(:a -> Double-Float)"))

  ;; Check that the monomorphism restriction still applies to defaulted bindings
  (signals coalton-impl/typechecker2/base:tc-error
    (check-coalton-types
     "(define (f x)
      (let ((y 1))
        (+ 0.5f0 y)
        (+ 0.5d0 y)))"))

  ;; Check that ambigious predicates are detected
  (signals coalton-impl/typechecker2/base:tc-error
    (check-coalton-types
     "(define (f x)
        (into (into x)))"))

  ;; Check that superclasses of Num are defaulted
  (check-coalton-types
   "(define x (even? 2))"

   '("x" . "Boolean")))

  #+ignore
(deftest test-bind ()
  (check-coalton-types
   '((coalton:define x
       (coalton::bind x 5 (coalton-prelude:+ x 1))))
   '((x . Integer)))

  (check-coalton-types
   '((coalton:define (f x)
       (coalton::bind x (coalton-prelude:+ x 1) x)))
   '((f . (coalton-prelude:Num :a => :a -> :a))))

  (signals tc:coalton-type-error
    (run-coalton-typechecker
     '((coalton:define _
         (coalton::bind id (coalton:fn (x) x)
                        (coalton::seq
                         (id coalton:Unit)
                         (id "hello"))))))))
