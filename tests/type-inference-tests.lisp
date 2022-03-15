;;;; type-inference-tests.lisp

(in-package #:coalton-tests)

(deftest test-type-inference ()
  (check-coalton-types
   '((coalton:define f 5))
   '((f . Integer)))
  (check-coalton-types
   '((coalton:define f (fn (x) x))
     (coalton:define g (f 5)))
   '((f . (:a -> :a))
     (g . Integer)))
  (check-coalton-types
   '((coalton:define f (fn (x y) x))
     (coalton:define g (f 5 "str"))
     (coalton:define h (f "str" 5)))
   '((f . (:a -> (:b -> :a)))
     (g . Integer)
     (h . String)))

  ;; Check that identity qualifies
  (check-coalton-types
   '((coalton:define (id a) a)
     (coalton:define x (id 3))
     (coalton:define y (id "three")))
   '((id . (:a -> :a))
     (x . Integer)
     (y . String)))

  ;; Check that let bindings are polymorphic over kinds
  (check-coalton-types
   '((coalton:define x
       (coalton:let ((id (fn (a) a)))
         ((id id) 5))))
   '((x . Integer)))

  ;; Check that let bindings can have explicit types
  (check-coalton-types
   '((coalton:define (f x)
       (coalton:let ((coalton:declare g (Integer -> Integer))
             (g coalton-prelude:id))
         (g x))))
   '((f . (Integer -> Integer))))

  ;; Check that you can only call callable things
  (signals coalton-impl::coalton-type-error
    (run-coalton-typechecker
     '((coalton:define x (0 1)))))
  (signals coalton-impl::coalton-type-error
    (run-coalton-typechecker
     '((coalton:define x 5)
       (coalton:define y (x 1))))))

(deftest test-recursive-type-inference ()
  ;; Check mutually recursive definitions
  (check-coalton-types
   '((coalton:define (f a) (g a))
     (coalton:define (g b) (f b)))
   '((f . (:a -> :b))
     (g . (:a -> :b))))

  ;; Check unusual recursive definitions
  (check-coalton-types
   '((coalton:define f
       (fn (a) (f a))))
   '((f . (:a -> :b))))

  (check-coalton-types
   '((coalton:define f
       (fn (a) (f 5))))
   '((f . (Integer -> :a)))))

(deftest test-explicit-type-declerations ()
  ;; Check that explicit declerations can reduce the type of a definition
  (check-coalton-types
   '((coalton:declare f Integer)
     (coalton:define f (coalton-prelude:undefined "hello")))
   '((f . Integer)))

  ;; Declerations cannot be less specefic than their associated definition
  (signals coalton-impl::coalton-type-error
    (run-coalton-typechecker
     '((coalton:declare x :a)
       (coalton:define x 5))))

  ;; Implicitly typed functions should only infer types from the declared type signature of an explicitly typed functions
  ;; http://jeremymikkola.com/posts/2019_01_12_type_inference_for_haskell_part_12.html
  (check-coalton-types
   '((coalton:declare lst (coalton:List Integer))
     (coalton:define lst (coalton:make-list 1 2 3))

     (coalton:define (a x)
       (coalton-prelude:singleton (b x)))

     (coalton:declare b (:a -> :a))
     (coalton:define (b y)
       (coalton:let ((foo (c 5)))
         y))

     (coalton:define (c z)
       (coalton-prelude:append lst (a z))))

   '((a . (:a -> (coalton:List :a))))))

(deftest test-type-definitions ()
  ;; Test recursive type definitions
  (check-coalton-types
   '((coalton:define-type (Tree :a)
       (Leaf :a) (Branch (Tree :a) (Tree :a)))

     (coalton:define (f a)
       (Branch a (f a))))

   '((Leaf . (:a -> (Tree :a)))
     (Branch . ((Tree :a) -> ((Tree :a) -> (Tree :a))))
     (f . ((Tree :a) -> (Tree :a)))))

  ;; Check mutually recursive type definitions
  (check-coalton-types
   '((coalton:define-type (A :a)
       (A (B :a)))

     (coalton:define-type (B :a)
       (B (A :a))))
   '())

  ;; Check higher kinded type variables
  (check-coalton-types
   '((coalton:define-type (Fix :f)
       (In (:f (Fix :f)))))
   '((In . (:f (Fix :f) -> Fix :f))))

  ;; Check that constructors are properly typed
  (signals coalton-impl::coalton-type-error
    (run-coalton-typechecker
     '((coalton:define-type (Tree :a)
         (Leaf :a) (Branch (Tree :a) (Tree :a)))

       (coalton:define x
         (Branch (Leaf 5) (Leaf "string")))))))

(deftest test-kind-system ()
  ;; Check that types of kind * cannot be applied to
  (signals coalton-impl::coalton-parse-error
    (run-coalton-typechecker
     '((coalton:declare x (Integer Integer))
       (coalton:define x x))
     ))

  ;; Check that variables can not be declared to have kind (* -> *)
  (signals coalton-impl::coalton-type-error
    (run-coalton-typechecker
     '((coalton:define-type (Maybe :a)
         (Just :a)
         (Nothing))
       (coalton:declare x Maybe)
       (coalton:define x x)))))

(deftest test-pattern-invariants ()
  ;; Match branches must return the same type
  (signals coalton-impl::coalton-type-error
    (run-coalton-typechecker
     '((coalton:define-type (Maybe :a)
         (Just :a)
         (Nothing))

       (coalton:define (f x)
         (coalton:match x
           ((Just 5) 5)
           ((Just 6) "hello"))))))

  ;; Match branches must match the same type
  (signals coalton-impl::coalton-type-error
    (run-coalton-typechecker
     '((coalton:define-type (Maybe :a)
         (Just :a)
         (Nothing))

       (coalton:define (f x)
         (coalton:match x
           ((Just 5) Nothing)
           ((Just "hello") Nothing))))))

  ;; Match branches must match on constructors
  (signals coalton-impl/typechecker::coalton-type-error
    (run-coalton-typechecker
     '((coalton:define (g x) x)

       (coalton:define (f x)
         (coalton:match x
           ((g a) 5))))))

  ;; Constructors in match branches must be fully applied
  (signals coalton-impl::coalton-type-error
    (run-coalton-typechecker
     '((coalton:define (g x)
         (coalton:match x
           ((coalton:Cons x) x)))))))

(deftest test-monomorphism-restriction ()
  ;; Check that functions defined as a lambda are not subject to the
  ;; monomorphism restriction
  (check-coalton-types
   '((coalton:define-class (Show :a))

     (coalton:declare show (Show :a => (:a -> String)))
     (coalton:define (show x) "not impl")

     (coalton:define (f a)
       (show a))

     (coalton:define g
       (fn (b)
         (show b))))
   '((f . (Show :a => (:a -> String)))
     (g . (Show :a => (:a -> String))))))

(deftest test-type-classes ()
  ;; Check that type constrains are propegated
  (check-coalton-types
   '((coalton:define (f a b) (coalton-prelude::== a b)))

   '((f . (coalton-prelude::Eq :a => (:a -> :a -> coalton:Boolean)))))


  (check-coalton-types
   '((coalton:define-class (Eq_ :a)
      (== (:a -> :a -> coalton:Boolean)))

     (coalton:define-instance (Eq_ :a => (Eq_ (coalton:List :a)))
      (coalton:define (== a b) coalton:False))

     (coalton:define-type Color Red Green Blue)

     (coalton:define-instance (Eq_ Color)
      (coalton:define (== a b) coalton:False))

     (coalton:define a (== Red Green))

     (coalton:define (g x y)
       (== x (coalton-prelude:singleton y)))

     (coalton:define (h x y)
       (== (coalton-prelude:singleton x) (coalton-prelude:singleton y))))

   '((a . coalton:Boolean)
     (g . (Eq_ :a => ((coalton:List :a) -> :a -> coalton:Boolean)))
     (h . (Eq_ :a => (:a -> :a -> coalton:Boolean)))))


  (signals coalton-impl::coalton-type-error
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
   '((coalton:define-class (Eq_ :a)
      (== (:a -> :a -> coalton:Boolean)))

     (coalton:define-instance (Eq_ :a => (Eq_ (coalton:List :a)))
      (coalton:define (== a b) coalton:False))

     (coalton:declare f (Eq_ :a => (:a -> :a -> coalton:Boolean)))
     (coalton:define (f a b)
       (coalton:match (== a b)
         ((coalton:True) coalton:True)
         ((coalton:False)
          (f (coalton-prelude:singleton a)
             (coalton-prelude:singleton b))))))
   '((f . (Eq_ :a => (:a -> :a -> coalton:Boolean)))))

  ;; Check that polymorphic recursion is not possible without an explicit binding
  (signals coalton-impl::coalton-type-error
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

(deftest test-typeclass-definition-constraints ()
  ;; Check that typeclasses cannot have additional constrains defined in a method
  ;;
  ;; this is a stylistic decision and not a technical limitation
  (signals coalton-impl::coalton-parse-error
    (run-coalton-typechecker
    '((coalton:define-class (Test :a)
       (test (coalton-prelude::Eq :a => (:a -> :a)))))))

  ;; Check that typeclass methods can constrain other variables
  (run-coalton-typechecker
   '((coalton:define-class (Test :a)
      (test (coalton-prelude::Eq :b => (:a -> :b)))))))


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

(deftest test-typeclass-overlapping-checks ()
  ;; Check than non overlapping instances can be defined
  (signals coalton-impl::overlapping-instance-error
    (run-coalton-typechecker
     '((coalton:define-class (Eq_ :a)
        (== (:a -> :a -> coalton:Boolean)))

       (coalton:define-instance (Eq_ :a => (Eq_ (coalton-prelude:Tuple :a Integer)))
        (coalton:define (== a b) coalton:False))

       (coalton:define-instance (Eq_ :a => (Eq_ (coalton-prelude:Tuple String :a)))
        (coalton:define (== a b) coalton:False))))))

(deftest test-typeclass-cyclic-superclass-checks ()
  (signals coalton-impl::cyclic-class-definitions-error
    (run-coalton-typechecker
     '((coalton:define-class ((TestClassA :a) => (TestClassB :a)))
       (coalton:define-class ((TestClassB :a) => (TestClassA :a))))))

  (signals coalton-impl::cyclic-class-definitions-error
    (run-coalton-typechecker
     '((coalton:define-class (TestClassB :b)
        (example-method ((TestClassA :a) => (:a -> :b))))
       (coalton:define-class ((TestClassB :a) => (TestClassA :a))))))

  ;; NOTE: This is allowed in Haskell 98
  (signals coalton-impl::cyclic-class-definitions-error
    (run-coalton-typechecker
     '((coalton:define-class (TestClassA :a)
        (example-method ((TestClassA :b) => (:a -> :b))))))))

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

(deftest test-seq ()
  (check-coalton-types
   '((coalton:define-class (Show :a))

     (coalton:declare show (Show :a => (:a -> String)))
     (coalton:define (show x) "not impl")

     (coalton:define (f x)
       (coalton:seq
        (coalton-prelude:Ok "hello")
        (coalton-prelude:map (coalton-prelude:+ 1) (coalton:make-list 1 2 3 4))
        (show x))))
   '((f . (Show :a => (:a -> String))))))


(deftest test-the ()
  (check-coalton-types
   '((coalton:define (f a b)
       ((coalton:the (coalton:Integer -> coalton:Integer -> coalton:Boolean) coalton-prelude:==)
        a b)))
   '((f . (coalton:Integer -> coalton:Integer -> coalton:Boolean))))

  (check-coalton-types
   '((coalton:define (f a b)
       ((coalton:the ((coalton-prelude:Eq :a) => (:a -> :a -> coalton:Boolean)) coalton-prelude:==)
        a b)))
   '((f . ((coalton-prelude:Eq :a) => (:a -> :a -> coalton:Boolean)))))

  (check-coalton-types
   '((coalton:define x (coalton:the coalton:U32 (coalton-prelude:+ (coalton-prelude:fromInt 1)
                                                                   (coalton-prelude:fromInt 2)))))
   '((x . coalton:U32))))

(deftest test-regression ()
  ;; Fixed in #283
  (check-coalton-types
   '((coalton:define (f a)
       (coalton:let ((g (coalton:fn (x) (coalton-prelude:Tuple x a))))
         (g 5))))
   '((f . (:a -> (coalton-prelude:Tuple Integer :a))))))

(deftest test-function-definition-shorthand ()
  (check-coalton-types
   '((coalton:define f (fn () 5)))
   '((f . (coalton:Unit -> coalton:Integer)))))

(deftest test-function-implicit-progn ()
  (check-coalton-types
   '((coalton:define (f a)
       (coalton:let a_ coalton:= (coalton-prelude:+ a 1))
       a_))
   nil))
