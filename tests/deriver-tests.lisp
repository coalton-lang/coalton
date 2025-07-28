(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (derive Eq)
  (define-type (DerivingThing :a)
    Nothing
    (Something :a))

  (derive Eq Default)
  (define-struct (DerivingPerson :a)
    (age U8)
    (name String)
    (things (vector:Vector (DerivingThing :a)))))

(coalton-toplevel
  (derive Eq)
  (define-type (DeriveTree :t)
    DeriveLeaf
    (DeriveNode :t (DeriveTree :t) (DeriveTree :t))))

(coalton-toplevel
  (derive Eq Hash)
  (define-type A
    A0
    (An B))

  (derive Eq Hash)
  (define-type B
    B0
    (Bn A)))

(coalton-toplevel
  (derive Eq Hash)
  (define-struct HashablePerson
    (age U8)
    (name String)))

(coalton-toplevel
  (derive Eq Default)
  (define-type DefaultableFoo (DFoo UFix))
  (derive Eq Default)
  (define-type DefaultableUnit DUnit))

(define-test derive-basic-test ()
  "Ensure `Eq' can be derived for structs and types."
  (is (== Nothing (The (DerivingThing U8) Nothing)))
  (is (/= Nothing (Something 12)))
  (is (== (Something "hi") (Something "hi")))
  (is (== (Something 12) (Something 12)))

  (is (== (DerivingPerson 1 "a" (vector:make (Something "computer")))
          (DerivingPerson 1 "a" (vector:make (Something "computer")))))
  (is (/= (DerivingPerson 1 "a" (vector:make (Something "computer")))
          (DerivingPerson 1 "a" (vector:make (Something "sardine"))))))

(define-test derive-default-test ()
  "Ensure `Default' is derived correctly for types with a single constructor."
  (is (== (the (DerivingPerson String) (DerivingPerson 0 "" (vector:make)))
          (default)))
  (is (== (DFoo 0) (default)))
  (is (== DUnit (Default))))

(define-test derive-recursive-test ()
  "Ensure deriving works for single recursive types."
  (is (== (the (DeriveTree UFix) DeriveLeaf) DeriveLeaf))
  (is (/= DeriveLeaf (the (DeriveTree UFix) (DeriveNode 1 DeriveLeaf DeriveLeaf))))
  (is (== (the (DeriveTree UFix) (DeriveNode 1 DeriveLeaf DeriveLeaf)) (DeriveNode 1 DeriveLeaf DeriveLeaf))))

(define-test derive-mutually-recursive-test ()
  "Ensure deriving works for mutually recursive types."
  (is (== B0 B0))
  (is (== (Bn (An B0)) (Bn (An B0))))
  (is (/= B0 (Bn (An B0)))))

(define-test derive-two-classes ()
  "Ensure we can derive multiple classes at once."
  (is (== (hash (HashablePerson 1 "a"))
          (hash (HashablePerson 1 "a"))))
  (is (/= (hash (HashablePerson 2 "a"))
          (hash (HashablePerson 3 "a"))))
  (is (== (hash (Bn A0)) (hash (Bn A0))))
  (is (/= (hash A0) (hash (An B0)))))

(define-test derive-hash-usage ()
  "A little demo of using derived Hash to make a table."
  (let map = (the (hashmap:HashMap HashablePerson UFix) hashmap:empty))
  (let map* = (hashmap:insert map (HashablePerson 1 "a") 1))
  (is (== (Some 1) (hashmap:lookup map* (HashablePerson 1 "a")))))

(in-package #:coalton-tests)

(uiop:define-package #:coalton-tests/deriver-tests
  (:use #:coalton #:coalton-library/classes))

(deftest derive-eq-for-invalid-type-test ()
  "Ensure we can't compile unsound code using derived methods."
  (check-coalton-types
   "(define-type UnEqAble UnEqAbleThing)
    (derive Eq)
    (define-type (MaybeEqAble :a) (MaybeEqAbleThing :a))
    (derive Eq)
    (define-type NotEqAble (NotEqAbleThing UnEqAble))")
  (signals coalton-impl/typechecker:tc-error 
    (check-coalton-types
     "(define-type UnEqAble UnEqAbleThing)
      (derive Eq)
      (define-type (MaybeEqAble :a) (MaybeEqAbleThing :a))
      (define (definitely-not-eqable)
        (== (MaybeEqAbleThing UnEqAbleThing) (MaybeEqAbleThing UnEqAbleThing)))"))
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define-type UnEqAble UnEqAbleThing)
      (derive Eq)
      (define-type NotEqAble (NotEqAbleThing UnEqAble))
      (define (definitely-not-eqable-2)
        (== (NotEqAbleThing UnEqAbleThing) (NotEqAbleThing UnEqAbleThing)))")))
