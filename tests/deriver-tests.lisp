(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (derive Eq)
  (define-type (DerivingThing :a)
    Nothing
    (Something :a))

  (derive Eq)
  (define-struct (DerivingPerson :a)
    (age U8)
    (name String)
    (things (vector:Vector (DerivingThing :a)))))

(define-test derive-basic-test ()
  "Ensure `Eq' can be derived for structs and types."
  (is (== Nothing (The (DerivingThing U8) Nothing)))
  (is (not (== Nothing (Something 12))))
  (is (== (Something "hi") (Something "hi")))
  (is (== (Something 12) (Something 12)))

  (is (== (DerivingPerson 1 "a" (vector:make (Something "computer")))
          (DerivingPerson 1 "a" (vector:make (Something "computer")))))
  (is (not (== (DerivingPerson 1 "a" (vector:make (Something "computer")))
               (DerivingPerson 1 "a" (vector:make (Something "sardine")))))))


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
