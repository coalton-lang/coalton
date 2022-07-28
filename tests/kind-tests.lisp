
(in-package :coalton-native-tests)

(coalton-toplevel

  (repr :native (cl:or cl:list cl:symbol))
  (declare-type Expr *)
  (define-type Expr)

  (declare check-kind (Expr -> Expr -> Boolean))
  (define (check-kind type kind)
    (lisp boolean (type kind)
      (to-boolean
       (coalton-impl/typechecker::kind=
        (coalton-impl/typechecker:parse-kind
         coalton-impl::*global-environment* kind)
        (kind-of type)))))

  (declare-type Fixed ((* -> *) -> *))
  (repr :transparent)
  (define-type (Fixed :f)
    (In (:f (Fixed :f))))

  (declare-type Proxy (:a -> *))
  (define-type (Proxy :f)
    Proxy))

(cl:defmacro the-kind (kind type)
  `(check-kind
    (lisp Expr () (cl:quote ,type))
    (lisp Expr () (cl:quote ,kind))))

(define-test kind-check ()
  (is (the-kind (:b -> *) Proxy))
  (is (the-kind (* -> *) Proxy))

  (is (the-kind ((* -> *) -> *) Fixed))
  (is (not (the-kind (* -> * -> *) Fixed)))

  (is (the-kind (*) Expr))
  (is (not (the-kind (* -> *) Expr)))

  (is (not (the-kind (* -> *) Arrow)))
  (is (the-kind (* -> * -> *) Arrow))
  (is (the-kind (* -> :b) Arrow))
  (is (the-kind (:a -> :b) Arrow))
  (is (not (the-kind (:a -> :a) Arrow))))

(in-package #:coalton-tests)

(deftest kind-errors ()
  ;; Check for mismatch types
  (signals coalton-impl::coalton-type-error
    (eval
     '(coalton:coalton-toplevel
       (coalton:declare-type Fixed (* -> *))
       (coalton:define-type (Fixed :f)
         (In (:f (Fixed :f)))))))

  ;; Looser mismatch
  (signals error
    (eval
     '(coalton:coalton-toplevel
       (coalton:declare-type Fixed :a)
       (coalton:define-type (Fixed :f)
         (In (:f (Fixed :f)))))))

  ;; Arity mismatch
  (signals coalton-impl::coalton-parse-error
    (eval
     '(coalton:coalton-toplevel
       (coalton:declare-type Unit (* -> *))
       (coalton:define-type Unit Unit))))

  ;; Check for orphans
  (signals error
    (eval
     '(coalton:coalton-toplevel
       (coalton:declare-type Orphan (* -> *)))))

  ;; Kind mismatch
  (signals coalton-impl::unification-error
    (eval
     '(coalton:coalton-toplevel
       (coalton:declare-type Proxy2 (:a -> :a -> *))
       (coalton:define-type (Proxy2 :a :b) Proxy2)

       (coalton:declare p (Proxy2 coalton:Integer coalton:List))
       (coalton:define p Proxy2)))))
