(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-alias Index UFix
    "Index aliases UFix")

  (declare index-incr (Index -> Index))
  (define (index-incr i)
    (1+ i)))

(define-test test-alias-definition ()
  (is (== (index-incr 3)
          4)))

(coalton-toplevel
  (define-alias (Stuff :a) (List :a))

  (declare first-element ((Stuff :a) -> :a))
  (define (first-element s)
    (list:head s)))

(define-test test-alias-parametric ()
  (let assorted = (the (Stuff Integer) (make-list 1 0 -1)))
  (is (== (first-element assorted)
          1)))
