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

;; question
(coalton-toplevel
  (define-alias Stuff (List :a))

  ;; (declare first-element (Stuff -> :a))
  (declare first-element ((Stuff :a) -> :a))
  (define (first-element ))
  )

;; Two definition options:
(define-alias Stuff (List :a))

(define-alias (Stuff :a) (List :a))
