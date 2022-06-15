(cl:in-package #:coalton)

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(coalton-toplevel
  (repr :enum)
  (define-type Unit Unit)

  (repr :native cl:t)
  (define-type Void)

  ;; Boolean is an early type
  (declare True Boolean)
  (define True (lisp Boolean ()  cl:t))

  (declare False Boolean)
  (define False (lisp Boolean ()  cl:nil))

  ;; List is an early type
  (declare Cons (:a -> (List :a) -> (List :a)))
  (define (Cons x xs)
    (lisp (List :a) (x xs)
      (cl:cons x xs)))

  (declare Nil (List :a))
  (define Nil
    (lisp (List :a) ()
      cl:nil)))
