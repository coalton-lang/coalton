(in-package #:coalton-library)

;;;;
;;;; This file is used to break a cyclic dependency between type
;;;; definitions and their usage in classes.lisp. Other types should
;;;; be defined in their respective files.
;;;;

(coalton-toplevel

  (define-type Unit Unit)

  ;; Boolean is an early type
  (define True (lisp Boolean ()  cl:t))
  (define False (lisp Boolean ()  cl:nil))


  ;; List is an early type
  (declare Cons (:a -> (List :a) -> (List :a)))
  (define (Cons x xs)
    (lisp (List :a) (x xs)
      (cl:cons x xs)))

  (declare Nil (List :a))
  (define Nil
    (lisp (List :a) ()
      cl:nil))

  (define-type (Tuple :a :b)
    "A heterogeneous collection of items."
    (Tuple :a :b))

  (define-type (Optional :a)
    "Represents something that may not have a value."
    (Some :a)
    None)

  (define-type (Result :bad :good)
    "Represents something that may have failed."
    ;; We write (Result :bad :good) instead of (Result :good :bad)
    ;; because of the limitations of how we deal with higher-kinded
    ;; types; we want to implement Functor on this.
    (Ok :good)
    (Err :bad))

  (define-type Fraction
    "A ratio of integers always in reduced form."
    ;; We avoid "Rational" or "Ratio" since those might be a more
    ;; generic concept than a humble fraction of integers. This
    ;; fraction is always assumed to be in reduced terms.
    ;;
    ;; This shouldn't be pattern matched against with user code.
    ;;
    ;; See arith.lisp for more info.
    (%Fraction Integer Integer)))
