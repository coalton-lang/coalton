(in-package #:coalton)

;; Define "faux applications" in the package `#:coalton' that are
;; treated as identity functions but are used by codegen to apply
;; optimizations.

(coalton-toplevel
  (declare inline (:a -> :a))
  (define (inline application)
    "Try to inline `application'."
    application)

  (declare noinline (:a -> :a))
  (define (noinline application)
    "Prevent `application' from being inlined."
    application)

  (declare likely (Boolean -> Boolean))
  (define (likely predicate)
    "Hint to the compiler that `predicate' is likely `True'."
    predicate)

  (declare unlikely (Boolean -> Boolean))
  (define (unlikely predicate)
    "Hint to the compiler that `predicate' is likely `False'."
    predicate))
