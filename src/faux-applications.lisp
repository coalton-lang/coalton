(in-package :coalton)

;; Define "faux applications" in the package `#:coalton' that are
;; treated as identity functions but are used by codegen to apply
;; optimizations.

(coalton-toplevel
  (define (inline application)
    "Try to inline `application'."
    application)

  (define (noinline application)
    "Prevent `application' from being inlined."
    application)

  (define (likely branch)
    "Not implemented."
    branch)

  (define (unlikely branch)
    "Not implemented."
    branch))
