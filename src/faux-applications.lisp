(in-package :coalton)

;; Define "faux applications" in the package `#:coalton' that are
;; treated as identity functions but are used by codegen to apply
;; optimizations.

(coalton-toplevel
  (define (inline   x) x)
  (define (noinline x) x)
  (define (likely   x) x)
  (define (unlikely x) x))
