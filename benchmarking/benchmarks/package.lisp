;;;; package.lisp
;;;;
;;;; Benchmarks packages and common functions

(uiop:define-package #:coalton-benchmarks
  (:use #:coalton
        #:coalton-prelude
        #:coalton-benchmarking)
  (:mix-reexport
   #:coalton-benchmarks/fibonacci
   #:coalton-benchmarks/big-float
   #:coalton-benchmarks/gabriel)
  (:export
   #:run-coalton-benchmarks))

(in-package #:coalton-benchmarks)

(coalton-toplevel

  (define (%run-coalton-benchmarks)
    (run-package-benchmarks "coalton-benchmarks/fibonacci")
    (run-package-benchmarks "coalton-benchmarks/big-float")
    (run-gabriel-benchmarks)))

(cl:defun run-coalton-benchmarks ()
  (coalton (%run-coalton-benchmarks)))
