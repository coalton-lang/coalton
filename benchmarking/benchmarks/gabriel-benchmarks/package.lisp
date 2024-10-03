(uiop:define-package #:coalton-benchmarks/gabriel
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-benchmarking)
  (:mix-reexport
   #:coalton-benchmarks/gabriel/tak
   #:coalton-benchmarks/gabriel/takr
   #:coalton-benchmarks/gabriel/stak
   #:coalton-benchmarks/gabriel/takl)
  (:export
   #:run-gabriel-benchmarks))

(in-package #:coalton-benchmarks/gabriel)

(coalton-toplevel

  (define (run-gabriel-benchmarks)
    (run-package-benchmarks "coalton-benchmarks/gabriel/tak")
    (run-package-benchmarks "coalton-benchmarks/gabriel/takr")
    (run-package-benchmarks "coalton-benchmarks/gabriel/stak")
    (run-package-benchmarks "coalton-benchmarks/gabriel/takl")))
