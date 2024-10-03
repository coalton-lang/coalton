(uiop:define-package #:coalton-benchmarks/gabriel
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-benchmarking)
  (:mix-reexport
   #:coalton-benchmarks/gabriel/tak
   #:coalton-benchmarks/gabriel/takr
   #:coalton-benchmarks/gabriel/stak
   #:coalton-benchmarks/gabriel/takl))

(in-package #:coalton-benchmarks/gabriel)

(reexport-benchmarks
   "coalton-benchmarks/gabriel/tak"
   "coalton-benchmarks/gabriel/takr"
   "coalton-benchmarks/gabriel/stak"
   "coalton-benchmarks/gabriel/takl")
