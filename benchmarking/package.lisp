(uiop:define-package #:coalton-benchmarking
    (:use #:coalton
          #:coalton-prelude)
    (:mix-reexport
     #:coalton-benchmarking/printing
     #:coalton-benchmarking/benchmarking))
