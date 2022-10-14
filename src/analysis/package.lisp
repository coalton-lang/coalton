(uiop:define-package #:coalton-impl/analysis
  (:documentation "Static analysis passes for COALTON.")
  (:mix-reexport
   #:coalton-impl/analysis/pattern-exhaustiveness
   #:coalton-impl/analysis/analysis))
