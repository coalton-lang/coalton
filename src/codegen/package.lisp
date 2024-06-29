(uiop:define-package #:coalton-impl/codegen
  (:import-from
   #:coalton-impl/codegen/codegen-expression
   #:codegen-expression)
  (:export
   #:codegen-expression)

  (:import-from
   #:coalton-impl/codegen/transformations
   #:make-function-table)
  (:export
   #:make-function-table)

  (:import-from
   #:coalton-impl/codegen/optimizer
   #:optimize-node
   #:direct-application
   #:inline-applications)
  (:export
   #:optimize-node
   #:direct-application
   #:inline-applications)

  (:import-from
   #:coalton-impl/codegen/translate-expression
   #:translate-expression)
  (:export
   #:translate-expression)

  (:import-from
   #:coalton-impl/codegen/program
   #:compile-translation-unit)
  (:export
   #:compile-translation-unit))
