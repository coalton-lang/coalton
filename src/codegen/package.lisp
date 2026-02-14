(uiop:define-package #:coalton-impl/codegen
  (:documentation "The Coalton code generator translates typed AST into Common Lisp forms.

The pipeline: typed AST -> codegen AST -> optimization -> CL code generation.

Key stages:
- Translation (translate-expression.lisp): converts typechecker AST to codegen AST,
  inserting type class dictionary passing and constructor applications.
- Optimization (optimizer.lisp): inlining, constant folding, direct application,
  match compilation, and tail-call optimization.
- Monomorphization (monomorphize.lisp): specializes polymorphic functions for
  concrete types, eliminating dictionary-passing overhead.
- Code generation (codegen-expression.lisp): emits Common Lisp forms from the
  optimized codegen AST.")
  (:import-from
   #:coalton-impl/codegen/codegen-expression
   #:codegen-expression)
  (:export
   #:codegen-expression)

  (:import-from
   #:coalton-impl/codegen/optimizer
   #:make-function-table
   #:optimize-node
   #:direct-application)
  (:export
   #:make-function-table
   #:optimize-node
   #:direct-application)

  (:import-from
   #:coalton-impl/codegen/translate-expression
   #:translate-expression)
  (:export
   #:translate-expression)

  (:import-from
   #:coalton-impl/codegen/program
   #:compile-translation-unit
   #:*codegen-hook*)
  (:export
   #:compile-translation-unit
   #:*codegen-hook*)

  (:import-from
   #:coalton-impl/codegen/intrinsic-applications
   #:transform-intrinsic-applications)
  (:export
   #:transform-intrinsic-applications))
