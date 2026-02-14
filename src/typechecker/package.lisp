(uiop:define-package #:coalton-impl/typechecker
  (:documentation "The Coalton type checker implements Hindley-Milner type inference
extended with type classes (following 'Typing Haskell in Haskell' by Mark P. Jones).

The typechecker takes an untyped parse tree and produces a typed AST annotated
with inferred types, resolved type class dictionaries, and specialization
information. Key subsystems include:

- Type inference (define.lisp) via constraint generation and unification
- Kind inference (parse-type.lisp) for higher-kinded types
- Type class resolution (context-reduction.lisp, define-class.lisp, define-instance.lisp)
- Functional dependencies (fundeps.lisp) for multi-parameter type classes
- Specialization (specialize.lisp) for performance-critical type class dispatch
- Variance checking (variance.lisp) for type parameters
- Environment management (environment.lisp, tc-env.lisp) for the global type database")
  (:mix-reexport
   #:coalton-impl/typechecker/stage-1
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/pattern
   #:coalton-impl/typechecker/expression
   #:coalton-impl/typechecker/traverse
   #:coalton-impl/typechecker/toplevel
   #:coalton-impl/typechecker/binding
   #:coalton-impl/typechecker/accessor
   #:coalton-impl/typechecker/partial-type-env
   #:coalton-impl/typechecker/parse-type
   #:coalton-impl/typechecker/variance
   #:coalton-impl/typechecker/define-type
   #:coalton-impl/typechecker/derive
   #:coalton-impl/typechecker/define-class
   #:coalton-impl/typechecker/tc-env
   #:coalton-impl/typechecker/define
   #:coalton-impl/typechecker/define-instance
   #:coalton-impl/typechecker/specialize
   #:coalton-impl/typechecker/translation-unit))
