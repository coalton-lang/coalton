(uiop:define-package #:coalton-impl/parser
  (:documentation "The Coalton parser transforms s-expressions into a typed AST.

Parsing proceeds in two phases: first, Coalton source forms (from COALTON-TOPLEVEL
or the Coalton reader) are parsed into an untyped CST (concrete syntax tree).
Then the renamer resolves names and produces the final parse tree that the
typechecker consumes.

Key concepts:
- Expressions, patterns, and types each have their own node types.
- Top-level forms (define, define-type, define-class, etc.) are parsed by toplevel.lisp.
- The collect protocol provides generic traversal for gathering sub-nodes.
- The binding and type-definition protocols abstract over definition forms.")
  (:mix-reexport
   #:coalton-impl/parser/base
   #:coalton-impl/parser/reader
   #:coalton-impl/parser/types
   #:coalton-impl/parser/pattern
   #:coalton-impl/parser/expression
   #:coalton-impl/parser/toplevel
   #:coalton-impl/parser/collect
   #:coalton-impl/parser/renamer
   #:coalton-impl/parser/binding
   #:coalton-impl/parser/type-definition))
