---
title: "coalton-codegen-ast"
description: "Dump the Coalton AST for generated forms."
hideMeta: true
weight: 340
---

`coalton-codegen-ast` compiles top-level Coalton forms, prints the typechecked AST for each top-level [`define`](/manual/operators/define/) to `*standard-output*`, and returns `nil`.

## Syntax

```lisp
(coalton-codegen-ast
  ⟨toplevel-form⟩*)
```

## Semantics

- `coalton-codegen-ast` accepts the same top-level forms as [`coalton-toplevel`](/manual/operators/coalton-toplevel/).
- During compilation, it invokes `*codegen-hook*` with `:AST` right after typechecking and before dictionary translation and optimization.
- For each top-level `define` binding in the unit, it prints `⟨name⟩ :: ⟨type-scheme⟩` followed by the readable structure of its typechecked body (`coalton-impl/typechecker/expression:node-body`) to `*standard-output*`.
- Unlike [`coalton-codegen`](/manual/operators/coalton-codegen/), it prints during evaluation and returns `nil`.

## Examples

```lisp
(coalton-codegen-ast
  (declare double (Integer -> Integer))
  (define (double x)
    (+ x x)))
;; prints:
;; DOUBLE :: INTEGER → INTEGER
;; #S(NODE-BODY ...)
; => NIL
```

## Other notes

- See [`pprint-coalton-codegen-ast`](/manual/operators/pprint-coalton-codegen-ast/), [`coalton-codegen`](/manual/operators/coalton-codegen/), and [`coalton-codegen-types`](/manual/operators/coalton-codegen-types/).
- To look up the post-optimization codegen AST (`coalton-impl/codegen/ast:node`) of an already-defined top-level symbol in the global environment, call `(coalton:lookup-code 'name)`.


