---
title: "pprint-coalton-codegen-ast"
description: "Pretty-print AST inspection output."
hideMeta: true
weight: 355
---

`pprint-coalton-codegen-ast` is the pretty-printing wrapper around [`coalton-codegen-ast`](/manual/operators/coalton-codegen-ast/).

## Syntax

```lisp
(pprint-coalton-codegen-ast
  ⟨toplevel-form⟩*)
```

## Semantics

- `pprint-coalton-codegen-ast` accepts the same top-level forms as [`coalton-toplevel`](/manual/operators/coalton-toplevel/).
- It runs [`coalton-codegen-ast`](/manual/operators/coalton-codegen-ast/) inside a temporary package with local symbols imported to print the typechecked AST (`coalton-impl/typechecker/expression:node-body`) for each top-level `define` to `*standard-output*` (followed by `NIL`), and returns zero values `(cl:values)`.
- Use it when investigating how Coalton parsed, renamed, and typechecked a definition before dictionary translation and Common Lisp code generation.

## Examples

```lisp
(pprint-coalton-codegen-ast
  (declare double (Integer -> Integer))
  (define (double x)
    (+ x x)))
```

## Other Notes

- See [`coalton-codegen-ast`](/manual/operators/coalton-codegen-ast/), [`pprint-coalton-codegen`](/manual/operators/pprint-coalton-codegen/), and [`pprint-coalton-codegen-types`](/manual/operators/pprint-coalton-codegen-types/).

