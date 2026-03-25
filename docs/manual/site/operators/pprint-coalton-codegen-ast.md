---
title: "pprint-coalton-codegen-ast"
description: "Pretty-print AST inspection output."
hideMeta: true
weight: 355
---

`pprint-coalton-codegen-ast` is the readable-display companion to
[`coalton-codegen-ast`](/manual/operators/coalton-codegen-ast/).

## Syntax

```lisp
(pprint-coalton-codegen-ast
  ⟨form⟩ ...)
```

## Semantics

- It pretty-prints AST inspection output instead of returning generated Lisp.
- Use it when investigating how Coalton parsed or lowered a definition before
  final code generation.

## Example

```lisp
(pprint-coalton-codegen-ast
  (define (double x)
    (+ x x)))
```
