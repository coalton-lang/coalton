---
title: "coalton-codegen-ast"
description: "Dump the Coalton AST for generated forms."
hideMeta: true
weight: 340
---

`coalton-codegen-ast` exposes the intermediate AST rather than the final Lisp.

## Syntax

```lisp
(coalton-codegen-ast
  ⟨toplevel-form⟩ ...)
```

## Semantics

- It prints AST information for the toplevel definitions in the given forms.
- It is intended for compiler debugging and internal investigation.
- Unlike the other codegen macros, it returns `NIL` after printing.

## Example

```lisp
(coalton-codegen-ast
  (define (double x)
    (+ x x)))
```
