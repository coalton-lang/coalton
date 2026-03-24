---
title: "pprint-coalton-codegen-types"
description: "Pretty-print typed generated Lisp."
hideMeta: true
weight: 350
---

`pprint-coalton-codegen-types` pretty-prints the output of
[`coalton-codegen-types`](/manual/operators/coalton-codegen-types/).

## Syntax

```lisp
(pprint-coalton-codegen-types
  ⟨form⟩ ...)
```

## Semantics

- It shows generated Lisp together with emitted host type declarations.
- This is useful when checking whether Coalton is emitting the declarations you
  expect.

## Example

```lisp
(pprint-coalton-codegen-types
  (define (double x)
    (+ x x)))
```
