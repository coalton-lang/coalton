---
title: "coalton-codegen-types"
description: "Inspect generated Lisp with host type declarations."
hideMeta: true
weight: 335
---

`coalton-codegen-types` is the typed variant of
[`coalton-codegen`](/manual/operators/coalton-codegen/).

## Syntax

```lisp
(coalton-codegen-types
  ⟨toplevel-form⟩ ...)
```

## Semantics

- It compiles Coalton forms and returns the generated Lisp code.
- Unlike `coalton-codegen`, it includes emitted Lisp type declarations.
- This is useful when debugging host-compiler interop and generated declarations.

## Example

```lisp
(coalton-codegen-types
  (define (double x)
    (+ x x)))
```
