---
title: "coalton-codegen"
description: "Inspect generated Lisp for Coalton forms."
hideMeta: true
weight: 330
---

`coalton-codegen` expands Coalton forms into generated Lisp for inspection.

## Syntax

```lisp
(coalton-codegen
  ⟨toplevel-form⟩ ...)
```

## Semantics

- It accepts the same kind of body you would give to `coalton-toplevel`.
- The expansion returns generated Lisp code without host Lisp type
  declarations.
- Use it when debugging, especially when something is behaving unexpectedly.

## Example

```lisp
(coalton-codegen
  (define (double x)
    (+ x x)))
```
