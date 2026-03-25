---
title: "progn"
description: "Flattened sequencing form."
hideMeta: true
weight: 255
---

`progn` is a sequencing form for flattened expression bodies.

## Syntax

```lisp
(progn
  ⟨expr⟩ ...)
```

## Semantics

- Expressions are evaluated in order.
- The final expression determines the result and type.
- Inside `progn`, Coalton also supports short-`let` syntax.
- Non-final expressions may produce zero values without requiring an explicit
  `let (values) = ...`.

## Example

```lisp
(progn
  (+ x y)
  (* x y)
  (values x y))
```
