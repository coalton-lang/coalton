---
title: "the"
description: "Inline type annotation."
hideMeta: true
weight: 100
---

`the` declares the type of an expression.

## Syntax

```lisp
(the ⟨type⟩ ⟨expr⟩)
```

## Semantics

- `the` is the standard way to resolve ambiguity in an expression.
- It is especially useful around numeric literals, collection builders,
  sequence builders, and conversions.
- Unlike `declare`, `the` works directly at expression sites.
- `the` accepts explicit `forall` binders; when present, those type variables
  are scoped over the enclosed expression.

## Example

```lisp
(the (List F32) [1 2 3 4])
```
