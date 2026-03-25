---
title: "derive"
description: "Attribute for automatically generating type class instances."
hideMeta: true
weight: 45
---

`derive` is a toplevel attribute that asks Coalton to generate instances for a
`define-type` or `define-struct`.

## Syntax

```lisp
(derive ⟨class⟩...)
(define-type ...)
```

## Semantics

- `derive` must appear immediately before a compatible `define-type` or
  `define-struct`.
- It applies to the next definition, not to arbitrary expressions.
- Coalton only derives supported classes, and the definition's fields must
  supply the required instances.
- Multiple classes can be derived in one attribute.
- Supported classes: `Eq`, `Hash`, `Default`, `Show`

## Example

```lisp
(derive Eq Hash)
(define-struct Point
  (x UFix)
  (y UFix))
```

