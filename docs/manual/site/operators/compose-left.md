---
title: ".>"
description: "Left-associative composition macro."
hideMeta: true
weight: 270
---

`.>` is a macro that builds a unary composed function by composing its inputs
from left to right.

## Syntax

```lisp
(.> ⟨expr⟩...)
```

## Semantics

- `(.> f g h)` expands to a unary function equivalent to
  `(fn (x) (h (g (f x))))`.
- It is the composition-only counterpart of the `pipe` macro.
- Because it is a macro, it is syntax for creating a function rather than a
  higher-order function you can pass around unexpanded.
- It works in the opposite direction as `.<`.

## Example

```lisp
(.> tokenize normalize index)
```
