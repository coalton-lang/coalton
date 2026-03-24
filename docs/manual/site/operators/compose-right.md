---
title: ".<"
description: "Right-associative composition macro."
hideMeta: true
weight: 260
---

`.<` is a macro that builds a unary composed
function by composing its inputs from right to left.

## Syntax

```lisp
(.< ⟨expr⟩...)
```

## Semantics

- `(.< f g h)` expands to a unary function equivalent to
  `(fn (x) (f (g (h x))))`.
- It follows the same order as ordinary mathematical composition.
- Because it is a macro, it is syntax for building a function, not itself a
  first-class function value.
- It works in the opposite direction as `.>`.

## Example

```lisp
(.< show reverse sort)
```
