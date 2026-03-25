---
title: "*"
description: "Type-level separator for multiple inputs and outputs."
hideMeta: true
weight: 35
---

`*` is a separator for function input and output types separated by `->`.

(The symbol `*` is also used in expressions for multiplication.)

## Syntax

```lisp
(⟨type-a⟩ * ⟨type-b⟩ * ... -> ⟨result-a⟩ * ⟨result-b⟩ * ...)
(lisp (-> ⟨type-a⟩ * ⟨type-b⟩ ...) ...)
```

## Semantics

- On the left of [`->`](/manual/operators/arrow/), `*` separates positional
  input types.
- On the right of `->`, `*` separates multiple return values.
- In a `lisp` form, it denotes that Lisp will return multiple values.

## Example

```lisp
(declare sum-and-product (Integer * Integer -> Integer * Integer))
```
