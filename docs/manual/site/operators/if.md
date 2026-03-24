---
title: "if"
description: "Boolean conditional form."
hideMeta: true
weight: 200
---

`if` is a two-way Boolean conditional.

## Syntax

```lisp
(if ⟨test-expr⟩ ⟨then-expr⟩ ⟨else-expr⟩)
```

## Semantics

- `⟨test-expr⟩` must be a Coalton `Boolean`.
- `if` must always have a consequent and alternative.
- For multi-armed conditionals, use `cond`.
- For structural branching on algebraic data, use `match`.
- Stylistically, `if` should only be used for pure computations;
  side-effectful computations should always use `cond`.
- It is not recommended to use `progn` with `if`.

## Example

```lisp
(if (zero? n) 0 1)
```
