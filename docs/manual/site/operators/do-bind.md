---
title: "<-"
description: "Monadic bind syntax in `do` notation."
hideMeta: true
weight: 60
---

`<-` appears inside `do` notation. It runs a monadic action and binds its inner
value to a name for the rest of the block.

## Syntax

```lisp
(do
  ...
  (⟨var⟩ <- ⟨expr⟩)
  ...)
```

## Semantics

- `(⟨var⟩ <- ⟨expr⟩)` is `do` notation for a monadic bind.
- Under the hood it expands to a monadic bind using the standard library.
- The bound name is in scope for the remaining forms in the block.

## Example

```lisp
(do
  (a <- ax)
  (b <- bx)
  (pure (+ a b)))
```
