---
title: "do"
description: "Monadic sequencing form."
hideMeta: true
weight: 260
---

`do` is Coalton's monadic sequencing form.

## Syntax

```lisp
(do
  ⟨clause⟩...)

;; ⟨clause⟩ := ⟨expr⟩
;;           | (⟨var⟩ <- ⟨expr⟩)
```

## Semantics

- `do` is surface syntax for sequencing monadic operations.
- Use [`<-`](/manual/operators/do-bind/) to bind the result of an action.
- Ordinary local bindings can also appear inside a `do` block.
- Values are usually returned with `pure`.

## Example

```lisp
(do
  (a <- ax)
  (b <- bx)
  (pure (+ a b)))
```
