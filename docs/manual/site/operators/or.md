---
title: "or"
description: "Short-circuiting disjunction."
hideMeta: true
weight: 240
---

`or` is a short-circuiting Boolean macro.

## Syntax

```lisp
(or ⟨expr⟩...)
```

## Semantics

- Arguments are evaluated left to right.
- Evaluation stops as soon as `True` is reached.
- Use `boolean-or` when you need an ordinary function instead.

## Example

```lisp
(or cached? cheap? forced?)
```
