---
title: "and"
description: "Short-circuiting conjunction."
hideMeta: true
weight: 230
---

`and` is a short-circuiting Boolean macro.

## Syntax

```lisp
(and ⟨expr⟩ ...)
```

## Semantics

- Arguments are evaluated left to right.
- Evaluation stops as soon as a `False` value is reached.
- Use `boolean-and` when you need an ordinary function instead.

## Example

```lisp
(and open? writable? nonempty?)
```
