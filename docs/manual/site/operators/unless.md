---
title: "unless"
description: "Negated conditional sequencing form."
hideMeta: true
weight: 220
---

`unless` is the negated counterpart to `when`.

## Syntax

```lisp
(unless ⟨test⟩
  ⟨expr⟩...)
```

## Semantics

- `unless` is a convenience form for one-sided conditional code for effect.
- Expressions in the body are sequenced as an implicit `progn`. Any results
  are discarded; the `unless` expression returns `Void` (zero values).
- A final body expression that already returns `Void` retains tail position
  when the `unless` expression is in tail position, including calls in `rec`.
- Use `when` for the negated form.

## Example

```lisp
(unless ready?
  (error "Not ready"))
```
