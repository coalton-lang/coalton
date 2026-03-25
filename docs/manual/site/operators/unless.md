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
- Expressions in the body are sequenced as an implicit `progn`, and the
  result must unify to `Void`.
- Use `when` for the negated form.

## Example

```lisp
(unless ready?
  (error "Not ready"))
```
