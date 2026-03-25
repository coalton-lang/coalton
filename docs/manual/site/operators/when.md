---
title: "when"
description: "Conditional sequencing form."
hideMeta: true
weight: 210
---

`when` conditionally runs its body when the test is `True`.

## Syntax

```lisp
(when ⟨test⟩
  ⟨expr⟩...)
```

## Semantics

- `when` is a convenience form for one-sided conditional code for effect.
- Expressions in the body are sequenced as an implicit `progn`, and the
  result must unify to `Void`.
- Use `unless` for the negated form.

## Example

```lisp
(when verbose?
  (show "starting"))
```
