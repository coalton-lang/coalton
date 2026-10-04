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
- Expressions in the body are sequenced as an implicit `progn`. Any results
  are discarded; the `when` expression returns `Void` (zero values).
- A final body expression that already returns `Void` retains tail position
  when the `when` expression is in tail position, including calls in `rec`.
- Use `unless` for the negated form.

## Example

```lisp
(when verbose?
  (show "starting"))
```
