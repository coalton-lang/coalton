---
title: "unlikely"
description: "Branch-likelihood hint for `Boolean` expressions."
hideMeta: true
weight: 90
---

`unlikely` is a branch-probability hint indicating that a `Boolean` expression is expected to evaluate to `False`.

## Syntax

```lisp
(unlikely ⟨boolean-expr⟩)
```

## Semantics

- `unlikely` is a function of type `(Boolean -> Boolean)` that returns the value of `⟨boolean-expr⟩` unchanged.
- It annotates conditional tests (such as in [`if`](/manual/operators/if/), [`cond`](/manual/operators/cond/), [`when`](/manual/operators/when/), or [`unless`](/manual/operators/unless/)) to indicate that the `True` branch is a rare or exceptional path.
- It does not alter program semantics or evaluation order.

## Examples

```lisp
(coalton-toplevel
  (declare checked-step (Integer -> Integer))
  (define (checked-step n)
    (when (unlikely (< n 0))
      (error "negative step count"))
    (+ n 1)))
```

## Other notes

- `unlikely` is defined as an identity function on `Boolean` in `coalton` (`src/intrinsic-applications.lisp`); the compiler does not currently emit host branch-prediction hints for it.
- Use [`likely`](/manual/operators/likely/) for the complementary hint when a condition is expected to be `True`.

