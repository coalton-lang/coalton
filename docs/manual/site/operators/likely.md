---
title: "likely"
description: "Branch-likelihood hint for `Boolean` expressions."
hideMeta: true
weight: 85
---

`likely` is a branch-probability hint indicating that a `Boolean` expression is expected to evaluate to `True`.

## Syntax

```lisp
(likely ⟨boolean-expr⟩)
```

## Semantics

- `likely` is a function of type `(Boolean -> Boolean)` that returns the value of `⟨boolean-expr⟩` unchanged.
- It annotates conditional tests (such as in [`if`](/manual/operators/if/), [`cond`](/manual/operators/cond/), [`when`](/manual/operators/when/), or [`unless`](/manual/operators/unless/)) to indicate that the `True` branch is the common fast path.
- It does not alter program semantics or evaluation order.

## Examples

```lisp
(coalton-toplevel
  (declare get-or-compute ((Optional Integer) * (Void -> Integer) -> Integer))
  (define (get-or-compute cached compute)
    (if (likely (some? cached))
        (unwrap cached)
        (compute))))
```

## Other notes

- `likely` is defined as an identity function on `Boolean` in `coalton` (`src/intrinsic-applications.lisp`); the compiler does not currently emit host branch-prediction hints for it.
- Use [`unlikely`](/manual/operators/unlikely/) for the complementary hint when a condition is expected to be `False`.

