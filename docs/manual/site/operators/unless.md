---
title: "unless"
description: "Negated conditional sequencing form."
hideMeta: true
weight: 220
---

`unless` evaluates its body for side effects when a `Boolean` test is `False`, and returns `Void` (zero values).

## Syntax

```lisp
(unless ⟨test-expr⟩
  ⟨body-element⟩*
  ⟨expr⟩)
```

## Semantics

- `⟨test-expr⟩` must have type `Boolean`, and at least one body expression is required.
- If `⟨test-expr⟩` evaluates to `False`, the body is evaluated sequentially as an implicit [`progn`](/manual/operators/progn/) (supporting multiple expressions and shorthand [`let`](/manual/operators/let/) bindings).
- If `⟨test-expr⟩` evaluates to `True`, the body is not evaluated.
- Any value(s) produced by the final `⟨expr⟩` of the body are discarded; the `unless` expression always has type `Void` (zero values).
- When the final body expression already has type `Void`, it remains in tail position if the `unless` expression is in tail position (including for [`rec`](/manual/operators/rec/) tail calls).

## Examples

```lisp
(coalton-toplevel
  (declare checked-divide (Integer * Integer -> (Optional Integer)))
  (define (checked-divide a b)
    (unless (/= b 0)
      (return None))
    (Some (coalton/math:div a b))))
```

## Other Notes

- Because `unless` returns `Void` (zero values) rather than `Unit`, a function declared `(Foo -> Unit)` that ends with an `unless` form should follow `unless` with `Unit` (or be declared `(Foo -> Void)`).
- Use [`when`](/manual/operators/when/) for the positive counterpart.

