---
title: "when"
description: "Conditional sequencing form."
hideMeta: true
weight: 210
---

`when` evaluates its body for side effects when a `Boolean` test is `True`, and returns `Void` (zero values).

## Syntax

```lisp
(when ⟨test-expr⟩
  ⟨body-element⟩*
  ⟨expr⟩)
```

## Semantics

- `⟨test-expr⟩` must have type `Boolean`, and at least one body expression is required.
- If `⟨test-expr⟩` evaluates to `True`, the body is evaluated sequentially as an implicit [`progn`](/manual/operators/progn/) (supporting multiple expressions and shorthand [`let`](/manual/operators/let/) bindings).
- If `⟨test-expr⟩` evaluates to `False`, the body is not evaluated.
- Any value(s) produced by the final `⟨expr⟩` of the body are discarded; the `when` expression always has type `Void` (zero values).
- When the final body expression already has type `Void`, it remains in tail position if the `when` expression is in tail position (including for [`rec`](/manual/operators/rec/) tail calls).

## Examples

```lisp
(coalton-toplevel
  ;; Guard an early return inside a function
  (declare first-positive ((List Integer) -> (Optional Integer)))
  (define (first-positive xs)
    (for ((rem xs (match rem ((Cons _ tl) tl) ((Nil) Nil))))
      :while (not (coalton/list:null? rem))
      (match rem
        ((Cons x _)
         (when (> x 0)
           (return (Some x))))
        ((Nil) (values))))
    None))
```

## Other notes

- Because `when` returns `Void` (zero values) rather than `Unit`, a function declared `(Foo -> Unit)` that ends with a `when` form should follow `when` with `Unit` (or be declared `(Foo -> Void)`).
- Use [`unless`](/manual/operators/unless/) for the negated test `(when (not test) ...)`.

