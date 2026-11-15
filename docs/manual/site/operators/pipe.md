---
title: "pipe"
description: "Threading macro."
hideMeta: true
weight: 310
---

`pipe` is a left-to-right threading macro that passes an initial value through a sequence of unary functions.

## Syntax

```lisp
(pipe ⟨init-expr⟩ ⟨fn-expr⟩+)
```

## Semantics

- Requires at least two subforms (`⟨init-expr⟩` and at least one `⟨fn-expr⟩`).
- `(pipe x f1 f2 ... fn)` expands to `(fn (... (f2 (f1 x))))` (equivalent to `(nest fn ... f2 f1 x)`).
- Each `⟨fn-expr⟩` must be an expression of unary function type (such as a unary function name, an [`fn`](/manual/operators/fn/) or [`ƒ`](/manual/operators/short-fn/) lambda, or a unary conversion macro `(as T)`), applied to the result of the preceding step.
- Because `pipe` is a purely syntactic macro expansion into nested function calls, it introduces no intermediate closures or runtime overhead.

## Examples

```lisp
(coalton
  (pipe (the (List Integer) [3 1 4 1 5])
        reverse
        (fn (xs) (map ƒx.(* x 2) xs))
        sum))
; => 28
```

## Other Notes

- `pipe` is the data-first counterpart of [`nest`](/manual/operators/nest/), which writes the same functions in outside-in (right-to-left) order.
- To build a reusable unary function from a left-to-right pipeline without supplying the initial value `x` yet, use [`.>`](/manual/operators/compose-left/).

