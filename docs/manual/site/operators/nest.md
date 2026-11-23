---
title: "nest"
description: "Nested application macro."
hideMeta: true
weight: 320
---

`nest` is a right-to-left nested function application macro that flattens deeply nested unary calls.

## Syntax

```lisp
(nest ⟨fn-expr⟩+ ⟨init-expr⟩)
```

## Semantics

- Requires at least two subforms (at least one `⟨fn-expr⟩` followed by `⟨init-expr⟩`).
- `(nest f1 f2 ... fn x)` expands to `(f1 (f2 (... (fn x))))`.
- Each `⟨fn-expr⟩` must evaluate to a unary function applied to the result of the subexpression to its right.
- Because `nest` expands directly into nested function applications at macroexpansion time, it introduces no intermediate closures or runtime overhead.

## Examples

```lisp
(coalton
  (nest show-as-string
        sum
        (fn (xs) (map ƒx.(* x x) xs))
        (the (List Integer) [1 2 3 4])))
; => "30"
```

## Other notes

- `nest` writes stages in standard mathematical order (outermost function first, initial argument last), whereas [`pipe`](/manual/operators/pipe/) writes the same pipeline in execution order (initial argument first, outermost function last).
- To build a reusable unary function in right-to-left order without supplying the final argument `x`, use [`.<`](/manual/operators/compose-right/).

