---
title: ".>"
description: "Left-associative composition macro."
hideMeta: true
weight: 270
---

`.>` is an expression macro that composes one or more unary functions in left-to-right (pipeline) execution order.

## Syntax

```lisp
(.> ⟨fn-expr⟩+)
```

## Semantics

- Requires at least one `⟨fn-expr⟩`.
- `(.> f1 f2 ... fn)` expands to `(fn (x) (pipe x f1 f2 ... fn))`, which further expands to `(fn (x) (fn (... (f2 (f1 x)))))` for a fresh parameter `x`.
- When the resulting function is applied to an argument `x`, `f1` is called first on `x`, `f2` is called on `(f1 x)`, and so on through `fn`.
- With a single argument, `(.> f)` expands to `(fn (x) (f x))`.
- Because `.>` is a macro rather than an ordinary function, it can compose an arbitrary number of stages into a single lambda without allocating intermediate composed closures, and cannot itself be passed unapplied as a higher-order function value.

## Examples

```lisp
(coalton
  (map (.> ƒx.(+ x 1)
           ƒx.(* x x)
           show-as-string)
       (the (List Integer) [1 2 3 4])))
; => ("4" "9" "16" "25")
```

## Other notes

- `.>` is the point-free counterpart of [`pipe`](/manual/operators/pipe/), and composes functions in the opposite order of [`.<`](/manual/operators/compose-right/) and the standard library function `compose`.
- When you need a first-class binary composition function value `(B -> C) * (A -> B) -> (A -> C)`, use `compose` from the standard library.

