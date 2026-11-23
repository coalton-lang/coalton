---
title: ".<"
description: "Right-associative composition macro."
hideMeta: true
weight: 260
---

`.<` is an expression macro that composes one or more unary functions in right-to-left (mathematical) order.

## Syntax

```lisp
(.< ⟨fn-expr⟩+)
```

## Semantics

- Requires at least one `⟨fn-expr⟩`.
- `(.< f1 f2 ... fn)` expands to `(fn (x) (nest f1 f2 ... fn x))`, which further expands to `(fn (x) (f1 (f2 (... (fn x)))))` for a fresh parameter `x`.
- When the resulting function is applied to an argument `x`, `fn` is called first on `x`, and `f1` is called last.
- With a single argument, `(.< f)` expands to `(fn (x) (f x))`.
- Because `.<` is a macro rather than an ordinary function, it can compose an arbitrary number of stages into a single lambda without allocating intermediate composed closures, and cannot itself be passed unapplied as a higher-order function value.

## Examples

```lisp
(coalton
  (map (.< show-as-string
           ƒx.(* x x)
           ƒx.(+ x 1))
       (the (List Integer) [1 2 3 4])))
; => ("4" "9" "16" "25")
```

## Other notes

- `.<` is the point-free counterpart of [`nest`](/manual/operators/nest/), and composes functions in the same right-to-left order as the standard library binary function `compose` (and the opposite order of [`.>`](/manual/operators/compose-left/)).
- When you need a first-class binary composition function value `(B -> C) * (A -> B) -> (A -> C)`, use `compose` from the standard library.

