---
title: "let*"
description: "Sequential lexical binding form."
hideMeta: true
weight: 170
---

`let*` introduces lexical bindings sequentially from top to bottom.

## Syntax

```lisp
(let* (⟨binding-or-declare⟩*)
  ⟨body-element⟩*
  ⟨expr⟩)

;; ⟨binding-or-declare⟩ := (⟨var⟩ ⟨init-expr⟩)
;;                       | (declare ⟨var⟩ ⟨qualified-type⟩)
```

## Semantics

- `let*` evaluates and binds `(⟨var⟩ ⟨init-expr⟩)` clauses strictly in top-to-bottom source order.
- Each `⟨init-expr⟩` sees bindings introduced earlier in the `let*` list, but does not see its own binder or later binders (bindings in `let*` are non-recursive).
- Duplicate variable names within the same `let*` binding list are rejected at compile time.
- Each `(⟨var⟩ ⟨init-expr⟩)` must produce a single value; to destructure patterns or multiple values sequentially in a body, use shorthand `(let ⟨pattern⟩ = ⟨expr⟩)` or `(let (values ...) = ⟨expr⟩)` (see [`let`](/manual/operators/let/)).
- Local [`declare`](/manual/operators/declare/) forms in the binding list constrain the named bindings.
- The body is an implicit [`progn`](/manual/operators/progn/) (supporting multiple expressions and shorthand `let` bindings), and its final expression is in tail position when the `let*` is in tail position.

## Examples

```lisp
(coalton
  (let* ((declare x Integer)
         (x 10)
         (y (* x 2))
         (z (+ x y)))
    z))
; => 30
```

## Other Notes

- Non-escaping local functions bound by `let*` are declared `dynamic-extent`, allowing the Lisp compiler to stack-allocate their closures.
- In release mode, a locally bound `coalton/cell:Cell` created with `cell:new` that does not escape its lexical scope is unboxed into a typed mutable lexical variable with no heap allocation. See [Performance and Optimization](/manual/topics/performance-and-optimization/).
- Use [`let`](/manual/operators/let/) when defining recursive or mutually recursive local functions.

