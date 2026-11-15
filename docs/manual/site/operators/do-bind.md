---
title: "<-"
description: "Monadic bind syntax in `do` notation."
hideMeta: true
weight: 60
---

`<-` is the monadic bind operator inside [`do`](/manual/operators/do/) notation, binding a pattern to the value produced by a monadic action.

## Syntax

```lisp
(do
  ...
  (⟨pattern⟩ <- ⟨monadic-expr⟩)
  ...
  ⟨final-expr⟩)
```

## Semantics

- `(⟨pattern⟩ <- ⟨monadic-expr⟩)` is valid only as a non-final clause of a [`do`](/manual/operators/do/) expression; a `do` block cannot end with a `<-` clause.
- `⟨monadic-expr⟩` must have type `(:m :a)`, where `:m` is the `Monad` type constructor of the enclosing `do` block, and `⟨pattern⟩` must have type `:a`.
- `⟨pattern⟩` may be a variable, [`_`](/manual/operators/wildcard/), a constructor pattern (such as `(Tuple x y)`), or an [`=`](/manual/operators/pattern-bind/) alias pattern.
- The clause desugars to a call to `coalton/classes:>>=`:
  ```lisp
  (>>= ⟨monadic-expr⟩
       (fn (G)
         (match G
           (⟨pattern⟩ ⟨remaining-do-body⟩))))
  ```
- Variables bound by `⟨pattern⟩` are in scope for all subsequent clauses and `⟨final-expr⟩` in the `do` block.
- [`return`](/manual/operators/return/) is not permitted inside `⟨monadic-expr⟩`.

## Examples

```lisp
;; Binding variables and destructuring a Tuple inside `do`
(coalton
  (do
    (x <- (Some (the Integer 10)))
    ((Tuple y z) <- (Some (Tuple 20 12)))
    (pure (+ x (+ y z)))))
; => 42
```

## Other Notes

- For pure (non-monadic) bindings inside a `do` block, use `(let ⟨pattern⟩ = ⟨expr⟩)` instead of `<-`.
- See [`do`](/manual/operators/do/) and [`need`](/manual/operators/need/).

