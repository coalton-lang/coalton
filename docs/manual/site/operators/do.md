---
title: "do"
description: "Monadic sequencing form."
hideMeta: true
weight: 260
---

`do` sequences monadic computations using `Monad` bind (`>>=`) and local bindings.

## Syntax

```lisp
(do
  ⟨do-element⟩*
  ⟨final-expr⟩)

;; ⟨do-element⟩ := (⟨pattern⟩ <- ⟨monadic-expr⟩)
;;               | (let ⟨pattern⟩ = ⟨pure-expr⟩)
;;               | (let (values ⟨var-or-_⟩*) = ⟨multi-value-expr⟩)
;;               | (let *⟨dynamic-var⟩* = ⟨expr⟩)
;;               | ⟨monadic-expr⟩
```

## Semantics

- A `do` block requires at least one form and must end with a `⟨final-expr⟩` (it cannot end with a [`<-`](/manual/operators/do-bind/) bind or a shorthand [`let`](/manual/operators/let/)).
- The entire `do` expression has type `(:m :result)` for some type constructor `:m` implementing `Monad :m`, where `(:m :result)` is the type of `⟨final-expr⟩`.
- A GADT pattern supplies its index equalities to the remaining clauses and final expression, just as it would in the corresponding `match` arm. It does not change the type of the input computation.
- Clauses before `⟨final-expr⟩` are translated from top to bottom as follows:
  - **Monadic bind `(⟨pattern⟩ <- ⟨monadic-expr⟩)`**:
    `⟨monadic-expr⟩` must have type `(:m :a)` for the same monad `:m`. Desugars to:
    ```lisp
    (>>= ⟨monadic-expr⟩
         (fn (G)
           (match G
             (⟨pattern⟩ ⟨rest⟩))))
    ```
  - **Unbound monadic action `⟨monadic-expr⟩`**:
    An ordinary expression before the final form must have type `(:m :a)` for the same monad `:m`; its inner value `:a` is ignored. Desugars to:
    ```lisp
    (>>= ⟨monadic-expr⟩
         (fn (_)
           ⟨rest⟩))
    ```
  - **Shorthand local bindings `(let ⟨pattern⟩ = ⟨pure-expr⟩)` and `(let (values ...) = ⟨multi-value-expr⟩)`**:
    Bind pure values locally for the remainder of the `do` block without invoking `>>=`.
  - **Shorthand dynamic binding `(let *⟨dynamic-var⟩* = ⟨expr⟩)`**:
    Rebinds `*⟨dynamic-var⟩*` via [`dynamic-bind`](/manual/operators/dynamic-bind/) for the remainder of the `do` block.
- [`return`](/manual/operators/return/) cannot appear inside the right-hand side `⟨monadic-expr⟩` of a `(⟨pattern⟩ <- ⟨monadic-expr⟩)` clause.

## Examples

```lisp
(coalton-toplevel
  ;; Sequencing Optional computations with pattern destructuring and pure `let`
  (declare sum-first-two ((List (Tuple Integer Integer)) -> (Optional Integer)))
  (define (sum-first-two pairs)
    (do
      ((Tuple x1 y1) <- (coalton/list:head pairs))
      (rest <- (coalton/list:tail pairs))
      ((Tuple x2 y2) <- (coalton/list:head rest))
      (let total = (+ (+ x1 y1) (+ x2 y2)))
      (pure total))))

(coalton
  (Tuple (sum-first-two (the (List (Tuple Integer Integer)) [(Tuple 1 2) (Tuple 3 4)]))
         (sum-first-two (the (List (Tuple Integer Integer)) [(Tuple 1 2)]))))
; => #.(TUPLE 10 #.NONE)
```

## Other notes

- To lift an ordinary value into the monad at the end of a `do` block, use `pure` (from `Applicative`), **not** [`return`](/manual/operators/return/) (which performs an early exit from the enclosing function).
- Because each `(⟨pattern⟩ <- ...)` and non-final `⟨monadic-expr⟩` expands to a callback lambda passed to `>>=`, calls after a monadic bind are inside the callback body rather than in direct tail position of an outer [`rec`](/manual/operators/rec/). For imperative early-return unwrapping on `Result` or `Optional` without callback nesting, see [`need`](/manual/operators/need/).

