---
title: "let"
description: "Recursive lexical binding and shorthand body-binding form."
hideMeta: true
weight: 160
---

`let` introduces recursive or dependency-ordered lexical bindings in a block, or sequential destructuring and dynamic bindings inside an expression body.

## Syntax

```lisp
;; Block `let` (mutually visible and dependency-ordered bindings)
(let (⟨binding-or-declare⟩*)
  ⟨body-element⟩*
  ⟨expr⟩)

;; Shorthand pattern binding (inside a body)
(let ⟨pattern⟩ = ⟨expr⟩)

;; Shorthand multiple-value binding (inside a body)
(let (values ⟨var-or-_⟩*) = ⟨expr⟩)

;; Shorthand dynamic variable rebinding (inside a body)
(let *⟨dynamic-var⟩* = ⟨expr⟩)

;; ⟨binding-or-declare⟩ := (⟨var⟩ ⟨init-expr⟩)
;;                       | (declare ⟨var⟩ ⟨qualified-type⟩)
```

## Semantics

### Block `let`

- `(let (⟨binding-or-declare⟩*) ...)` binds zero or more ordinary variables `⟨var⟩` over `⟨body-element⟩* ⟨expr⟩`. Duplicate variable names in the same binding list are rejected.
- All variables bound in the `let` list are in lexical scope across all `⟨init-expr⟩` forms in the same list, as well as in the body.
- Bindings are grouped into strongly connected dependency components (SCCs) and evaluated in topological dependency order: if `x` depends on `y` and `y` does not depend on `x`, `y` is evaluated before `x` regardless of source order.
- Recursive and mutually recursive cycles within a `let` list are allowed for:
  - Function bindings whose initializer is an [`fn`](/manual/operators/fn/) (compiled to Common Lisp `labels`).
  - Direct constructor applications of algebraic data types or structs that do not use `:enum` or `:transparent` representation (allocated first and then back-patched with `setf`).
  - Other recursive value cycles are rejected at compile time.
- Each `(⟨var⟩ ⟨init-expr⟩)` must produce a single value; `Void` and multi-value results cannot be bound in a block `let` clause (use shorthand `(let (values ...) = ...)` instead).
- Local [`declare`](/manual/operators/declare/) forms in the binding list constrain the named bindings.
- Non-expansive bindings (functions, variables, literals, and constructor applications) are generalized to polymorphic types inside the body; expansive bindings follow the relaxed value restriction.
- Use [`let*`](/manual/operators/let-star/) when you want strict top-to-bottom sequential evaluation without mutual visibility across bindings.

### Shorthand `let` in bodies

- Inside any implicit [`progn`](/manual/operators/progn/) body (`define`, `fn`, `let`, `let*`, `progn`, `when`, `unless`, `cond`, `match`, `catch`, `handle`, `resumable`, `protect`, `unsafe`, `for`, `for*`, `do`), a non-final form may be a shorthand `let`. Value-returning bodies cannot end with a shorthand `let` (loop bodies in `for` and `for*` discard all body forms and may end with a pattern or `values` shorthand `let`).
- `(let ⟨pattern⟩ = ⟨expr⟩)` evaluates `⟨expr⟩`, matches it against `⟨pattern⟩` (a variable, `_`, constructor pattern, or [`=`](/manual/operators/pattern-bind/) pattern), and binds its variables for the remaining forms in the body.
  - Writing `(let _ = ⟨expr⟩)` explicitly discards any result, including `Void`, multiple values, or `Result` (suppressing discarded-result warnings).
- `(let (values ⟨var-or-_⟩*) = ⟨expr⟩)` binds the multiple return values of `⟨expr⟩` (or zero values when written `(let (values) = ⟨expr⟩)`). Each binder in `(values ...)` must be a variable name or `_`.
- `(let *⟨dynamic-var⟩* = ⟨expr⟩)` dynamically rebinds `*⟨dynamic-var⟩*` for the remaining forms in the enclosing body (equivalent to wrapping the rest of the body in [`dynamic-bind`](/manual/operators/dynamic-bind/); supported in non-loop bodies and `do` blocks).

## Examples

```lisp
;; Block `let` orders non-cyclic dependencies automatically
(coalton
  (let ((declare x Integer)
        (x (+ y 1))
        (y 2))
    (+ x y)))
; => 5

;; Mutually recursive local functions in block `let`
(coalton
  (let ((even? (fn (n) (if (== n 0) True (odd? (- n 1)))))
        (odd?  (fn (n) (if (== n 0) False (even? (- n 1))))))
    (even? (the Integer 10))))
; => COMMON-LISP:T

;; Shorthand `let` for pattern destructuring and multiple values
(coalton
  (progn
    (let (Tuple a b) = (Tuple 10 20))
    (let (values sum diff) = (values (+ a b) (- b a)))
    (Tuple sum diff)))
; => #.(TUPLE 30 10)
```

## Other notes

- Non-escaping local functions bound by `let` (whose names are only called directly within their lexical scope and never returned, stored, or captured by an escaping closure) are declared `dynamic-extent`, allowing the Lisp compiler to stack-allocate their closures.
- In release mode, a locally bound `coalton/cell:Cell` created with `cell:new` that does not escape its lexical scope is unboxed into a typed mutable lexical variable with no heap allocation. See [Performance and Optimization](/manual/topics/performance-and-optimization/).

