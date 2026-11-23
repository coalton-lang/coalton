---
title: "progn"
description: "Flattened sequencing form."
hideMeta: true
weight: 255
---

`progn` sequences a series of expressions (and shorthand `let` bindings) in expression context, or splices a series of top-level forms in [`coalton-toplevel`](/manual/operators/coalton-toplevel/).

## Syntax

```lisp
;; Expression sequencing
(progn
  ⟨body-element⟩*
  ⟨expr⟩)

;; Top-level form splicing (inside `coalton-toplevel`)
(progn
  ⟨toplevel-form⟩*)

;; ⟨body-element⟩ := ⟨expr⟩
;;                 | (let ⟨pattern⟩ = ⟨expr⟩)
;;                 | (let (values ⟨var-or-_⟩*) = ⟨expr⟩)
;;                 | (let *⟨dynamic-var⟩* = ⟨expr⟩)
```

## Semantics

- **In expression contexts**:
  - Requires at least one expression and cannot end with a shorthand `let` binding.
  - Evaluates each `⟨body-element⟩` sequentially from top to bottom.
  - Non-final expressions are evaluated for their side effects and their results (whether a single value, `Void`, or multiple values) are discarded. Discarding a `Result` without `(let _ = ...)` emits a compiler warning.
  - Shorthand [`let`](/manual/operators/let/) forms (`(let pat = expr)`, `(let (values ...) = expr)`, and `(let *dyn* = expr)`) bind variables for all subsequent forms in the `progn`.
  - The final `⟨expr⟩` determines the value(s) and type of the `progn` (a single value, `Void`, or multiple values) and is in tail position when the `progn` is in tail position.
- **In top-level contexts (`coalton-toplevel`)**:
  - Splices zero or more top-level declarations, definitions, and attributes into the enclosing `coalton-toplevel` unit.
  - A top-level `progn` cannot be preceded by an attribute (`repr`, `derive`, `inline`, `monomorphize`, `overlap`) and cannot end with a trailing unconsumed attribute.

## Examples

```lisp
;; Expression-level `progn` with shorthand `let` bindings
(coalton
  (progn
    (let (Tuple x y) = (Tuple 3 4))
    (let (values sum prod) = (values (+ x y) (* x y)))
    (+ sum prod)))
; => 19

;; Splicing multiple top-level definitions in `coalton-toplevel`
(coalton-toplevel
  (progn
    (declare a Integer)
    (define a 10)
    (declare b Integer)
    (define b (+ a 5))))
```

## Other notes

- Implicit `progn` bodies are used throughout Coalton in [`define`](/manual/operators/define/), [`fn`](/manual/operators/fn/), [`let`](/manual/operators/let/), [`let*`](/manual/operators/let-star/), [`when`](/manual/operators/when/), [`unless`](/manual/operators/unless/), [`cond`](/manual/operators/cond/) clauses, [`match`](/manual/operators/match/) / [`catch`](/manual/operators/catch/) / [`handle`](/manual/operators/handle/) / [`resumable`](/manual/operators/resumable/) branches, [`protect`](/manual/operators/protect/) cleanup forms, and [`unsafe`](/manual/operators/unsafe/). An explicit `progn` is needed when grouping multiple expressions into a single-expression slot (such as a branch of [`if`](/manual/operators/if/)) or when using shorthand `(let x = ...)` bindings inside [`coalton`](/manual/operators/coalton/).

