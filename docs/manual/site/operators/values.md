---
title: "values"
description: "Multiple-values construction, destructuring, and kind annotation."
hideMeta: true
weight: 195
---

`values` returns zero, one, or multiple values from an expression, destructures multiple values in a shorthand [`let`](/manual/operators/let/) binding, and (written `Values`) annotates result-polymorphic type variables in [`forall`](/manual/operators/forall/).

## Syntax

```lisp
;; Produce zero values (type `Void`)
(values)

;; Produce one or more values (type `⟨type⟩` or `⟨type-1⟩ * ⟨type-2⟩ * ...`)
(values ⟨expr⟩+)

;; Destructure zero, one, or more values in an expression body
(let (values ⟨var-or-_⟩*) = ⟨expr⟩)

;; Result-sequence kind annotation in `forall`
(forall ((:⟨var⟩ Values)) ⟨qualified-type⟩)
```

## Semantics

- **Constructing values with `(values ...)`**:
  - `(values)` produces zero values and has type `Void`.
  - `(values ⟨expr⟩)` produces a single value and has the same type as `⟨expr⟩`.
  - `(values ⟨expr-1⟩ ⟨expr-2⟩ ...)` evaluates its arguments left-to-right and returns multiple values of type `⟨type-1⟩ * ⟨type-2⟩ * ...` (see [`*`](/manual/operators/star/)).
  - Every argument `⟨expr-i⟩` to `values` must be a single-value expression (`Void` and nested multi-value expressions cannot be passed as components of `values`).
  - A multi-value expression can appear wherever multiple values are accepted: as the return value of a function (in tail position or via [`return`](/manual/operators/return/)), on the right-hand side of `(let (values ...) = ...)`, as a non-final expression in [`progn`](/manual/operators/progn/), as the `:returns` expression of [`for`](/manual/operators/for/)/[`for*`](/manual/operators/for-star/), as the protected expression of [`protect`](/manual/operators/protect/), or in the branches of [`if`](/manual/operators/if/), [`cond`](/manual/operators/cond/), [`match`](/manual/operators/match/), [`catch`](/manual/operators/catch/), or [`handle`](/manual/operators/handle/).
- **Destructuring values with `(let (values ...) = ...)`**:
  - Valid inside any implicit [`progn`](/manual/operators/progn/) or [`do`](/manual/operators/do/) body (not inside a block `(let (...) ...)` binding list).
  - The number of binders `⟨var-or-_⟩*` must match the exact number of values returned by `⟨expr⟩` (including `0` for `(let (values) = ⟨void-expr⟩)`).
  - Each binder must be an ordinary variable name or [`_`](/manual/operators/wildcard/), and all variable names in the `(values ...)` list must be distinct.
- **Kind annotation `Values` in [`forall`](/manual/operators/forall/)**:
  - In a `forall` binder `(:r Values)`, the same symbol `coalton:Values` denotes the kind of an entire result sequence (zero, one, or multiple values). See [`forall`](/manual/operators/forall/).

## Examples

```lisp
(coalton-toplevel
  ;; Return two values without heap-allocating a Tuple
  (declare min-max (Ord :a => :a * :a -> :a * :a))
  (define (min-max a b)
    (if (<= a b)
        (values a b)
        (values b a)))

  ;; Destructure multiple return values in a function body
  (declare span (Integer * Integer -> Integer))
  (define (span a b)
    (let (values lo hi) = (min-max a b))
    (- hi lo))

  ;; Return zero values (`Void`)
  (declare noop (Void -> Void))
  (define (noop)
    (values)))
```

## Other Notes

- `(values ...)` and `(let (values ...) = ...)` compile directly to Common Lisp `cl:values` and `cl:multiple-value-bind`, avoiding heap allocation of intermediate tuples. See [Performance and Optimization](/manual/topics/performance-and-optimization/).
- See [`*`](/manual/operators/star/), [`->`](/manual/operators/arrow/), [`let`](/manual/operators/let/), and [`forall`](/manual/operators/forall/).

