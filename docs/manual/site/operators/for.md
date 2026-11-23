---
title: "for"
description: "Imperative loop form."
hideMeta: true
weight: 270
---

`for` is Coalton's imperative loop form with parallel step updates.

## Syntax

```lisp
(for ⟨label⟩? (⟨binding-or-declare⟩*)
  [:returns ⟨return-expr⟩]
  [{:while | :until | :repeat} ⟨termination-expr⟩]
  ⟨body-element⟩*)

;; ⟨label⟩              := :⟨keyword-name⟩
;; ⟨binding-or-declare⟩ := (⟨var⟩ ⟨init-expr⟩ ⟨step-expr⟩)
;;                       | (declare ⟨var⟩ ⟨qualified-type⟩)
```

## Semantics

- **Label**: An optional keyword `⟨label⟩` (such as `:outer`) names the loop so nested [`break`](/manual/operators/break/) and [`continue`](/manual/operators/continue/) forms can target it.
- **Bindings**:
  - The binding list `(⟨binding-or-declare⟩*)` is required, though it may be empty `()`.
  - Each binding `(⟨var⟩ ⟨init-expr⟩ ⟨step-expr⟩)` requires both an initializer and a step expression. Write `⟨var⟩` itself as `⟨step-expr⟩` when a binding should remain constant across iterations.
  - `⟨init-expr⟩` and `⟨step-expr⟩` must unify to the same single-value type, and optional `(declare ⟨var⟩ ⟨qualified-type⟩)` clauses constrain the named loop variables.
  - Initializers are evaluated once before the first iteration using [`let`](/manual/operators/let/)-style dependency ordering.
  - At the end of each iteration, all `⟨step-expr⟩` forms are evaluated in parallel (via `cl:psetq`) from the current iteration's bindings. Use [`for*`](/manual/operators/for-star/) when later steps should observe earlier updated bindings in the same step phase.
- **Return value (`:returns`)**:
  - If present, `:returns ⟨return-expr⟩` must appear **immediately** after the binding list.
  - `⟨return-expr⟩` is evaluated in the scope of the loop bindings when the loop exits (either when the termination clause stops the loop or when [`break`](/manual/operators/break/) is executed), and its value(s) (a single value, `Void`, or multiple values) are returned from the `for` form.
  - When `:returns` is omitted, the `for` expression has type `Void` (zero values).
- **Termination clause**:
  - At most one of `:while`, `:until`, or `:repeat` may follow `:returns` (or the binding list):
    - `:while ⟨boolean-expr⟩`: tested before each iteration; the loop runs while the condition is `True`.
    - `:until ⟨boolean-expr⟩`: tested before each iteration; the loop stops as soon as the condition is `True`.
    - `:repeat ⟨ufix-expr⟩`: `⟨ufix-expr⟩` (of type `UFix`) is evaluated **once** before the first iteration (in the loop binding scope), and the loop runs that many iterations.
  - With no termination clause, the loop runs indefinitely until exited via [`break`](/manual/operators/break/), [`return`](/manual/operators/return/), [`throw`](/manual/operators/throw/), or [`resume-to`](/manual/operators/resume-to/).
- **Body**:
  - `⟨body-element⟩*` forms are evaluated sequentially on each iteration (and may include shorthand [`let`](/manual/operators/let/) bindings); their values are discarded.

## Examples

```lisp
;; Parallel step update computing Fibonacci numbers
(coalton
  (for ((declare i UFix)
        (i 0 (+ i 1))
        (a 0 b)
        (b 1 (+ a b)))
    :returns a
    :while (< i 10)))
; => 55

;; Fixed-count `:repeat` loop with an early `:outer` break
(coalton
  (progn
    (let total = (coalton/cell:new (the UFix 0)))
    (for :outer ()
      :repeat 5
      (for ((j 0 (+ j 1)))
        :repeat 5
        (when (> (coalton/cell:read total) 12)
          (break :outer))
        (coalton/cell:update! ƒs.(+ s j) total)))
    (coalton/cell:read total)))
; => 13
```

## Other notes

- In release mode, a local `coalton/cell:Cell` accumulator created with `cell:new` (either outside the loop or as an invariant `for` binding `(acc (cell:new init) acc)`) and mutated or read inside a `for` loop (via `cell:read`, `cell:write!`, `cell:swap!`, `cell:update!`, `cell:increment!`, `cell:decrement!`, `cell:push!`, or `cell:pop!`) is unboxed into a typed mutable lexical variable when the `Cell` does not escape its defining function. See [Performance and Optimization](/manual/topics/performance-and-optimization/).
- See [`for*`](/manual/operators/for-star/), [`break`](/manual/operators/break/), [`continue`](/manual/operators/continue/), and [`rec`](/manual/operators/rec/).

