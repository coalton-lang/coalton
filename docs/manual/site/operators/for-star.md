---
title: "for*"
description: "Sequential imperative loop form."
hideMeta: true
weight: 280
---

`for*` is the sequential counterpart to [`for`](/manual/operators/for/), evaluating both loop initializers and step updates sequentially from top to bottom.

## Syntax

```lisp
(for* ⟨label⟩? (⟨binding-or-declare⟩*)
  [:returns ⟨return-expr⟩]
  [{:while | :until | :repeat} ⟨termination-expr⟩]
  ⟨body-element⟩*)

;; ⟨label⟩              := :⟨keyword-name⟩
;; ⟨binding-or-declare⟩ := (⟨var⟩ ⟨init-expr⟩ ⟨step-expr⟩)
;;                       | (declare ⟨var⟩ ⟨qualified-type⟩)
```

## Semantics

- `for*` has the same clause structure, `:returns` behavior, termination clauses (`:while`, `:until`, `:repeat`), and loop-label rules as [`for`](/manual/operators/for/), with sequential binding and step evaluation:
  - **Sequential initializers**: Each `⟨init-expr⟩` is evaluated top-to-bottom (via `cl:let*`), so later initializers can reference earlier loop variables, and initializers cannot reference their own or later loop variables.
  - **Sequential steps**: At the end of each iteration, `⟨step-expr⟩` forms are evaluated and assigned top-to-bottom (via `cl:setq`), so each `⟨step-expr⟩` observes the already-updated values of earlier bindings in the list.
- Duplicate binding variable names in the same `for*` binding list are rejected at compile time.
- `:returns ⟨return-expr⟩`, if supplied, must appear immediately after the binding list; when omitted, `for*` returns `Void` (zero values).

## Examples

```lisp
;; `acc` step uses the already-incremented value of `i` in the same iteration
(coalton
  (for* ((declare i Integer)
         (declare acc Integer)
         (i 0 (+ i 1))
         (acc 0 (+ acc i)))
    :returns acc
    :until (== i 5)))
; => 15
```

## Other notes

- Contrast the step phase of `for*` with [`for`](/manual/operators/for/): in `(for ((i 0 (+ i 1)) (acc 0 (+ acc i))) ...)`, `(+ acc i)` adds the *old* value of `i` from before the step (`0 + 1 + 2 + 3 + 4 = 10`), whereas in `for*` above, `i` is updated to `(+ i 1)` first and `(+ acc i)` adds the *new* value of `i` (`1 + 2 + 3 + 4 + 5 = 15`).
- In release mode, a local `coalton/cell:Cell` accumulator created with `cell:new` and mutated or read inside a `for*` loop is unboxed into a typed mutable lexical variable when the `Cell` does not escape its defining function. See [Performance and Optimization](/manual/topics/performance-and-optimization/).

