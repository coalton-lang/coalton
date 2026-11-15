---
title: "break"
description: "Loop exit form."
hideMeta: true
weight: 290
---

`break` exits an enclosing [`for`](/manual/operators/for/) or [`for*`](/manual/operators/for-star/) loop immediately.

## Syntax

```lisp
;; Exit the innermost enclosing loop
(break)

;; Exit a labeled enclosing loop
(break ⟨label⟩)

;; ⟨label⟩ := :⟨keyword-name⟩
```

## Semantics

- `(break)` exits the innermost enclosing `for` or `for*` loop; `(break ⟨label⟩)` exits the enclosing loop named by keyword `⟨label⟩`.
- `break` must appear lexically inside the body of the target `for` or `for*` loop within the same function:
  - It cannot appear in the loop's binding initializers, step expressions, `:returns` expression, or termination condition.
  - It cannot cross an anonymous [`fn`](/manual/operators/fn/) boundary to exit a loop outside that `fn`.
- When `break` executes:
  - The current iteration's step expressions are **skipped**.
  - Control transfers to the end of the target loop, which then evaluates and returns its `:returns` expression (or returns `Void` if `:returns` was omitted).
- A `break` expression itself has type `Void` (zero values).

## Examples

```lisp
;; Break out of a loop early and return the current loop binding
(coalton
  (for ((declare i Integer)
        (i 0 (+ i 1)))
    :returns i
    :while (< i 100)
    (when (== (* i i) 49)
      (break))))
; => 7

;; Break out of an outer labeled loop from an inner loop
(coalton
  (progn
    (let found = (coalton/cell:new None))
    (for :rows ((r 0 (+ r 1)))
      :while (< r 4)
      (for ((c 0 (+ c 1)))
        :while (< c 4)
        (when (and (== r 2) (== c 3))
          (coalton/cell:write! found (Some (Tuple r c)))
          (break :rows))))
    (coalton/cell:read found)))
; => #.(TUPLE 2 3)
```

## Other Notes

- Unlike [`return`](/manual/operators/return/), which exits the entire enclosing function with a supplied value, `break` exits only the target `for` or `for*` loop and always uses that loop's `:returns` clause to produce the loop result.
- See [`continue`](/manual/operators/continue/), [`for`](/manual/operators/for/), and [`for*`](/manual/operators/for-star/).

