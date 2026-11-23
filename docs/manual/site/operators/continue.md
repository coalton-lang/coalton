---
title: "continue"
description: "Loop-iteration skip form."
hideMeta: true
weight: 300
---

`continue` skips the remainder of the current [`for`](/manual/operators/for/) or [`for*`](/manual/operators/for-star/) iteration and advances to the next iteration.

## Syntax

```lisp
;; Continue the innermost enclosing loop
(continue)

;; Continue a labeled enclosing loop
(continue ⟨label⟩)

;; ⟨label⟩ := :⟨keyword-name⟩
```

## Semantics

- `(continue)` applies to the innermost enclosing `for` or `for*` loop; `(continue ⟨label⟩)` applies to the enclosing loop named by keyword `⟨label⟩`.
- `continue` must appear lexically inside the body of the target `for` or `for*` loop within the same function (it cannot appear in loop header clauses or cross an [`fn`](/manual/operators/fn/) boundary).
- When `continue` executes:
  - The remaining body expressions of the target loop's current iteration are skipped.
  - The target loop's step expressions **are evaluated** as usual (and any `:repeat` counter is decremented).
  - Control proceeds to the next iteration's termination test (`:while`, `:until`, or `:repeat`).
- A `continue` expression itself has type `Void` (zero values).

## Examples

```lisp
;; Sum only odd numbers below 10 by skipping even iterations
(coalton
  (progn
    (let sum = (coalton/cell:new (the Integer 0)))
    (for ((declare i Integer)
          (i 0 (+ i 1)))
      :while (< i 10)
      (when (even? i)
        (continue))
      (coalton/cell:update! ƒs.(+ s i) sum))
    (coalton/cell:read sum)))
; => 25

;; Continue an outer labeled loop from inside a nested loop
(coalton
  (for :outer ((i 0 (+ i 1)))
    :while (< i 3)
    (for ((j 0 (+ j 1)))
      :while (< j 3)
      (when (== i j)
        (continue :outer)))))
```

## Other notes

- See [`break`](/manual/operators/break/), [`for`](/manual/operators/for/), and [`for*`](/manual/operators/for-star/).

