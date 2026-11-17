---
title: "unwrap-as"
description: "Unchecked conversion shorthand."
hideMeta: true
weight: 130
---

`unwrap-as` is an expression macro that performs a fallible `TryInto` conversion and immediately unwraps the resulting `Optional`, panicking if conversion fails.

## Syntax

```lisp
;; Convert `⟨expr⟩` to `⟨type⟩` via `TryInto` and unwrap
(unwrap-as ⟨type⟩ ⟨expr⟩)

;; Produce a unary unwrapping conversion function of type `(:a -> ⟨type⟩)`
(unwrap-as ⟨type⟩)
```

## Semantics

- `(unwrap-as ⟨type⟩ ⟨expr⟩)` expands to `(the ⟨type⟩ (coalton/classes:unwrap (coalton/classes:try-into ⟨expr⟩)))`.
- `(unwrap-as ⟨type⟩)` with one argument expands to `(fn (x) (the ⟨type⟩ (coalton/classes:unwrap (coalton/classes:try-into x))))`.
- Requires an instance of `TryInto :src ⟨type⟩`, where `:src` is the inferred type of `⟨expr⟩` (or the input type of the returned unary function).
- Returns the converted value of type `⟨type⟩` when `(try-into ⟨expr⟩)` produces `(Some v)`, or signals a `Panic` error if it produces `None`.
- `⟨type⟩` may reference scoped type variables bound by an enclosing [`forall`](/manual/operators/forall/).

## Examples

```lisp
;; Narrow an Integer known to fit in U16
(coalton (unwrap-as U16 (the Integer 1024)))
; => 1024

;; Use the one-argument form `(unwrap-as ⟨type⟩)` in a pipeline
(coalton
  (map (unwrap-as U8) (the (List Integer) [10 20 30])))
; => (10 20 30)
```

## Other Notes

- Use `unwrap-as` only when surrounding invariants guarantee that the conversion will succeed. When failure is expected and should be handled without panicking, use [`try-as`](/manual/operators/try-as/).
- For total conversions that never fail (`Into`), use [`as`](/manual/operators/as/).

