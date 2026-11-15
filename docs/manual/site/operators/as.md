---
title: "as"
description: "Total conversion shorthand."
hideMeta: true
weight: 110
---

`as` is an expression macro for total type conversions via the `Into` type class.

## Syntax

```lisp
;; Convert `⟨expr⟩` to `⟨type⟩`
(as ⟨type⟩ ⟨expr⟩)

;; Produce a unary converting function of type `(:a -> ⟨type⟩)`
(as ⟨type⟩)
```

## Semantics

- `(as ⟨type⟩ ⟨expr⟩)` expands to `(the ⟨type⟩ (coalton/classes:into ⟨expr⟩))`.
- `(as ⟨type⟩)` with one argument expands to `(fn (x) (the ⟨type⟩ (coalton/classes:into x)))`.
- Requires an instance of `Into :src ⟨type⟩`, where `:src` is the inferred type of `⟨expr⟩` (or the input type of the returned unary function).
- `⟨type⟩` may reference scoped type variables bound by an enclosing [`forall`](/manual/operators/forall/).
- Because `Into` represents total conversions that always succeed, `as` returns a value of `⟨type⟩` directly (not wrapped in `Optional` or `Result`).

## Examples

```lisp
;; Convert a String into a List of Chars
(coalton (as (List Char) "coalton"))
; => (#\c #\o #\a #\l #\t #\o #\n)

;; Use the one-argument form `(as ⟨type⟩)` with `map` or `pipe`
(coalton
  (map (as F64) (the (List I32) [1 2 3 4])))
; => (1.0d0 2.0d0 3.0d0 4.0d0)

(coalton
  (pipe (the I32 16)
        (as F64)
        sqrt))
; => 4.0d0
```

## Other Notes

- Unlike [`the`](/manual/operators/the/), which only constrains the static type of an expression without changing its runtime value, `as` invokes the `into` method and may allocate or copy data (for example, converting a `String` to a `List Char`).
- For partial conversions that may fail (`TryInto`), use [`try-as`](/manual/operators/try-as/) or [`unwrap-as`](/manual/operators/unwrap-as/).

