---
title: "try-as"
description: "Partial conversion shorthand."
hideMeta: true
weight: 120
---

`try-as` is an expression macro for fallible type conversions via the `TryInto` type class, returning an `Optional`.

## Syntax

```lisp
;; Attempt to convert `⟨expr⟩` to `⟨type⟩`, returning `(Optional ⟨type⟩)`
(try-as ⟨type⟩ ⟨expr⟩)

;; Produce a unary fallible conversion function of type `(:a -> (Optional ⟨type⟩))`
(try-as ⟨type⟩)
```

## Semantics

- `(try-as ⟨type⟩ ⟨expr⟩)` expands to `(the (Optional ⟨type⟩) (coalton/classes:try-into ⟨expr⟩))`.
- `(try-as ⟨type⟩)` with one argument expands to `(fn (x) (the (Optional ⟨type⟩) (coalton/classes:try-into x)))`.
- Requires an instance of `TryInto :src ⟨type⟩`, where `:src` is the inferred type of `⟨expr⟩` (or the input type of the returned unary function).
- Evaluates to `(Some value)` when `⟨expr⟩` can be converted to `⟨type⟩`, or `None` when the conversion fails (such as an out-of-range integer conversion).
- `⟨type⟩` may reference scoped type variables bound by an enclosing [`forall`](/manual/operators/forall/).

## Examples

```lisp
;; Bounds-checked conversion from Integer to U8
(coalton
  (Tuple (try-as U8 (the Integer 200))
         (try-as U8 (the Integer 300))))
; => #.(TUPLE 200 #.NONE)

;; Use the one-argument form `(try-as ⟨type⟩)` with `map`
(coalton
  (map (try-as U8) (the (List Integer) [-1 0 127 255 256])))
; => (#.NONE 0 127 255 #.NONE)
```

## Other Notes

- Combine `(try-as ⟨type⟩ ⟨expr⟩)` with [`need`](/manual/operators/need/) or [`do`](/manual/operators/do/) to propagate `None` early from functions returning `Optional`.
- When conversion failure should panic immediately rather than return `Optional`, use [`unwrap-as`](/manual/operators/unwrap-as/). For total conversions via `Into`, use [`as`](/manual/operators/as/).

