---
title: "define-struct"
description: "Struct definition form."
hideMeta: true
weight: 70
---

`define-struct` defines a product type with named fields, a constructor, and field accessor functions.

## Syntax

```lisp
(repr :transparent)?
(derive ⟨class-name⟩+)?
(define-struct ⟨struct-head⟩
  ⟨docstring⟩?
  ⟨field-spec⟩*)

;; ⟨struct-head⟩ := ⟨struct-name⟩
;;                | (⟨struct-name⟩ ⟨type-var⟩+)
;; ⟨field-spec⟩  := (⟨field-name⟩ ⟨docstring⟩? ⟨field-type⟩)
```

## Semantics

- `define-struct` is a top-level form valid inside [`coalton-toplevel`](/manual/operators/coalton-toplevel/).
- It defines both a type `⟨struct-name⟩` (optionally parameterized by keyword type variables `⟨type-var⟩+`) and a single constructor with the same name `⟨struct-name⟩`.
- For fields `(f1 t1) ... (fn tn)`, the constructor `⟨struct-name⟩` has function type `(t1 * ... * tn -> ⟨struct-type⟩)` (or value type `⟨struct-type⟩` when zero fields are declared).
- Each field `(f τ)` makes the field accessor `.f` available as a first-class unary function of type `(⟨struct-type⟩ -> τ)`.
- Multiple struct types may define fields with the same name. Coalton resolves `.f` using the inferred type of the argument (or expected function type) and signals `"Ambiguous accessor"` if the struct type cannot be determined.
- Struct values can also be destructured positionally in [`match`](/manual/operators/match/), [`let`](/manual/operators/let/), and function parameters using the pattern `(⟨struct-name⟩ p1 ... pn)`.
- `define-struct` supports [`derive`](/manual/operators/derive/) (`Eq`, `Ord`, `Hash`, `Default`, `Show`) and `(repr :transparent)` (valid only when the struct has a single field). No other [`repr`](/manual/operators/repr/) modes are allowed on structs.

## Examples

```lisp
(coalton-toplevel
  (derive Eq Show Default)
  (define-struct (Point :a)
    "A point in 2D space."
    (x "Horizontal coordinate." :a)
    (y "Vertical coordinate."   :a))

  (declare reflect-diagonal ((Point :a) -> (Point :a)))
  (define (reflect-diagonal p)
    (Point (.y p) (.x p)))

  ;; `.x` is a first-class unary function
  (declare x-coords ((List (Point Integer)) -> (List Integer)))
  (define (x-coords pts)
    (map .x pts)))
```

## Other Notes

- In release mode, the compiler can eliminate a locally constructed struct used only for `.field` access or [`match`](/manual/operators/match/) destructuring, including across local `if`, `cond`, and `match` branches. Field initializers still evaluate once, in left-to-right order, while the fields are kept in unboxed scalar bindings. See [Performance and Optimization](/manual/topics/performance-and-optimization/).
- Struct names are conventionally written in `CamelCase`.

