---
title: "set-type-printing-mode"
description: "Control how Coalton prints types and aliases."
hideMeta: true
weight: 320
---

`set-type-printing-mode` is a Common Lisp function that configures how Coalton formats types and type aliases in printed output and compiler diagnostics.

## Syntax

```lisp
(set-type-printing-mode ⟨mode⟩)

;; ⟨mode⟩ := :types | :aliases | :types-and-aliases
```

## Semantics

- `set-type-printing-mode` is an ordinary Common Lisp function (called outside `coalton`).
- `⟨mode⟩` must evaluate to one of three keywords (any other value signals an error) and is stored in `coalton-impl/typechecker:*coalton-type-printing-mode*`:
  - `:types` (default): prints only the expanded underlying types.
  - `:aliases`: prints only the most recent type alias name when a type was produced through a [`define-type-alias`](/manual/operators/define-type-alias/).
  - `:types-and-aliases`: prints `[Alias := UnderlyingType]` inline whenever a type carries one or more type aliases.
- Returns `⟨mode⟩`.
- The setting affects [`type-of`](/manual/operators/type-of/), `show` on `TypeScheme` values, `coalton:print-value-db`, and compiler type error messages.

## Examples

```lisp
(coalton-toplevel
  (define-type-alias (Pair :a) (Tuple :a :a))
  (declare origin (Pair Integer))
  (define origin (Tuple 0 0)))

(set-type-printing-mode :types)
(coalton (type-of origin))
; => TUPLE INTEGER INTEGER

(set-type-printing-mode :aliases)
(coalton (type-of origin))
; => PAIR INTEGER

(set-type-printing-mode :types-and-aliases)
(coalton (type-of origin))
; => [PAIR INTEGER := TUPLE INTEGER INTEGER]
```

## Other Notes

- [`describe-type-of`](/manual/operators/describe-type-of/) and [`describe-type-alias`](/manual/operators/describe-type-alias/) locally bind the printing mode to `:types-and-aliases` regardless of the current global setting.

