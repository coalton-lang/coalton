---
title: "print-specializations"
description: "List registered Coalton function specializations."
hideMeta: true
weight: 325
---

`print-specializations` is a Common Lisp REPL helper that prints registered [`specialize`](/manual/operators/specialize/) rules.

## Syntax

```lisp
(print-specializations)
(print-specializations ⟨package-designator⟩)
```

## Semantics

- `print-specializations` is an ordinary Common Lisp function (called outside `coalton`).
- With no arguments (or `nil`), it prints all specialization rules registered in Coalton's global environment, grouped by the Common Lisp package of the generic function symbol.
- When `⟨package-designator⟩` (a package, symbol, string, or character) is supplied, it prints only the specializations for functions in that package, or signals an error if the package does not exist.
- Each entry displays the generic function and its type scheme, followed by each specialized target function and its specialized type.
- Returns `nil`.

## Examples

```lisp
(coalton-toplevel
  (declare add-one (Num :a => :a -> :a))
  (define (add-one x) (+ x 1))

  (declare add-one-ufix (UFix -> UFix))
  (define (add-one-ufix x) (+ x 1))

  (specialize add-one add-one-ufix (UFix -> UFix)))

(print-specializations cl:*package*)
;; prints:
;; [package COALTON-USER]
;;
;;   ADD-ONE :: ∀ A. NUM A ⇒ A → A
;;     ADD-ONE-UFIX :: #<FUNCTION-TY UFIX → UFIX>
```

## Other Notes

- Coalton exports four additional global database inspection functions with the same `(&optional ⟨package-designator⟩)` signature:
  - `(coalton:print-value-db)`: prints all global values, functions, constructors, and methods with their type schemes.
  - `(coalton:print-type-db)`: prints all defined types with their kinds.
  - `(coalton:print-class-db)`: prints all type classes with their parameter kinds and method signatures.
  - `(coalton:print-instance-db)`: prints all registered instances for each type class.

