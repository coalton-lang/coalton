---
title: "type-of"
description: "Inspect the inferred type of an expression."
hideMeta: true
weight: 300
---

`type-of` is an in-language Coalton expression form that reflects the inferred static type scheme of an expression as a runtime `TypeScheme` value without evaluating the expression.

## Syntax

```lisp
(type-of ⟨expr⟩)
```

## Semantics

- `type-of` is a Coalton expression operator (used inside [`coalton`](/manual/operators/coalton/) or [`coalton-toplevel`](/manual/operators/coalton-toplevel/)).
- It infers the principal type scheme of `⟨expr⟩` at compile time—performing unification, functional-dependency improvement, struct accessor resolution, and context reduction—and evaluates at runtime to a `coalton/types:TypeScheme` object representing that scheme.
- `⟨expr⟩` is **not** evaluated at runtime, so `(type-of ⟨expr⟩)` never performs side effects and is treated as a non-expansive value by the typechecker.
- `⟨expr⟩` may be polymorphic or have unresolved typeclass constraints; those constraints and quantified type variables become part of the returned `TypeScheme`.
- The returned `TypeScheme` can be printed with `show`, converted to a `String` with `show-as-string`, or inspected directly at the Common Lisp REPL.

## Examples

```lisp
;; Inspect the inferred type of a polymorphic expression at the REPL
(coalton (type-of (fn (xs) (map ƒx.(+ x 1) xs))))
; => ∀ (:F (Type → Type)) :A. (Num :A) (Functor :F) ⇒ :F :A → :F :A

;; Convert a reflected TypeScheme to a Coalton String
(coalton (show-as-string (type-of (Tuple 1 "hello"))))
; => "∀ :A. Num :A ⇒ Tuple :A String"
```

## Other Notes

- How `TypeScheme` objects render type aliases is controlled by [`set-type-printing-mode`](/manual/operators/set-type-printing-mode/).
- For a Common Lisp REPL helper that looks up a global symbol by name and prints its type and aliases directly to `*standard-output*`, see [`describe-type-of`](/manual/operators/describe-type-of/).

