---
title: "describe-type-of"
description: "Print type information for a Coalton value."
hideMeta: true
weight: 310
---

`describe-type-of` is a Common Lisp REPL helper that prints the type and type aliases of a global Coalton symbol.

## Syntax

```lisp
(describe-type-of ⟨symbol⟩)
```

## Semantics

- `describe-type-of` is an ordinary Common Lisp function (called outside `coalton`), and `⟨symbol⟩` must evaluate to a `cl:symbol`.
- It inspects `⟨symbol⟩` in the current Common Lisp environment and Coalton's global value environment and prints one line to `*standard-output*`:
  - If `⟨symbol⟩` has a Common Lisp `macro-function` (including [`define-expression-macro`](/manual/operators/define-expression-macro/) and [`define-toplevel-macro`](/manual/operators/define-toplevel-macro/)), it prints `macro`.
  - If `⟨symbol⟩` names a known Coalton value, function, typeclass method, or data constructor, it prints its type scheme with `:types-and-aliases` printing mode enabled.
  - Otherwise, it prints `unknown`.
- Returns zero values `(cl:values)`.

## Examples

```lisp
(coalton-toplevel
  (define-type-alias (Pair :a) (Tuple :a :a))

  (declare make-diagonal (:a -> (Pair :a)))
  (define (make-diagonal x)
    (Tuple x x)))

(describe-type-of 'make-diagonal)
;; prints: ∀ :A. :A → [PAIR :A := TUPLE :A :A]

(describe-type-of 'pipe)
;; prints: macro
```

## Other Notes

- Unlike [`type-of`](/manual/operators/type-of/), which is a Coalton expression form that can inspect arbitrary compound expressions, `describe-type-of` is a Common Lisp function that looks up a single symbol in the global environment and always enables `:types-and-aliases` display.
- To inspect a type alias symbol itself, use [`describe-type-alias`](/manual/operators/describe-type-alias/). To list all values in a package, use `(coalton:print-value-db OptionalPackage)`.

