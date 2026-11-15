---
title: "describe-type-alias"
description: "Print the definition behind a type alias."
hideMeta: true
weight: 315
---

`describe-type-alias` is a Common Lisp REPL helper that prints the underlying expansion of a Coalton type alias.

## Syntax

```lisp
(describe-type-alias ⟨symbol⟩)
```

## Semantics

- `describe-type-alias` is an ordinary Common Lisp function (called outside `coalton`), and `⟨symbol⟩` must evaluate to a `cl:symbol`.
- It looks up `⟨symbol⟩` in Coalton's global type-alias environment:
  - If `⟨symbol⟩` names a type alias defined with [`define-type-alias`](/manual/operators/define-type-alias/), it prints the alias and its expanded type (in `:types-and-aliases` mode) to `*standard-output*`.
  - If `⟨symbol⟩` is not a known type alias (even if it names a nominal `define-type` or `define-struct`), it prints `unknown`.
- Returns zero values `(cl:values)`.

## Examples

```lisp
(coalton-toplevel
  (define-type-alias (Pair :a) (Tuple :a :a))
  (define-type-alias IntPair (Pair Integer)))

(describe-type-alias 'Pair)
;; prints: #<COALTON-IMPL/TYPECHECKER/TYPES:TAPP [PAIR :A := TUPLE :A :A]>

(describe-type-alias 'IntPair)
;; prints: #<COALTON-IMPL/TYPECHECKER/TYPES:TAPP [INTPAIR := PAIR INTEGER := TUPLE INTEGER INTEGER]>
```

## Other Notes

- See [`define-type-alias`](/manual/operators/define-type-alias/), [`describe-type-of`](/manual/operators/describe-type-of/), and [`set-type-printing-mode`](/manual/operators/set-type-printing-mode/).

