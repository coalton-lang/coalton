---
title: "[=>]"
description: "Association builder and association comprehension syntax."
hideMeta: true
weight: 146
---

`[=>]` is Coalton's reader syntax for association builders and association
comprehensions.

## Syntax

```lisp
;; finite association
[=>]
[⟨key-expr⟩ => ⟨value-expr⟩ ...]

;; association comprehension
[⟨key-expr⟩ => ⟨value-expr⟩ ⟨clause⟩...]
```

## Semantics

- `[... => ...]` does not denote a specific, single type. It is generic across several
  association-like types.
- `[=>]` is an empty association builder.
- Any other bracket form containing `=>` is an association builder or
  association comprehension.
- Association builders and comprehensions must be finite.
- Keys must all have the same type, and values must all have the same type.
- Without a more specific expected type, association builders default to
  `Seq (Tuple :key :value)`.
- Association builders, especially empty ones, often need [`the`](/manual/operators/the/)
  to fix the intended key and value types.

## Options

- `:for ⟨var⟩ :below ⟨ufix⟩` iterates from `0` up to but not including the bound.
- `:for ⟨var⟩ :in ⟨iter⟩` iterates over an iterator.
- `:with ⟨var⟩ = ⟨expr⟩` introduces a lexical binding for the remaining clauses.
- `:when ⟨expr⟩` guards emitted entries.
- New types can opt-in to this syntax by defining instance of `FromItemizedAssociation` and `FromAssociationComprehension`.

## Example

```lisp
(define squares
  [x => (* x x) :for x :below 5])

(define empty-pairs
  (the (HashMap String Integer) [=>]))
```
