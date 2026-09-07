---
title: "[]"
description: "Collection builder and collection comprehension syntax."
hideMeta: true
weight: 145
---

`[]` is Coalton's reader syntax for collection builders and collection
comprehensions.

## Syntax

```lisp
;; finite collection
[]
[⟨expr⟩ ...]

;; collection comprehension
[⟨expr⟩ ⟨clause⟩...]
```

## Semantics

- `[...]` does not denote a specific, single type. It is generic across several
  collection-like types.
- `[]` is an empty collection builder.
- Collection builders and comprehensions must be finite.
- Elements must all have the same type.
- Without a more specific expected type, collection builders default to `Seq :t`.
- Collection builders, especially empty ones, often need [`the`](/manual/operators/the/)
  to fix the intended element type.

Defaulting also applies when a collection is consumed immediately. For example,
`(fold + 0 [1 2 3])` builds a `Seq Integer` and returns `6`. Choosing `Seq` leaves
element types to ordinary inference; a surrounding type annotation or function
signature can still require a different collection type.

## Options

- `:for ⟨var⟩ :below ⟨ufix⟩` iterates from `0` up to but not including the bound.
- `:for ⟨var⟩ :in ⟨iter⟩` iterates over an iterator.
- `:with ⟨var⟩ = ⟨expr⟩` introduces a lexical binding for the remaining clauses.
- `:when ⟨expr⟩` guards emitted entries.
- New types can opt-in to this syntax by defining instance of `FromItemizedCollection` and `FromCollectionComprehension`.

## Example

```lisp
(define evens
  [x :for x :below 10 :when (even? x)])

(define xs
  (the (List F32) [1 2 3]))
```
