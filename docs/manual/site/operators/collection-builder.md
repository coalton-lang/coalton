---
title: "[]"
description: "Collection builder and collection comprehension syntax."
hideMeta: true
weight: 145
---

`[]` is reader syntax for finite collection builders and collection comprehensions, overloaded across collection types.

## Syntax

```lisp
;; Empty collection builder
[]

;; Itemized collection builder
[⟨elem-expr⟩+]

;; Collection comprehension
[⟨head-expr⟩ ⟨clause⟩+]

;; ⟨clause⟩ := :for ⟨var-or-_⟩ :below ⟨bound-expr⟩
;;           | :for ⟨var-or-_⟩ :in ⟨iterable-expr⟩
;;           | :with ⟨var-or-_⟩ = ⟨expr⟩
;;           | :when ⟨boolean-expr⟩
```

## Semantics

- `[...]` is polymorphic over any collection type `:c` with element type `:e`:
  - Itemized builders `[e1 ... en]` (including `[]`) desugar to the `FromItemizedCollection :c :e :s` typeclass protocol (`begin-collection-builder`, `adjoin-to-collection-builder`, `finalize-collection-builder`), passing the exact element count `n` up front.
  - Comprehensions `[head ⟨clause⟩+]` desugar to the `FromCollectionComprehension :c :e :s` typeclass protocol (`begin-collection-comprehension`, `adjoin-to-collection-comprehension`, `finalize-collection-comprehension`). When the first clause is `:for ⟨var⟩ :in ⟨iter⟩`, the iterator's `size-hint` (`Optional UFix`) is passed to `begin-collection-comprehension`.
- All element expressions in an itemized builder must unify to the same element type `:e` and are evaluated left-to-right in index order.
- **Comprehension clauses** are nested left-to-right from outermost to innermost:
  - `:for ⟨var-or-_⟩ :below ⟨bound-expr⟩` evaluates `⟨bound-expr⟩` once as an upper bound and iterates `⟨var-or-_⟩` from `0` by `1+` while `(< i bound)` (for any type implementing `Num` and `Ord`).
  - `:for ⟨var-or-_⟩ :in ⟨iterable-expr⟩` converts `⟨iterable-expr⟩` via `coalton/iterator:into-iter` (`IntoIterator`) and folds over its elements.
  - `:with ⟨var-or-_⟩ = ⟨expr⟩` binds `⟨var-or-_⟩` to `⟨expr⟩` for the remaining clauses and `⟨head-expr⟩`.
  - `:when ⟨boolean-expr⟩` filters iterations: subsequent clauses and `⟨head-expr⟩` run only when `⟨boolean-expr⟩` evaluates to `True`.
- **Type defaulting**: If the surrounding context does not determine a specific collection type `:c`, Coalton defaults `:c` to `(Seq :e)` while still inferring the element type `:e` normally. Empty builders `[]` whose element type is also unconstrained often need [`the`](/manual/operators/the/) (e.g., `(the (Vector Integer) [])`).

## Examples

```lisp
;; Itemized collection with type annotation
(coalton (the (List Integer) [1 2 3 4]))
; => (1 2 3 4)

;; Comprehension with multiple generators, `:with`, and `:when`
(coalton
  (the (Vector (Tuple UFix UFix))
    [(Tuple x y)
     :for x :below 4
     :for y :below 4
     :with sum = (+ x y)
     :when (even? sum)]))
; => #(#.(TUPLE 0 0) #.(TUPLE 0 2) #.(TUPLE 1 1) #.(TUPLE 1 3)
;      #.(TUPLE 2 0) #.(TUPLE 2 2) #.(TUPLE 3 1) #.(TUPLE 3 3))

;; Defaulting to `Seq` when consumed by a generic `Foldable` operation
(coalton (fold + 0 [10 20 30]))
; => 60
```

## Other notes

- Standard library types supporting `[]` are `Seq`, `List`, `Vector`, `Queue`, and `LispArray`. (For key-value maps and associations, use [`[=>]`](/manual/operators/association-builder/).)
- User-defined collection types can opt into `[]` syntax by implementing `FromItemizedCollection` and `FromCollectionComprehension` from `coalton/classes`.

