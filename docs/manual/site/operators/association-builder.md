---
title: "[=>]"
description: "Association builder and association comprehension syntax."
hideMeta: true
weight: 146
---

`[=>]` is reader syntax for finite key-value association builders and association comprehensions, overloaded across map and association types.

## Syntax

```lisp
;; Empty association builder
[=>]

;; Itemized association builder
[⟨key-expr⟩ => ⟨value-expr⟩ ...]

;; Association comprehension
[⟨key-expr⟩ => ⟨value-expr⟩ ⟨clause⟩+]

;; ⟨clause⟩ := :for ⟨var-or-_⟩ :below ⟨bound-expr⟩
;;           | :for ⟨var-or-_⟩ :in ⟨iterable-expr⟩
;;           | :with ⟨var-or-_⟩ = ⟨expr⟩
;;           | :when ⟨boolean-expr⟩
```

## Semantics

- `[... => ...]` is polymorphic over any association collection type `:c` with key type `:k` and value type `:v`:
  - Itemized association builders `[k1 => v1 ... kn => vn]` (and empty `[=>]`) desugar to the `FromItemizedAssociation :c :k :v :s` typeclass protocol (`begin-association-builder`, `adjoin-to-association-builder`, `finalize-association-builder`), passing the exact entry count `n` up front.
  - Association comprehensions `[k => v ⟨clause⟩+]` desugar to the `FromAssociationComprehension :c :k :v :s` typeclass protocol (`begin-association-comprehension`, `adjoin-to-association-comprehension`, `finalize-association-comprehension`). When the first clause is `:for ⟨var⟩ :in ⟨iter⟩`, the iterator's `size-hint` is passed to `begin-association-comprehension`.
- All `⟨key-expr⟩` forms must unify to a single key type `:k`, and all `⟨value-expr⟩` forms must unify to a single value type `:v`. Entries are evaluated left-to-right in index order.
- **Comprehension clauses** follow the same rules as [`[]`](/manual/operators/collection-builder/):
  - `:for ⟨var-or-_⟩ :below ⟨bound-expr⟩` iterates from `0` by `1+` while `(< i bound)` (for any type implementing `Num` and `Ord`).
  - `:for ⟨var-or-_⟩ :in ⟨iterable-expr⟩` iterates over any `IntoIterator` source.
  - `:with ⟨var-or-_⟩ = ⟨expr⟩` introduces a local binding for subsequent clauses and the key/value expressions.
  - `:when ⟨boolean-expr⟩` guards entry emission with a `Boolean` condition.
- **Type defaulting**: If the surrounding context does not fix the target type `:c`, Coalton defaults `:c` to `(Seq (Tuple :k :v))` while inferring `:k` and `:v` from the key and value expressions. Empty builders `[=>]` usually require [`the`](/manual/operators/the/) to specify the map, key, and value types.

## Examples

```lisp
;; Itemized HashMap construction
(coalton
  (the (coalton/hashmap:HashMap String Integer)
    ["one" => 1
     "two" => 2
     "three" => 3]))

;; Association comprehension building a table of squares
(coalton
  (the (List (Tuple UFix UFix))
    [x => (* x x) :for x :below 5 :when (odd? x)]))
; => (#.(TUPLE 1 1) #.(TUPLE 3 9))

;; Empty association builder with explicit type
(coalton
  (the (coalton/hashmap:HashMap String Integer) [=>]))
```

## Other notes

- Standard library types supporting `[=>]` are `HashMap`, `Hashtable`, `OrdMap`, `List (Tuple :k :v)`, and `Seq (Tuple :k :v)`.
- User-defined map or association types can opt into `[=>]` syntax by implementing `FromItemizedAssociation` and `FromAssociationComprehension` from `coalton/classes`.

