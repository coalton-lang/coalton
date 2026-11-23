---
title: "derive"
description: "Attribute for automatically generating type class instances."
hideMeta: true
weight: 45
---

`derive` is a top-level attribute that automatically generates type class instances for a [`define-type`](/manual/operators/define-type/) or [`define-struct`](/manual/operators/define-struct/).

## Syntax

```lisp
(derive ⟨class-name⟩+)
```

## Semantics

- `derive` must appear immediately before a `define-type` or `define-struct` (or separated from it only by a [`repr`](/manual/operators/repr/) attribute). At most one `derive` attribute may be attached to a definition.
- The target type must have at least one constructor.
- For each listed `⟨class-name⟩`, Coalton synthesizes a [`define-instance`](/manual/operators/define-instance/) for `(⟨class-name⟩ ⟨type⟩)` whose context requires `(⟨class-name⟩ ⟨field-type⟩)` for every constructor or struct field type `⟨field-type⟩`.
- Built-in derivable classes in the standard library:
  - `Eq`: Implements `==` by checking that two values have the same constructor and recursively comparing corresponding fields left-to-right with `==`.
  - `Ord`: Implements `<=>` by ordering constructors from first to last in declaration order. For the same constructor, it compares fields left-to-right with `<=>`, stopping at the first non-`EQ` result. Equal fields (or no fields) give `EQ`. The type must also have a compatible `Eq` instance, which can be derived with `(derive Eq Ord)`. Reordering constructors or fields can change the ordering.
  - `Hash`: Implements `hash` by seeding a constructor-specific hash from the type name and constructor name, then folding each field's `hash` with `coalton/hash:combine-hashes`.
  - `Default`: Implements `default` for single-constructor types and structs by calling `(default)` for every field. (Rejects sum types with multiple constructors.)
  - `Show`: Implements `show-to` by formatting nullary constructors as `"Ctor"` and constructors with fields as `"(Ctor field1 field2 ...)"`.

## Examples

```lisp
(coalton-toplevel
  (derive Eq Ord Hash Default Show)
  (define-struct (Point :a)
    (x :a)
    (y :a))

  (derive Eq Show)
  (define-type (Shape :a)
    (Circle (Point :a) :a)
    (Rectangle (Point :a) (Point :a))))

(coalton
  (progn
    (let p = (the (Point Integer) (default)))
    (== p (Point 0 0))))
; => COMMON-LISP:T
```

The standard library derives `Ord` for `Tuple3`, `Tuple4`, and `Tuple5`. Like pairs, they compare lexicographically and can be used as `OrdMap` keys when all their components implement `Ord`.

## Other notes

- Custom type classes can support `derive` by defining a Common Lisp method on the generic function `coalton-impl/typechecker:derive-methods` EQL-specialized on the class symbol.
- `define-exception`, `define-resumption`, and `define-type-alias` do not accept `derive`.

