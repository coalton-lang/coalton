---
title: "=>"
description: "Constraint arrow for qualified types and class contexts."
hideMeta: true
weight: 20
---

`=>` separates a type's constraint context from the rest of the type. It also
appears in `define-class` and `define-instance` heads.

## Syntax

```lisp
⟨class⟩ :a => ⟨type⟩
(⟨class1⟩ :a) (⟨class2⟩ :a) => ⟨type⟩
```

## Semantics

- Everything to the left of `=>` is a type class constraint (also called a predicate).
- Everything to the right is the main type or class head.
- `=>` is purely part of type syntax. It is not a value-level operator.

## Options

- A single predicate can appear without extra parentheses.
- Multiple predicates are written as separate parenthesized constraints.
- In class declarations, `=>` introduces superclass-style requirements.

## Example

```lisp
(declare sort-pair ((Ord :a) => Tuple :a :a -> Tuple :a :a))

;; To be Ord, you must be Eq
(define-class (Eq :t => Ord :t)
  ...)
```
