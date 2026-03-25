---
title: "forall"
description: "Explicit type-variable quantification."
hideMeta: true
weight: 30
---

`forall` introduces explicit type-variable binders in a type declaration, also
commonly known as *scoped type variables*. Coalton can infer polymorphism without
it, but `forall` is useful when you want the type-variable names themselves to be
part of the declaration.

## Syntax

```lisp
(forall (⟨var⟩...) ⟨type⟩)
```

## Semantics

- `forall` explicitly quantifies the listed type variables.
- Those binder names are then available inside related `the`, `declare`, and
  `lisp` annotations in the corresponding body.
- `forall` expressions inside of `define-class` are scoped to their method
  definitions in `define-instance`.
- Without `forall`, declarations are still implicitly quantified, but the names
  are not scoped into the body.

## Options

- Coalton also accepts the Unicode alias `∀`.

## Example

```lisp
(declare keep-first (forall (:left :right) :left -> :right -> :left))
```
