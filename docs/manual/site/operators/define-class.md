---
title: "define-class"
description: "Type class definition form."
hideMeta: true
weight: 80
---

`define-class` introduces a type class.

## Syntax

```lisp
(define-class (⟨constraints⟩ ... => ⟨class-name⟩ ⟨fields⟩ ... ⟨func-deps⟩?)
  (⟨method⟩ ⟨docstring⟩? ⟨type⟩)
  ...)

;; ⟨func-deps⟩ := (⟨type-var⟩ -> ⟨type-var⟩*)
```

## Semantics

- The class head may include constraints and functional dependencies.
- Constraints in effect allow the definition of a subclass.
- Functional dependencies help the inferencer if one type variable
  completely determine the others.
- Methods are declared as part of the class definition.
- Instances are supplied separately with `define-instance`.
- If no methods are present, then it is called a "marker class". They
  must still be instantiated, but only serve to mark a type.
- A docstring is optional.

## Example

```lisp
(define-class (Eq :a)
  (== "Mathematical equality." (:a * :a -> Boolean)))
```
