---
title: "define-type"
description: "Algebraic data type definition form."
hideMeta: true
weight: 50
---

`define-type` creates an algebraic data type and its constructors.

## Syntax

```lisp
⟨repr-directive⟩?
⟨derive-directive⟩?
(define-type ⟨type⟩
  ⟨docstring⟩?
  ⟨constructor⟩...)

;; ⟨type⟩ := ⟨name⟩
;;         | (⟨name⟩ ⟨type-var⟩...)
;; ⟨constructor⟩ := ⟨constructor-name⟩ ⟨docstring⟩?
;;                | (⟨constructor-name⟩ ⟨docstring⟩? ⟨arg-type⟩ ...)
```

## Semantics

- Constructors are values/functions which construct the data type.
- The form supports docstrings and `derive`/`repr` directives.
- It is the main way to define tagged unions and sum types in Coalton.
- Types and constructors are conventionally written in CamelCase.

## Example

```lisp
(define-type (Tree :a)
  (Leaf :a)
  (Branch (Tree :a) (Tree :a)))
```
