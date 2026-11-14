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

In release mode, the compiler can eliminate locally constructed data type
values—including single-constructor types such as `Tuple` and sum types such as
`Optional` and `Result` produced across local `if`, `cond`, and `match`
branches—when they are consumed only by local pattern matching. Constructor
arguments still evaluate once, in order. Passing, returning, or capturing the
value can prevent this optimization unless inlining exposes its pattern matches.

## Example

```lisp
(define-type (Tree :a)
  (Leaf :a)
  (Branch (Tree :a) (Tree :a)))
```
