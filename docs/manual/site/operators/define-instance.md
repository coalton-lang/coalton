---
title: "define-instance"
description: "Type class instance definition form."
hideMeta: true
weight: 90
---

`define-instance` supplies an implementation of a type class for a concrete
type or constrained type pattern.

## Syntax

```lisp
(define-instance (⟨class⟩ ⟨type⟩...)
  ⟨method⟩...)

;; ⟨method⟩ := ⟨define-form⟩
```

## Semantics

- The instance head determines which class/type combination is being
  implemented.
- All required class methods must be implemented.
- Instances may not overlap (i.e., only zero or one instance must be valid for a given type)

## Example

```lisp
(define-instance (Eq Unit)
  (define (== _ _) True))
```
