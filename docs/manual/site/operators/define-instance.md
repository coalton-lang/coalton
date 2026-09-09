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
- Overlapping instances must all opt in with the [`overlap`](/manual/operators/overlap/)
  attribute. Resolution requires a unique most-specific matching head.

## Example

```lisp
(define-instance (Eq Unit)
  (define (== _ _) True))
```
