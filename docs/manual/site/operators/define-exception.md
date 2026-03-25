---
title: "define-exception"
description: "Exception type definition form."
hideMeta: true
weight: 52
---

`define-exception` declares an exception type that can be constructed and
signaled with [`throw`](/manual/operators/throw/).

## Syntax

```lisp
(define-exception ⟨name⟩
  ⟨constructor⟩...)

;; ⟨constructor⟩ := ⟨constructor-name⟩
;;                | (⟨constructor-name⟩ ⟨arg-type⟩ ...)
```

## Semantics

- `define-exception` is a toplevel definition form.
- Its constructor syntax matches [`define-type`](/manual/operators/define-type/)
  closely, including optional docstrings on the type and constructors.
- Exception types do not accept type variables.
- Exception constructors are ordinary constructors and can be created outside
  `throw`.

## Example

```lisp
(define-exception BadEgg
  (UnCracked Egg)
  (DeadlyEgg Egg))
```
