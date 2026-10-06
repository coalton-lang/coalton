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

(repr :native ⟨lisp-condition-type⟩)
(define-exception ⟨name⟩)

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
- Every exception type is an instance of the `Exception` class, which
  [`throw`](/manual/operators/throw/) requires. Instances of `Exception` cannot
  be written manually, and an exception type cannot be redefined as a type
  that is not an exception.
- With `(repr :native ⟨lisp-condition-type⟩)`, the exception type is an
  existing Lisp condition type instead of a new one. The Lisp type must be a
  subtype of `cl:serious-condition`, and the exception cannot have
  constructors. The Lisp type must be defined when the `define-exception`
  form is compiled, so a `define-condition` in the same file must be wrapped
  in `(eval-when (:compile-toplevel :load-toplevel :execute) ...)`. Values
  are usually obtained by catching them with a
  `(the ⟨name⟩ var)` branch of [`catch`](/manual/operators/catch/), or
  constructed in a [`lisp`](/manual/operators/lisp/) form.
- `define-exception` accepts no other attributes.

## Example

```lisp
(define-exception BadEgg
  (UnCracked Egg)
  (DeadlyEgg Egg))

(repr :native cl:division-by-zero)
(define-exception DivisionByZero
  "Lisp's DIVISION-BY-ZERO condition.")
```
