---
title: "define-resumption"
description: "Resumption definition form."
hideMeta: true
weight: 54
---

`define-resumption` declares a resumable control signal that can be handled by
[`resumable`](/manual/operators/resumable/).

## Syntax

```lisp
(define-resumption ⟨constructor⟩)
(define-resumption (⟨constructor⟩ ⟨arg-type⟩ ...)
  ⟨docstring⟩)
```

## Semantics

- `define-resumption` is a toplevel definition form.
- A resumption has exactly one constructor, which also determines the
  resumption type's name.
- Unlike exceptions, resumptions are intended to transfer control to an
  enclosing `resumable` handler via [`resume-to`](/manual/operators/resume-to/).
- Every resumption type is an instance of the `Resumption` class, which
  `resume-to` requires. Instances of `Resumption` cannot be written manually,
  and a resumption type cannot be redefined as a type that is not a
  resumption.
- Optional docstrings are allowed.

## Example

```lisp
(define-resumption (ServeRaw Egg)
  "Suggest that the egg be served raw.")
```
