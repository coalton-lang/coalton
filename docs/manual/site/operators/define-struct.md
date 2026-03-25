---
title: "define-struct"
description: "Struct definition form."
hideMeta: true
weight: 70
---

`define-struct` creates a struct-like product type with named fields.

## Syntax

```lisp
(define-struct ⟨name⟩
  (⟨field-name⟩ ⟨docstring⟩? ⟨type⟩)...)
```

## Semantics

- Toplevel form to define a new struct-like data type.
- Structs are useful when named fields are clearer than positional fields of `define-type`.
- A field named `f` is accessed with the operator `.f`.
- A struct name is conventionally written in CamelCase.

## Example

```lisp
(define-struct Point
  (x Integer)
  (y Integer))

(let ((p (Point 1 2))
  (Point (.y p) (.x p))))
```
