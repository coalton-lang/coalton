---
title: "repr"
description: "Representation attribute for types and structs."
hideMeta: true
weight: 70
---

`repr` is a toplevel attribute that changes the runtime representation of a
type or struct.

## Syntax

```lisp
(repr :enum)
(repr :transparent)
(repr :lisp)
(repr :native ⟨lisp-ty⟩)
```

## Semantics

- `repr` must appear immediately before a compatible `define-type` or
  `define-struct`.
- `:enum` chooses a special representation of a non-parametric type based
   off of symbols. This allows efficient storage and fast matching.
- `:transparent` creates a no-overhead type when the type definition is
  just a union of one branch. The representation of a transparent type
  is the same as the type being wrapped.
- `:lisp` ensures the data type is fully compatible with Lisp: it provides
  a distinguished type definition and ensures the value is wrapped so it
  can be specialized in CLOS.
- `:native` allows the programmer to tell Coalton what the representation
  of the type is on the Lisp side. It is used for making interfaces to
  Lisp code.
- `define-struct` only supports transparent representation.
- Representation choice affects generated Lisp layout, interop, and some
  optimizations.

## Example

```lisp
(repr :transparent)
(define-type Gray (Gray U8))

(repr :native cl:cons)
(define-type (Pair :car :cdr))

(declare make-pair (forall (:car :cdr) (:car * :cdr -> (Pair :car :cdr))))
(define (make-pair x y)
  (lisp (-> (Pair :car :cdr))
    (cl:cons x y)))
```
