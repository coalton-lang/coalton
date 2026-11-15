---
title: "repr"
description: "Representation attribute for types, structs, and exceptions."
hideMeta: true
weight: 70
---

`repr` is a top-level attribute that specifies the Common Lisp runtime representation of a [`define-type`](/manual/operators/define-type/), [`define-struct`](/manual/operators/define-struct/), or [`define-exception`](/manual/operators/define-exception/).

## Syntax

```lisp
(repr :enum)
(repr :transparent)
(repr :lisp)
(repr :native ⟨lisp-type-specifier⟩)
```

## Semantics

- `repr` must appear immediately before `define-type`, `define-struct`, or `define-exception` (or separated from it only by a [`derive`](/manual/operators/derive/) attribute). At most one `repr` attribute is allowed per definition.
- **`:enum`**:
  - Valid on `define-type` when every constructor is nullary (has zero fields).
  - Represents each constructor `C` of type `T` at runtime as the symbol `T/C`, with Common Lisp type `(cl:member T/C1 T/C2 ...)`.
  - Avoids heap allocation and allows [`match`](/manual/operators/match/) to compile to `cl:case` jump tables.
- **`:transparent`**:
  - Valid on `define-type` or `define-struct` when the type has exactly one constructor with exactly one field `(C τ)`.
  - Eliminates any runtime wrapper: the runtime representation of the type is identical to that of `τ`, the constructor `C` compiles to an inlined identity function, and struct field accessors compile to `id`.
- **`:lisp`**:
  - Valid on `define-type`.
  - Guarantees a distinct heap-allocated class or structure (`T` and constructor subtypes `T/C`) across both development and release modes so values can be specialized in CLOS `cl:defmethod` definitions and tested with `cl:typep`.
- **`:native ⟨lisp-type-specifier⟩`**:
  - Valid on `define-type` (typically with zero constructors) and `define-exception` (with zero constructors).
  - Maps the Coalton type directly to `⟨lisp-type-specifier⟩` in emitted Common Lisp type declarations and `RuntimeRepr` instances.
  - On `define-exception`, `⟨lisp-type-specifier⟩` must be a compile-time subtype of `cl:serious-condition`.
- `define-struct` only accepts `(repr :transparent)`. `define-exception` only accepts `(repr :native ⟨lisp-condition-type⟩)`. `define-type-alias` and `define-resumption` do not accept `repr`.

## Examples

```lisp
(coalton-toplevel
  ;; Symbol-backed enumeration
  (repr :enum)
  (define-type Color
    Red
    Green
    Blue)

  ;; Zero-overhead newtype wrapper
  (repr :transparent)
  (define-type Meters
    (Meters F64))

  ;; Foreign Common Lisp type wrapper
  (repr :native cl:cons)
  (define-type (LispPair :car :cdr))

  (declare make-pair (forall (:car :cdr) :car * :cdr -> (LispPair :car :cdr)))
  (define (make-pair x y)
    (lisp (-> (LispPair :car :cdr)) (x y)
      (cl:cons x y))))
```

## Other Notes

- Without an explicit `repr` attribute, Coalton compiles `define-type` and `define-struct` definitions as CLOS classes in development mode and frozen `cl:defstruct` types in release mode.
- Parametric opaque native types (`(repr :native ...) (define-type (T :a))`) have their type parameters inferred as invariant because Coalton cannot inspect their internal mutability.
- See [Lisp Interop](/manual/topics/lisp-interop/) for details on runtime representation guarantees.

