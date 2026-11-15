---
title: "coalton-codegen-types"
description: "Inspect generated Lisp with host type declarations."
hideMeta: true
weight: 335
---

`coalton-codegen-types` compiles top-level Coalton forms and returns the generated Common Lisp code—including all emitted Common Lisp type declarations—as a quoted s-expression.

## Syntax

```lisp
(coalton-codegen-types
  ⟨toplevel-form⟩*)
```

## Semantics

- `coalton-codegen-types` accepts the same top-level forms as [`coalton-toplevel`](/manual/operators/coalton-toplevel/).
- It runs the full Coalton compiler pipeline with `coalton-impl/settings:*emit-type-annotations*` bound to `t`, emitting Common Lisp `(declaim (ftype ...))`, `(declare (type ...))`, and `(the (values ...) ...)` forms.
- It expands to a quoted Common Lisp `(progn ...)` form containing the generated code, returning it as data without executing the generated definitions or permanently modifying `*global-environment*`.

## Examples

```lisp
(coalton-codegen-types
  (declare double (UFix -> UFix))
  (define (double x)
    (+ x x)))
```

## Other Notes

- For formatted output printed to `*standard-output*` with local package prefixes stripped, use [`pprint-coalton-codegen-types`](/manual/operators/pprint-coalton-codegen-types/).
- To inspect the generated Common Lisp without type declarations, use [`coalton-codegen`](/manual/operators/coalton-codegen/) or [`pprint-coalton-codegen`](/manual/operators/pprint-coalton-codegen/).

