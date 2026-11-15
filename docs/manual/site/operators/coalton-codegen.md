---
title: "coalton-codegen"
description: "Inspect generated Lisp for Coalton forms."
hideMeta: true
weight: 330
---

`coalton-codegen` compiles top-level Coalton forms and returns the generated Common Lisp code (without host type declarations) as a quoted s-expression.

## Syntax

```lisp
(coalton-codegen
  ⟨toplevel-form⟩*)
```

## Semantics

- `coalton-codegen` accepts the same top-level forms as [`coalton-toplevel`](/manual/operators/coalton-toplevel/) (`declare`, `define`, `define-type`, `define-struct`, `define-class`, `define-instance`, etc.).
- It runs the full Coalton compiler pipeline (parsing, type inference, specialization, inlining, monomorphization, scalar replacement, and Common Lisp code generation) with `coalton-impl/settings:*emit-type-annotations*` bound to `nil`.
- It expands to a quoted Common Lisp `(progn ...)` form containing the generated code, so evaluating `(coalton-codegen ...)` returns the generated code as data **without** executing it or permanently updating `*global-environment*`.

## Examples

```lisp
(coalton-codegen
  (declare double (Integer -> Integer))
  (define (double x)
    (+ x x)))
; => (PROGN
;     (LOCALLY
;      (DECLARE #+SBCL (OPTIMIZE (SB-C::TYPE-CHECK 0)))
;      (COALTON-IMPL/GLOBAL-LEXICAL:DEFINE-GLOBAL-LEXICAL DOUBLE FUNCTION)
;      (DEFUN DOUBLE (X-2)
;        (DECLARE (IGNORABLE X-2))
;        ...)
;      (SETF DOUBLE #'DOUBLE))
;     (VALUES))
```

## Other Notes

- For human-readable pretty-printed output without package prefixes on local symbols, use [`pprint-coalton-codegen`](/manual/operators/pprint-coalton-codegen/).
- To include emitted Common Lisp `(declare (type ...))`, `(declaim (ftype ...))`, and `(the (values ...) ...)` annotations, use [`coalton-codegen-types`](/manual/operators/coalton-codegen-types/) or [`pprint-coalton-codegen-types`](/manual/operators/pprint-coalton-codegen-types/).
- To inspect Coalton's post-optimization internal AST before Common Lisp code generation, use [`coalton-codegen-ast`](/manual/operators/coalton-codegen-ast/).

