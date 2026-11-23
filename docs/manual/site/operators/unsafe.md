---
title: "unsafe"
description: "Experimental escape hatch from runtime safety checks."
hideMeta: true
weight: 100
---

`unsafe` is an expression form exported from `#:coalton++` (`coalton++:unsafe`) that lexically disables generated Common Lisp runtime safety and type checks within its body.

## Syntax

```lisp
(coalton++:unsafe
  ⟨body-element⟩*
  ⟨expr⟩)
```

## Semantics

- `unsafe` is exported from the `#:coalton++` package rather than `#:coalton`.
- Its body is an implicit [`progn`](/manual/operators/progn/) (supporting multiple expressions and shorthand `let` bindings) and returns the value(s) of the final `⟨expr⟩`.
- Static typechecking is **not** disabled: Coalton still typechecks every expression inside `unsafe` under the normal type rules.
- In generated Common Lisp, `unsafe`:
  - Wraps the body in `(cl:locally (cl:declare (cl:optimize (cl:safety 0) #+sbcl (sb-c::type-check 0))) ...)`.
  - Suppresses the `(sb-c::type-check 1)` declaration that Coalton normally emits around embedded [`lisp`](/manual/operators/lisp/) forms inside its lexical scope.
- The final expression of an `unsafe` block is in tail position whenever the `unsafe` block itself is in tail position (including for [`rec`](/manual/operators/rec/) tail calls).
- Returning a value from a `lisp` form inside `unsafe` that violates its declared Coalton type causes undefined behavior in the host Common Lisp runtime.

## Examples

```lisp
(coalton-toplevel
  ;; Unchecked 64-bit unsigned addition via inline Lisp
  (declare unsafe-u64-add (U64 * U64 -> U64))
  (define (unsafe-u64-add a b)
    (coalton++:unsafe
      (lisp (-> U64) (a b)
        (cl:the (cl:unsigned-byte 64) (cl:+ a b))))))
```

## Other notes

- `unsafe` is intended for low-level standard library primitives and verified hot loops where boundary checks on `lisp` forms have already been established by surrounding invariants.
- Inspect the effect of `unsafe` on generated declarations with [`pprint-coalton-codegen-types`](/manual/operators/pprint-coalton-codegen-types/).

