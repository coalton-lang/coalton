---
title: "abi"
description: "Calling convention and multi-entrypoint attribute for functions and methods."
hideMeta: true
weight: 72
---

`abi` is a top-level attribute that controls the calling convention and worker entrypoints generated for a top-level function or typeclass instance method.

## Syntax

```lisp
;; Top-level attribute on declare or define
(abi ⟨convention⟩)
(declare ⟨name⟩ ⟨type⟩)

(abi ⟨convention⟩)
(define (⟨name⟩ ⟨arg⟩*) ⟨body⟩+)

;; Attribute on an instance method
(define-instance (...)
  (abi ⟨convention⟩)
  (define (⟨method-name⟩ ⟨arg⟩*) ⟨body⟩+))

;; ⟨convention⟩ := :auto | :lisp | :unpacked | :unboxed
```

## Semantics

- `(abi ⟨convention⟩)` must appear immediately before a top-level [`declare`](/manual/operators/declare/), a top-level [`define`](/manual/operators/define/), or a method definition inside [`define-instance`](/manual/operators/define-instance/). At most one `abi` attribute may be attached to a declaration or definition, and dynamic variables (`*name*`), types, and classes cannot carry an `abi` attribute.
- Every top-level function `f` always retains its canonical **`:lisp`** entrypoint on the symbol `f` (both `(cl:fdefinition 'f)` and the global lexical value `f`), so higher-order calls (`(map f xs)`), first-class function values, [`call-coalton-function`](/manual/topics/lisp-interop/), keyword-bearing calls, and REPL/FFI callers continue to use standard boxed Common Lisp calling conventions.
- When a specialized convention is active and eligible, the compiler generates one or more worker entrypoints alongside `f` and rewrites saturated direct calls `(f a1 ... an)` to invoke the specialized entrypoint directly:
  - **`:lisp`**:
    - Emits only the canonical boxed Common Lisp entrypoint `f`.
    - This is the default in development mode when no `abi` attribute is present.
  - **`:unpacked`**:
    - Generates a portable worker entrypoint `f%UNPACKED` over standard Common Lisp `cl:defun` and `cl:values`:
      - Single-constructor product arguments ([`define-struct`](/manual/operators/define-struct/) and single-constructor [`define-type`](/manual/operators/define-type/) ADTs such as `Tuple`) that are only destructured inside the function are flattened into separate positional parameters (up to 16 flattened inputs).
      - Single-constructor product return types are returned as multiple values via `cl:values` (up to 16 flattened outputs).
      - Non-recursive sum return types with concrete payload fields (such as `Optional` and `Result`, up to 8 constructors) are returned via `cl:values` as a discriminant tag (`Boolean` for 2-constructor sums, `UFix` for 3–8-constructor sums) followed by the payload values.
    - Intermediate pack/unpack steps across chained direct calls `(f (g x))` and [`match`](/manual/operators/match/) expressions cancel out during optimization without allocating intermediate structs or sum wrappers.
  - **`:unboxed`**:
    - Extends `:unpacked` with an additional worker entrypoint `f%UNBOXED` when supported by the host compiler and architecture (on SBCL `:x86-64` and `:arm64` when type annotations are enabled).
    - Passes flattened `U64`, `I64`, `F32`, and `F64` arguments and return values directly in CPU general-purpose and floating-point registers across non-inlined and cross-file calls without heap boxing.
    - On Common Lisp implementations or architectures without `:unboxed` support, `(abi :unboxed)` automatically falls back to `:unpacked` (for composite types) or `:lisp`.
  - **`:auto`**:
    - Automatically selects the most specialized eligible convention (`:unboxed` > `:unpacked` > `:lisp`).
    - This is the default for all eligible top-level functions and instance methods in release mode (`COALTON_ENV=release`).
- **Whole-value parameters**:
  - A struct or tuple parameter is flattened in `f%UNPACKED` / `f%UNBOXED` only when all uses of that parameter inside the function are field reads or pattern matches. If the parameter is also used as a whole value (for example, stored into a data structure, passed to a higher-order function, or captured in the variable list of a [`lisp`](/manual/operators/lisp/) form), it remains a single boxed parameter so the callee never re-allocates a duplicate copy.
- Functions with keyword parameters ([`&key`](/manual/operators/keyword-tail/)) always use the `:lisp` convention.

## Examples

```lisp
(coalton-toplevel
  (define-struct Vec3
    (x F64)
    (y F64)
    (z F64))

  ;; Flatten Vec3 inputs/outputs and pass all F64 leaves in XMM registers on SBCL
  (abi :unboxed)
  (declare vec3-fma (Vec3 * F64 * Vec3 -> Vec3))
  (define (vec3-fma a scale b)
    (Vec3 (+ (* (.x a) scale) (.x b))
          (+ (* (.y a) scale) (.y b))
          (+ (* (.z a) scale) (.z b))))

  ;; Return Optional U64 as (values Boolean U64) in registers without heap allocation
  (abi :unboxed)
  (declare safe-div (U64 * U64 -> Optional U64))
  (define (safe-div a b)
    (if (== b 0)
        None
        (Some (coalton/math/integral:quot a b))))

  ;; Force the canonical boxed Common Lisp ABI
  (abi :lisp)
  (declare boxed-sum (Tuple U64 U64 -> U64))
  (define (boxed-sum (Tuple x y))
    (+ x y)))
```

## Other notes

- When a function with `:unpacked` or `:unboxed` worker entrypoints is redefined with a compatible type signature, all of its entrypoints are updated in lockstep. When it is redefined with an incompatible signature, Coalton installs forwarding stubs on any retired worker symbols so existing compiled callers continue to route through the new definition.
- See [Performance and Optimization](/manual/topics/performance-and-optimization/) and [`repr`](/manual/operators/repr/).
