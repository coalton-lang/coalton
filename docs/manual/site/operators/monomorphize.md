---
title: "monomorphize"
description: "Monomorphization attribute for declarations and definitions."
hideMeta: true
weight: 72
---

`monomorphize` is a top-level optimization attribute that specializes polymorphic helper calls reachable from a [`declare`](/manual/operators/declare/) or [`define`](/manual/operators/define/) entry point.

## Syntax

```lisp
(monomorphize)
(declare ⟨name⟩ ⟨type⟩)

(monomorphize)
(define (⟨name⟩ ⟨arg⟩*)
  ⟨body⟩+)
```

## Semantics

- `(monomorphize)` takes no arguments and must appear immediately before a top-level `declare` or `define` form (and may be combined with [`inline`](/manual/operators/inline/)).
- It marks `⟨name⟩` as a root for Coalton's call-graph monomorphization pass during optimization.
- Starting from `⟨name⟩`, the monomorphizer traverses the call graph for applications of known top-level functions (without keyword arguments) that are passed one or more statically known typeclass dictionaries alongside runtime arguments.
- For each distinct combination of function and static dictionary arguments, the compiler generates a specialized top-level function with those dictionary parameters constant-propagated and removed, then re-runs inlining, specialization, and constant folding on the specialized body.
- `monomorphize` is most effective on functions declared with a concrete type (or partially known types) whose implementation calls polymorphic, typeclass-constrained helpers (including recursive polymorphic functions that cannot be fully inlined).
- `monomorphize` does not alter program semantics.

## Examples

```lisp
(coalton-toplevel
  (monomorphize)
  (declare axpy (F32 * F32 * F32 -> F32))
  (define (axpy a x y)
    (+ (* a x) y)))
```

Here the body uses generic typeclass methods `+` and `*`, which would otherwise pass `Num F32` dictionaries or call generic helpers. With `(monomorphize)`, the calls are specialized and inlined to unboxed single-precision floating-point instructions, as shown in this ARM64 disassembly:

```
COALTON-USER> (disassemble #'axpy)
; disassembly for AXPY
; Size: 48 bytes. Origin: #x7021136678                        ; AXPY
; 78:       AA0A40F9         LDR R0, [THREAD, #16]            ; binding-stack-pointer
; 7C:       AA0B00F9         STR R0, [CFP, #16]

; 80:       6008221E         FMUL S0, S3, S2
; 84:       0028211E         FADD S0, S0, S1

; 88:       0100261E         FMOV WNL1, S0
; 8C:       217C60D3         LSL NL1, NL1, #32
; 90:       2A640091         ADD R0, NL1, #25
; 94:       FB031DAA         MOV CSP, CFP
; 98:       5F0300F1         CMP NULL, #0
; 9C:       BD7B40A9         LDP CFP, LR, [CFP]
; A0:       C0035FD6         RET
; A4:       E00120D4         BRK #15                          ; Invalid argument count trap
```

## Other notes

- Monomorphized helper clones are deduplicated across all `(monomorphize)` entry points within the same `coalton-toplevel` unit.
- During monomorphization, any matching [`specialize`](/manual/operators/specialize/) rules are also applied automatically once types become concrete.
- See [`inline`](/manual/operators/inline/), [`specialize`](/manual/operators/specialize/), and [Performance and Optimization](/manual/topics/performance-and-optimization/).
