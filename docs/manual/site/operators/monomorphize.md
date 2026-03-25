---
title: "monomorphize"
description: "Monomorphization attribute for declarations and definitions."
hideMeta: true
weight: 72
---

`monomorphize` is a toplevel optimization attribute for `declare` and `define`.

## Syntax

```lisp
(monomorphize)
(declare ⟨name⟩ ⟨monomorphic-type⟩)
(define (⟨name⟩ ⟨arg⟩ ...)
  ⟨body⟩)
```

## Semantics

- `(monomorphize)` must appear immediately before a `declare` or `define`
  form.
- It marks the binding for Coalton's monomorphization pass during optimization.
- This is mainly useful when the implementation is written in terms of generic
  class-constrained operations, but the binding itself has a concrete declared
  type.
- In practice this often means numerically generic bodies that use operators
  like `+`, `*`, or `<`, while the exported function is declared at a single
  concrete type such as `Integer` or `Double-Float`.
- `monomorphize` is an optimization hint and should not change observable
  behavior.

## Example

```lisp
(coalton-toplevel
  (monomorphize)
  (declare axpy (F32 * F32 * F32 -> F32))
  (define (axpy a x y)
    (+ (* a x) y)))
```

Here the body is ordinary generic numeric code, which would normally compile
into generic calls, even with the monomorphic declaration. `monomorphize` rewrites
all of the calls to monomorphic ones, which we can observe in this ARM64 disassembly:

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