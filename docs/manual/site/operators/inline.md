---
title: "inline"
description: "Inlining attribute and application hint."
hideMeta: true
weight: 75
---

`inline` is both a top-level attribute requesting that a function or method be inlined at all call sites and an expression-level hint requesting inlining at a specific call site.

## Syntax

```lisp
;; Top-level attribute on declare or define
(inline)
(declare ⟨name⟩ ⟨type⟩)

(inline)
(define (⟨name⟩ ⟨arg⟩*) ⟨body⟩+)

;; Attribute on an instance method
(define-instance (...)
  (inline)
  (define (⟨method-name⟩ ⟨arg⟩*) ⟨body⟩+))

;; Expression-level call-site hint
(inline (⟨fn-expr⟩ ⟨arg⟩*))
```

## Semantics

- **As a top-level or method attribute**:
  - `(inline)` takes no arguments and must appear immediately before [`declare`](/manual/operators/declare/), [`define`](/manual/operators/define/), or a method definition inside [`define-instance`](/manual/operators/define-instance/).
  - It marks the function or method for inlining across all saturated call sites.
  - It can also inline uses of typeclass-constrained values.
  - On a non-function value without typeclass constraints, `(inline)` has no effect and signals a style warning. It does not change when or how often such a value is initialized.
  - Dynamic variables (`*name*`) cannot be marked `inline`.
- **As an expression**:
  - `inline` has type `(:a -> :a)` and returns the value of its argument unchanged.
  - When its argument is syntactically a function application `(f a1 ...)`, it marks that specific call site for inlining even if `f` was not declared with `(inline)`.
  - If its argument is not a syntactic function application, `(inline expr)` evaluates directly to `expr`.
- **Inlining rules**:
  - Only saturated positional applications of functions with known bodies (and without keyword parameters) are inlined.
  - Arguments are bound sequentially left-to-right before the inlined body executes, preserving call-by-value evaluation order.
  - Recursive functions are unrolled up to `*inliner-max-unroll*` (default `3`) and total inlining depth is bounded by `*inliner-max-depth*` (default `16`).

## Examples

```lisp
(coalton-toplevel
  ;; Always inline `square` at every call site
  (inline)
  (declare square (Integer -> Integer))
  (define (square x)
    (* x x))

  (declare sum-of-squares (Integer * Integer -> Integer))
  (define (sum-of-squares a b)
    (+ (square a) (square b)))

  ;; Request call-site inlining for a specific application
  (declare step (Integer -> Integer))
  (define (step x)
    (inline (sum-of-squares x (+ x 1)))))
```

## Other notes

- Inlining small higher-order helpers and constructors often exposes local `Cell`s, structs, and `Optional`/`Result` values to unboxing and scalar replacement in release mode. See [Performance and Optimization](/manual/topics/performance-and-optimization/).
- Use [`noinline`](/manual/operators/noinline/) at a call site to prevent inlining of a specific call.

