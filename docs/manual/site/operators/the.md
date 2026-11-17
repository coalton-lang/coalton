---
title: "the"
description: "Inline type annotation."
hideMeta: true
weight: 100
---

`the` constrains the type of an expression inline, and also forms typed exception patterns in [`catch`](/manual/operators/catch/) and [`handle`](/manual/operators/handle/).

## Syntax

```lisp
;; Expression type annotation
(the ⟨type⟩ ⟨expr⟩)
(the (forall (⟨binder⟩+) ⟨qualified-type⟩) ⟨expr⟩)

;; Typed exception pattern in `catch` or `handle`
((the ⟨exception-type⟩ ⟨var-or-_⟩) ⟨handler-body⟩+)
```

## Semantics

- **As an expression**:
  - `(the ⟨type⟩ ⟨expr⟩)` unifies the inferred type of `⟨expr⟩` with `⟨type⟩` and evaluates to `⟨expr⟩` with no runtime overhead (`the` is erased after typechecking).
  - `⟨type⟩` may be a single-value type, a function type, `Void` (for zero-value expressions), or a multi-value type `T1 * T2 * ...` (for multi-value expressions).
  - `⟨type⟩` may specialize a polymorphic expression (for example, fixing the numeric type of a literal or the container type of `[]` or `[=>]`), but it cannot be *more general* than the inferred type of `⟨expr⟩` (annotating an `Integer` expression as `(the :a expr)` is a compile-time error).
  - `the` annotations without an explicit `forall` cannot include [`=>`](/manual/operators/constraint-arrow/) typeclass constraints; wrap constrained annotations in an explicit `(forall (⟨binder⟩+) ...)` or use [`declare`](/manual/operators/declare/) on a binding.
  - `⟨type⟩` may reference scoped type variables bound by an enclosing [`forall`](/manual/operators/forall/), or introduce its own explicit `(forall (⟨binder⟩+) ...)` whose type variables are scoped over `⟨expr⟩`.
  - When used directly around the right-hand side of an un-declared recursive binding `(let ((f (the ⟨type⟩ (fn ...)))) ...)`, the annotation is visible to recursive calls within `f`.
- **In [`catch`](/manual/operators/catch/) and [`handle`](/manual/operators/handle/) branches**:
  - A branch head written `((the ⟨exception-type⟩ ⟨var-or-_⟩) ⟨handler-body⟩+)` is pattern syntax rather than an expression: it matches any thrown exception of `⟨exception-type⟩` (which must be a concrete exception type with no type variables) and binds the exception value to `⟨var-or-_⟩`.

## Examples

```lisp
;; Fix the element and collection type of a builder literal
(coalton (the (Vector F32) [1 2 3 4]))
; => #(1.0 2.0 3.0 4.0)

;; Resolve an ambiguous target type in a conversion
(coalton (+ 1 (the Integer (unwrap (try-into "41")))))
; => 42

;; Catch any exception of type `BadEgg` and bind the exception value
(coalton-toplevel
  (define-exception BadEgg
    BrokenEgg
    RottenEgg)

  (declare inspect-egg ((Void -> Unit) -> (Result BadEgg Unit)))
  (define (inspect-egg check)
    (catch (Ok (check))
      ((the BadEgg e) (Err e)))))
```

## Other notes

- Unlike Common Lisp's `cl:the`, which is an unchecked trust declaration in many Lisp compilers, Coalton's `the` is statically verified by unification at compile time.
- See [`declare`](/manual/operators/declare/), [`forall`](/manual/operators/forall/), [`as`](/manual/operators/as/), and [`catch`](/manual/operators/catch/).

