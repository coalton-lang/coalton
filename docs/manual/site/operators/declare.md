---
title: "declare"
description: "Type declaration form."
hideMeta: true
weight: 30
---

`declare` specifies the static type of a top-level definition or local binding.

## Syntax

```lisp
;; Top-level declaration (inside coalton-toplevel)
⟨opt-attribute⟩*
(declare ⟨name⟩ ⟨qualified-type⟩)

;; Local declaration (inside let, let*, rec, for, or for* binding list)
(declare ⟨name⟩ ⟨qualified-type⟩)

;; ⟨opt-attribute⟩ := (inline) | (monomorphize)
```

## Semantics

- At top level inside [`coalton-toplevel`](/manual/operators/coalton-toplevel/), `declare` specifies the type scheme of a [`define`](/manual/operators/define/) form with the same `⟨name⟩` in that translation unit.
- Inside the binding list of [`let`](/manual/operators/let/), [`let*`](/manual/operators/let-star/), [`rec`](/manual/operators/rec/), [`for`](/manual/operators/for/), or [`for*`](/manual/operators/for-star/), `(declare ⟨name⟩ ⟨qualified-type⟩)` constrains the local binding `⟨name⟩` in that same binding list.
- Every `declare` must pair with a definition in the same scope, and at most one `declare` may be given per binding; orphan or duplicate declarations are compile-time errors.
- `⟨qualified-type⟩` may be a monomorphic type, a constrained type using [`=>`](/manual/operators/constraint-arrow/), or an explicitly quantified type using [`forall`](/manual/operators/forall/).
- Unquantified type variables (`:a`, `:b`, ...) in `⟨qualified-type⟩` are implicitly universally quantified over the declaration, except in local declarations where type variables already in scope from an enclosing `forall` refer to those outer scoped type variables.
- When `⟨qualified-type⟩` begins with an explicit [`forall`](/manual/operators/forall/), the listed type variables are brought into lexical scope across the corresponding definition body (for use in inner `declare`, [`the`](/manual/operators/the/), and [`lisp`](/manual/operators/lisp/) forms).
- The declared type is verified against the inferred type of the definition:
  - The declared type may specialize a more general inferred type (for example, declaring `Integer -> Integer` for `(+ x 1)`), but it cannot be more general than the inferred type.
  - All non-defaultable typeclass constraints required by the body must be entailed by the declared constraint context.
- A binding cannot be declared with a bare `Void` or multi-value pack `T1 * T2` type; declarations must be single-value types or function types (such as `Void -> Void` or `Integer -> Integer * Integer`).

## Examples

```lisp
(coalton-toplevel
  ;; Constrained polymorphic declaration
  (declare clamp (Ord :a => :a * :a * :a -> :a))
  (define (clamp lo hi x)
    (max lo (min hi x)))

  ;; Explicit `forall` brings `:a` into scope inside the body
  (declare singleton-vec (forall (:a) :a -> (Vector :a)))
  (define (singleton-vec x)
    (the (Vector :a) [x]))

  ;; Local `declare` inside `let` and `for`
  (declare sum-squares (UFix -> UFix))
  (define (sum-squares n)
    (let ((declare scale UFix)
          (scale 2))
      (for ((declare i UFix)
            (declare acc UFix)
            (i 0 (+ i 1))
            (acc 0 (+ acc (* scale (* i i)))))
        :returns acc
        :while (< i n)))))
```

## Other Notes

- Exported top-level `define` forms without a corresponding `declare` emit a `coalton:deprecation-warning` (planned to become a compile-time error in a future release).
- Top-level optimization attributes [`inline`](/manual/operators/inline/) and [`monomorphize`](/manual/operators/monomorphize/) may be placed immediately before either the `declare` form or the `define` form.

