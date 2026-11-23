---
title: "define-type-alias"
description: "Type alias definition form."
hideMeta: true
weight: 60
---

`define-type-alias` defines a transparent synonym for an existing type or type constructor.

## Syntax

```lisp
;; Non-parametric type alias
(define-type-alias ⟨alias-name⟩
  ⟨aliased-type⟩
  ⟨docstring⟩?)

;; Parametric type alias
(define-type-alias (⟨alias-name⟩ ⟨type-var⟩+)
  ⟨aliased-type⟩
  ⟨docstring⟩?)
```

## Semantics

- `define-type-alias` is a top-level form valid inside [`coalton-toplevel`](/manual/operators/coalton-toplevel/).
- It does not create a distinct nominal type: `⟨alias-name⟩` (applied to its parameters `⟨type-var⟩+`) expands directly to `⟨aliased-type⟩` and unifies with `⟨aliased-type⟩` everywhere.
- Because an alias is the same type as its expansion, you cannot define separate typeclass instances for both an alias and its underlying type.
- In a parametric alias `(⟨alias-name⟩ ⟨type-var⟩+)`:
  - Every `⟨type-var⟩` declared in the head must appear at least once in `⟨aliased-type⟩` (unused parameters are a compile-time error).
  - Every type variable appearing in `⟨aliased-type⟩` must be declared in the head.
  - All declared type parameters must be supplied whenever the alias is used, though `⟨aliased-type⟩` itself may have a higher kind (such as `Type -> Type`).
- Type aliases cannot be recursive or mutually recursive unless broken by an intervening nominal type; cyclic alias definitions are rejected at compile time.
- `define-type-alias` does not accept [`repr`](/manual/operators/repr/) or [`derive`](/manual/operators/derive/) attributes.
- If a `⟨docstring⟩` is provided, it must appear *after* `⟨aliased-type⟩`.

## Examples

```lisp
(coalton-toplevel
  ;; Parametric alias for homogeneous pairs
  (define-type-alias (Pair :a)
    (Tuple :a :a)
    "A tuple of two elements of the same type.")

  ;; Higher-kinded alias: StringResult has kind (Type -> Type)
  (define-type-alias StringResult
    (Result String)
    "A Result whose error type is fixed to String.")

  (declare swap-pair (Pair :a -> Pair :a))
  (define (swap-pair (Tuple x y))
    (Tuple y x))

  (declare parse-positive (String -> StringResult Integer))
  (define (parse-positive _s)
    (Ok 1)))
```

## Other notes

- Coalton records alias annotations alongside expanded types so REPL inspection tools ([`describe-type-of`](/manual/operators/describe-type-of/), [`describe-type-alias`](/manual/operators/describe-type-alias/), and [`set-type-printing-mode`](/manual/operators/set-type-printing-mode/)) can display both the alias name and the underlying type.
- When you need a zero-overhead distinct type with its own typeclass instances rather than a synonym, use [`define-type`](/manual/operators/define-type/) or [`define-struct`](/manual/operators/define-struct/) with `(repr :transparent)`.

