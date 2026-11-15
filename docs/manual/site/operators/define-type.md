---
title: "define-type"
description: "Algebraic data type definition form."
hideMeta: true
weight: 50
---

`define-type` defines a nominal algebraic data type and its value constructors.

## Syntax

```lisp
⟨repr-directive⟩?
⟨derive-directive⟩?
(define-type ⟨type-head⟩
  ⟨docstring⟩?
  ⟨constructor-spec⟩*)

;; ⟨type-head⟩        := ⟨type-name⟩
;;                     | (⟨type-name⟩ ⟨type-var⟩+)
;; ⟨constructor-spec⟩ := ⟨ctor-name⟩ ⟨docstring⟩?
;;                     | (⟨ctor-name⟩ ⟨field-type⟩*) ⟨docstring⟩?
;; ⟨repr-directive⟩   := (repr :enum | :transparent | :lisp | :native ⟨lisp-type⟩)
;; ⟨derive-directive⟩ := (derive ⟨class-name⟩+)
```

## Semantics

- `define-type` is a top-level form valid inside [`coalton-toplevel`](/manual/operators/coalton-toplevel/). Both `⟨type-name⟩` and all `⟨ctor-name⟩` symbols must belong to the current package.
- A non-parametric type is written as a bare symbol `⟨type-name⟩`; a parametric type is written `(⟨type-name⟩ :a :b ...)` with one or more distinct keyword type variables.
- Every type variable appearing in a constructor field type must be declared in `⟨type-head⟩`.
- Kinds of type parameters are inferred from their uses in constructor fields (including higher-kinded parameters such as `:f` used as `(:f :a)`). The resulting type constructor produces a type of kind `Type`.
- Variance of each type parameter is inferred automatically across mutually recursive type definitions. Unused (phantom) type parameters default to covariant; opaque types with zero constructors default to invariant.
- Each `⟨constructor-spec⟩` defines a constructor in the current package:
  - A nullary constructor written `C` or `(C)` is a value of the defined type.
  - A constructor `(C t1 ... tn)` with $n \ge 1$ fields is a function of type `(t1 * ... * tn -> ⟨type⟩)` that can be called or passed as a first-class function value.
  - An optional constructor docstring string literal may follow the constructor form.
  - In [`match`](/manual/operators/match/) and other patterns, constructors must always be parenthesized: `(C)` for nullary constructors and `(C p1 ... pn)` for constructors with fields.
- A `define-type` may have zero constructors to represent an uninhabited type or, with `(repr :native ⟨lisp-type⟩)`, an opaque foreign Common Lisp type.
- Preceding [`repr`](/manual/operators/repr/) and [`derive`](/manual/operators/derive/) attributes may be supplied in either order (at most one of each).

## Examples

```lisp
(coalton-toplevel
  ;; Enumeration with derived Eq and Show
  (derive Eq Show)
  (repr :enum)
  (define-type Direction
    "Cardinal compass directions."
    North "Due north."
    South "Due south."
    East  "Due east."
    West  "Due west.")

  ;; Parametric recursive algebraic data type
  (derive Eq Show)
  (define-type (Tree :a)
    (Leaf :a)
    (Branch (Tree :a) (Tree :a)))

  (declare tree-sum (Num :a => (Tree :a) -> :a))
  (define (tree-sum t)
    (match t
      ((Leaf x) x)
      ((Branch l r) (+ (tree-sum l) (tree-sum r))))))
```

## Other Notes

- By default, `define-type` compiles to CLOS classes in development mode (supporting interactive redefinition) and frozen `cl:defstruct` definitions in release mode. Use [`repr`](/manual/operators/repr/) to select `:enum`, `:transparent`, `:lisp`, or `:native` representations explicitly.
- In release mode, the compiler can eliminate locally constructed data type values—including single-constructor types such as `Tuple` and sum types such as `Optional` and `Result` produced across local `if`, `cond`, and `match` branches—when they are consumed only by local pattern matching. Constructor arguments still evaluate once, in left-to-right order. See [Performance and Optimization](/manual/topics/performance-and-optimization/).
- Type and constructor names are conventionally written in `CamelCase`.

