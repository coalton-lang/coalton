---
title: "->"
description: "Function type, kind, return-type, and functional-dependency arrow."
hideMeta: true
weight: 10
---

`->` constructs function types, [`lisp`](/manual/operators/lisp/) return-type specifications, arrow kinds in [`forall`](/manual/operators/forall/) binders, and functional dependencies in [`define-class`](/manual/operators/define-class/).

## Syntax

```lisp
;; Function type with positional and optional keyword inputs
(⟨input-types⟩ [&key (:⟨kw⟩ ⟨type⟩)*] -> ⟨output-spec⟩)

;; Function type with zero positional inputs
(Void -> ⟨output-spec⟩)
(&key (:⟨kw⟩ ⟨type⟩)+ -> ⟨output-spec⟩)

;; Inline `lisp` return type
(lisp (-> ⟨output-spec⟩) (⟨var-spec⟩*) ⟨lisp-form⟩+)

;; Arrow kind in a `forall` binder
(forall ((:⟨var⟩ (⟨kind-1⟩ -> ⟨kind-2⟩))) ⟨qualified-type⟩)

;; Functional dependency in `define-class`
(define-class (⟨class-name⟩ ⟨type-var⟩+ (⟨type-var⟩+ -> ⟨type-var⟩+)) ...)

;; ⟨input-types⟩ := ⟨type⟩ | ⟨input-types⟩ * ⟨type⟩
;; ⟨output-spec⟩ := Void
;;                | ⟨type⟩
;;                | ⟨type⟩ * ⟨type⟩ [* ⟨type⟩ ...]
;;                | ⟨input-spec⟩ -> ⟨output-spec⟩
```

## Semantics

- **In function types**:
  - `A -> B` is the type of a unary function taking `A` and returning `B`.
  - Multiple positional arguments are separated by [`*`](/manual/operators/star/) on the left of `->`: `A * B -> C` is a fixed-arity two-argument function called as `(f a b)`.
  - `->` is right-associative in output position: `A -> B -> C` means `A -> (B -> C)`, a unary function returning a unary function, called as `((f a) b)`.
  - `Void -> T` is a zero-argument function called as `(f)`. When keyword arguments are present on a function with zero positional inputs, omit `Void` and write `(&key (:k K) -> T)`.
  - `T -> Void` is a function that returns zero values (such as `(values)`).
  - `T -> A * B` is a function that returns multiple values via [`values`](/manual/operators/values/).
- **In [`lisp`](/manual/operators/lisp/) forms**:
  - `(-> ⟨output-spec⟩)` declares the return type(s) of the embedded Common Lisp body; nothing may appear to the left of `->`.
- **In [`forall`](/manual/operators/forall/) kind annotations**:
  - `(⟨kind-1⟩ -> ⟨kind-2⟩)` denotes the kind of a type constructor. Kind arrows associate to the right: `(Type -> Type -> Type)` is `(Type -> (Type -> Type))`.
- **In [`define-class`](/manual/operators/define-class/) functional dependencies**:
  - `(⟨from-var⟩+ -> ⟨to-var⟩+)` declares that the class type variables on the left uniquely determine those on the right.

## Examples

```lisp
(coalton-toplevel
  ;; Multi-argument function vs. curried function
  (declare add-pair (Integer * Integer -> Integer))
  (define (add-pair x y)
    (+ x y))

  (declare curried-add (Integer -> Integer -> Integer))
  (define (curried-add x)
    (fn (y) (+ x y)))

  ;; Nullary function returning multiple values
  (declare origin (Void -> Integer * Integer))
  (define (origin)
    (values 0 0))

  ;; Kind arrow in `forall`
  (declare map-id (forall ((:f (Type -> Type)) (:a Type))
                    (Functor :f => (:f :a) -> (:f :a))))
  (define (map-id xs)
    (map id xs)))
```

## Other notes

- Every function type `(A -> B)` has kind `Type`, even when its input is `Void` or its output is `Void` or a multi-value pack `B1 * B2`.
- When Unicode type printing is enabled, Coalton displays `->` as `→` in printed types and kinds, while source code uses `->`.
- See [`*`](/manual/operators/star/), [`&key`](/manual/operators/keyword-tail/), [`forall`](/manual/operators/forall/), and [`values`](/manual/operators/values/).

