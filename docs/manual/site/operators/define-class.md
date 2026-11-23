---
title: "define-class"
description: "Type class definition form."
hideMeta: true
weight: 80
---

`define-class` defines a type class, its superclass constraints, functional dependencies, and method signatures.

## Syntax

```lisp
(define-class (⟨superclass-context⟩? ⟨class-name⟩ ⟨type-var⟩+ ⟨fundep⟩*)
  ⟨docstring⟩?
  ⟨method-decl⟩*)

;; ⟨superclass-context⟩ := ⟨predicate⟩ =>
;;                       | (⟨predicate⟩)+ =>
;; ⟨predicate⟩          := ⟨superclass-name⟩ ⟨type⟩+
;; ⟨fundep⟩             := (⟨type-var⟩+ -> ⟨type-var⟩+)
;; ⟨method-decl⟩        := (⟨method-name⟩ ⟨docstring⟩? ⟨qualified-type⟩)
```

## Semantics

- `define-class` is a top-level form valid inside [`coalton-toplevel`](/manual/operators/coalton-toplevel/). `⟨class-name⟩` and all `⟨method-name⟩` symbols must belong to the current package.
- A class must have one or more distinct keyword type parameters `⟨type-var⟩+`. Parameter kinds (`Type`, `Type -> Type`, etc.) are inferred from superclass predicates and method signatures.
- **Superclasses**: Any predicates before [`=>`](/manual/operators/constraint-arrow/) are superclass requirements. Defining an instance of `⟨class-name⟩` requires instances for all superclasses to be available. Cyclic superclass hierarchies are rejected.
- **Functional dependencies**: Each `(⟨from-var⟩+ -> ⟨to-var⟩+)` clause declares that the types bound to `⟨from-var⟩+` uniquely determine `⟨to-var⟩+`. The compiler uses functional dependencies to improve types during inference and rejects instances that violate determinism. A class's functional dependencies cannot be changed by redefinition.
  - A superclass can determine a type that is not a parameter of the subclass. For example, if `A :x :y` declares `(:x -> :y)`, two subclasses requiring `A :x :y` agree on `:y` whenever they are used with the same `:x`. Neither subclass needs to expose `:y` as a parameter.
- **Methods**: Each `(⟨method-name⟩ ⟨docstring⟩? ⟨qualified-type⟩)` declares a generic function in the current package whose full type adds `(⟨class-name⟩ ⟨type-var⟩+)` to the front of `⟨qualified-type⟩`'s constraint context.
  - Every class type parameter must be reachable from each method's type (directly or via functional dependencies).
  - Method types may introduce additional type variables, additional constraints, keyword parameters ([`&key`](/manual/operators/keyword-tail/)), and explicit [`forall`](/manual/operators/forall/) binders.
  - Explicit `forall` binders in a method declaration are scoped into the corresponding method definition inside [`define-instance`](/manual/operators/define-instance/).
- **Marker classes**: A class with zero methods is permitted and acts as a type-level predicate that must still be instantiated with `define-instance`.

## Examples

```lisp
(coalton-toplevel
  ;; Single-parameter class with a superclass constraint
  (define-class (Eq :a => Measurable :a)
    "Types with a non-negative integer size."
    (measure "Return the size of a value." (:a -> UFix)))

  ;; Multi-parameter class with a functional dependency:
  ;; the collection type `:c` uniquely determines the element type `:e`.
  (define-class (Collectable :c :e (:c -> :e))
    (empty-collection (Void -> :c))
    (insert-item (:c * :e -> :c)))

  ;; Marker class with no methods
  (define-class (Sendable :a)
    "Marks types that may cross a channel boundary."))
```

## Other notes

- Coalton compiles type classes using dictionary passing: each class compiles to a struct/class holding its superclass dictionaries and method functions, and constrained functions receive the required dictionaries as hidden arguments.
- See [`define-instance`](/manual/operators/define-instance/), [`derive`](/manual/operators/derive/), and [`overlap`](/manual/operators/overlap/).

