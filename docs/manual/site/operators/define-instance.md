---
title: "define-instance"
description: "Type class instance definition form."
hideMeta: true
weight: 90
---

`define-instance` implements a type class for a specific type or constrained type pattern.

## Syntax

```lisp
(overlap)?
(define-instance (⟨instance-context⟩? ⟨class-name⟩ ⟨type⟩+)
  ⟨docstring⟩?
  ⟨method-def⟩*)

;; ⟨instance-context⟩ := ⟨predicate⟩ =>
;;                     | (⟨predicate⟩)+ =>
;; ⟨method-def⟩       := (inline)?
;;                       (define ⟨method-name⟩ ⟨body-element⟩* ⟨expr⟩)
;;                     | (inline)?
;;                       (define (⟨method-name⟩ ⟨arg⟩* [&key (⟨kw-var⟩ ⟨default-expr⟩)*])
;;                         ⟨body-element⟩*
;;                         ⟨expr⟩)
```

## Semantics

- `define-instance` is a top-level form valid inside [`coalton-toplevel`](/manual/operators/coalton-toplevel/).
- The instance head `(⟨class-name⟩ ⟨type⟩+)` specifies the class and type arguments being implemented; any predicates before [`=>`](/manual/operators/constraint-arrow/) form the instance context (prerequisite constraints on type variables in the head).
- Every method of `⟨class-name⟩` must be defined exactly once in the instance, and no unknown methods may appear.
- All superclass instances required by `⟨class-name⟩` must be satisfied by the global environment together with `⟨instance-context⟩`.
- **Type variable scoping**: Type variables appearing in the instance head or context, as well as any explicit [`forall`](/manual/operators/forall/) binders declared on the corresponding method in [`define-class`](/manual/operators/define-class/), are in lexical scope inside the method body.
- **No narrowing**: A method implementation must work for the full generality of the instance head; it is a compile-time error for a method body to unify two distinct instance type variables or specialize an instance type variable to a concrete type.
- **Functional dependencies**: Instance heads and contexts are checked for consistency and coverage against all functional dependencies of `⟨class-name⟩`.
- **Inlining**: Individual method definitions may be preceded by `(inline)` to request inlining when the instance dictionary is statically known.
- **Overlap**: Two instances for the same class whose heads can unify are rejected unless both carry the [`overlap`](/manual/operators/overlap/) attribute.

## Examples

```lisp
(coalton-toplevel
  (define-type Status
    Active
    Inactive)

  (define-type (Box :a)
    (Box :a))

  ;; Unconstrained instance for a concrete type
  (define-instance (Eq Status)
    (inline)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Active) (Active)) True)
        ((Tuple (Inactive) (Inactive)) True)
        (_ False))))

  ;; Constrained instance for a parameterized type
  (define-instance (Eq :a => Eq (Box :a))
    (define (== (Box x) (Box y))
      (== x y))))
```

## Other Notes

- Unconstrained instances compile to static global dictionary values; constrained instances compile to dictionary-constructor functions that accept the prerequisite dictionaries as arguments.
- Common standard library classes (`Eq`, `Hash`, `Default`, `Show`) can be generated automatically for [`define-type`](/manual/operators/define-type/) and [`define-struct`](/manual/operators/define-struct/) using [`derive`](/manual/operators/derive/).

