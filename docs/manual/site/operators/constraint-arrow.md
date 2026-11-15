---
title: "=>"
description: "Constraint arrow for qualified types and class contexts."
hideMeta: true
weight: 20
---

`=>` separates type class constraints (predicates) from the constrained type, class head, or instance head, and also separates keys from values in [`[=>]`](/manual/operators/association-builder/) association builders.

## Syntax

```lisp
;; Single constraint in a qualified type
(⟨class⟩ ⟨type⟩+ => ⟨constrained-type⟩)

;; Multiple constraints in a qualified type
((⟨class-1⟩ ⟨type⟩+) (⟨class-2⟩ ⟨type⟩+) ... => ⟨constrained-type⟩)

;; Superclass constraints in `define-class`
(define-class (⟨predicate⟩ => ⟨class-name⟩ ⟨type-var⟩+ ⟨fundep⟩*) ...)
(define-class ((⟨pred-1⟩) (⟨pred-2⟩) ... => ⟨class-name⟩ ⟨type-var⟩+ ⟨fundep⟩*) ...)

;; Instance context constraints in `define-instance`
(define-instance (⟨predicate⟩ => ⟨class-name⟩ ⟨type⟩+) ...)
(define-instance ((⟨pred-1⟩) (⟨pred-2⟩) ... => ⟨class-name⟩ ⟨type⟩+) ...)

;; Key-value separator in association builders
[=>]
[⟨key-expr⟩ => ⟨value-expr⟩ ...]
```

## Semantics

- **In qualified types** ([`declare`](/manual/operators/declare/) and [`define-class`](/manual/operators/define-class/) method signatures):
  - Everything to the left of `=>` is the constraint context (one unparenthesized predicate `C :a ...`, or one or more parenthesized predicates `(C1 :a) (C2 :b) ...`).
  - Everything to the right of `=>` is the constrained type (which need not be wrapped in extra parentheses when it is a function type, e.g., `(Ord :a => :a * :a -> Boolean)`).
  - When combined with [`forall`](/manual/operators/forall/), `forall` wraps the outside of the qualified type: `(forall (:a) (Ord :a => :a * :a -> :a))`.
  - Inline [`the`](/manual/operators/the/) annotations do not permit `=>` predicate contexts.
- **In [`define-class`](/manual/operators/define-class/)**:
  - Predicates to the left of `=>` declare superclasses that every instance of the class must also implement.
- **In [`define-instance`](/manual/operators/define-instance/)**:
  - Predicates to the left of `=>` declare prerequisite constraints on the instance's type variables that must hold for the instance to apply.
- **In [`[=>]`](/manual/operators/association-builder/)**:
  - Inside square brackets, `=>` separates each key expression from its value expression, or denotes an empty association builder `[=>]`.

## Examples

```lisp
(coalton-toplevel
  ;; Single and multiple typeclass constraints in `declare`
  (declare clamp (Ord :a => :a * :a * :a -> :a))
  (define (clamp lo hi x)
    (max lo (min hi x)))

  (declare show-equal? ((Eq :a) (Show :a) => :a * :a -> String))
  (define (show-equal? a b)
    (if (== a b)
        (show-as-string a)
        "different"))

  ;; Superclass constraint in `define-class` and context in `define-instance`
  (define-class (Eq :a => PrintableEq :a)
    (describe-eq (:a * :a -> String)))

  (define-instance ((Eq :a) (Show :a) => PrintableEq :a)
    (define (describe-eq x y)
      (show-equal? x y))))
```

## Other Notes

- At compile time, each predicate to the left of `=>` in a value or instance declaration becomes a hidden typeclass dictionary parameter passed to the compiled function or instance constructor.
- When Unicode type printing is enabled, Coalton displays `=>` as `⇒` in printed types and instance listings, while source code uses `=>`.

