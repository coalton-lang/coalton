---
title: "forall"
description: "Explicit type-variable quantification and kind annotations."
hideMeta: true
weight: 30
---

`forall` introduces explicit type-variable binders in a type declaration, also
commonly known as *scoped type variables*. Coalton can infer polymorphism without
it, but `forall` is useful when you want the type-variable names themselves to be
part of the declaration.

## Syntax

```lisp
(forall (⟨binder⟩...) ⟨type⟩)

;; Each binder is either a keyword or a keyword with a kind annotation:
:a
(:a Type)
(:f (Type -> Type))
(:r Values)
```

The body can be grouped or written as the rest of the `forall` form:

```lisp
(forall (:a) (:a -> :a))
(forall (:a) :a -> :a)
```

`Type` and `Values` are exported symbols in the `coalton` package, not keyword
symbols. `Values` is the same symbol as the existing `values` operator;
its meaning here is determined by its position in a binder. Coalton also accepts
the Unicode alias `∀` for `forall`.

## Semantics

- `forall` explicitly quantifies the listed type variables.
- Those binder names are then available inside related `the`, `declare`, and
  `lisp` annotations in the corresponding body.
- `forall` expressions inside of `define-class` are scoped to their method
  definitions in `define-instance`.
- Without `forall`, declarations are still implicitly quantified, but the names
  are not scoped into the body.
- An inner declaration's explicit binder shadows an outer binder with the same
  name, including its kind. Nested `forall` forms within a single type are
  flattened into one binder list and cannot repeat a binder name.
- An explicitly quantified type may also refer to enclosing scoped variables.
  Any other type variable must appear in its binder list.

## Kinds

A kind describes how a type can be used. `Type` describes ordinary single-value
types, such as `Integer`, `String`, and `Tuple Integer String`. A function type
also has kind `Type`, even when calling the function returns multiple values.

An arrow kind describes a type constructor. For example, `List` has kind
`Type -> Type`, and `Tuple` has kind `Type -> Type -> Type`.

```lisp
(declare keep-container
  (forall ((:f (Type -> Type)) (:a Type))
    (:f :a) -> (:f :a)))
(define (keep-container x) x)
```

Kind arrows associate to the right: `(Type -> Type -> Type)` means
`(Type -> (Type -> Type))`. Parentheses can also describe higher-order kinds,
such as `((Type -> Type) -> Type)`.

An annotation constrains kind inference. For example, using `(:f :a)` after
binding `(:f Type)` is an error. An unannotated binder retains ordinary kind
inference, so `:f` can infer `Type -> Type`. An unconstrained kind defaults to
`Type`. Thus a bare binder is not always equivalent to a binder annotated with
`Type`.

Supported annotations are `Type`, arrow kinds built from `Type`, and the
standalone annotation `Values`. Arrows containing `Values`, such as
`(Values -> Type)`, are currently rejected. Kind variables and kind polymorphism
are not supported. The [`kind-of`](/manual/operators/kind-of/) helper currently
prints `Type` as `*`.

## Result polymorphism with `Values`

`Values` binds a variable representing an entire result sequence: zero values,
one value, or several values. Use it to declare functions that forward results:

```lisp
(declare call-results
  (forall ((:r Values)) (Void -> :r) -> :r))
(define (call-results f) (f))

(call-results (fn () (values)))             ; zero values
(call-results (fn () 42))                   ; one Integer
(call-results (fn () (values 42 "answer"))) ; Integer and String
```

Each call instantiates `:r` with one fixed sequence of result types. This does
not permit different branches of a function to return incompatible result
sequences. A single `Tuple Integer String` remains one value; it is distinct
from the two results `Integer * String`.

A `Values` variable must occupy an entire function output slot or a scoped
[`lisp`](/manual/operators/lisp/) output annotation. It cannot be an ordinary
argument, a keyword argument's type, a type argument such as `List :r`, a class
predicate argument, or a component of a result sequence such as `Integer * :r`.
Result sequences cannot be concatenated or spliced using these binders.

Explicit binders are also available in the function body:

```lisp
(declare call-results-through-lisp
  (forall ((:r Values)) (Void -> :r) -> :r))
(define (call-results-through-lisp f)
  (lisp (-> :r) (f)
    (call-coalton-function f)))
```

An ordinary variable, whether implicitly quantified, a bare explicit binder, or
annotated `Type`, cannot stand for zero or multiple values. For example:

```lisp
(declare call-one (forall (:a) (Void -> :a) -> :a))
(define (call-one f) (f))

(call-one (fn () 42))                   ; accepted
(call-one (fn () (values 42 "answer"))) ; type error
```

Inference can still discover result polymorphism for functions without a type
declaration. Printed schemes show result binders as `(:r Values)` and show
ordinary arrow kinds explicitly. For example, an inferred forwarding function
may print as `forall (:A Values). (Void -> :A) -> :A`. The dot is part of the
type display; its declaration syntax is `(forall ((:a Values)) (Void -> :a) -> :a)`.
