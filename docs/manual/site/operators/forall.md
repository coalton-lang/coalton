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
(forall (⟨binder⟩*) ⟨qualified-type⟩)
(∀ (⟨binder⟩*) ⟨qualified-type⟩)

;; ⟨binder⟩ := :⟨var⟩
;;           | (:⟨var⟩ ⟨kind⟩)
;; ⟨kind⟩   := Type
;;           | Values
;;           | (⟨kind⟩ -> ⟨kind⟩ [-> ⟨kind⟩ ...])
```

The quantified body can be parenthesized or written as the rest of the `forall` form:

```lisp
(forall (:a) (:a -> :a))
(forall (:a) :a -> :a)
(forall (:a) (Ord :a => :a * :a -> :a))
```

`Type` and `Values` are exported symbols in the `#:coalton` package, not keyword
symbols. `Values` is the same symbol as the [`values`](/manual/operators/values/) operator;
its meaning here is determined by its position in a binder. Coalton also accepts
the Unicode alias `∀` for `forall`.

## Semantics

- `forall` explicitly quantifies the listed type variables.
- `forall` may appear both at the outer level of a [`declare`](/manual/operators/declare/), [`the`](/manual/operators/the/), or class method signature, and in nested type positions such as function arguments, function return types, [`define-type`](/manual/operators/define-type/) constructor fields, [`define-struct`](/manual/operators/define-struct/) fields, and [`define-type-alias`](/manual/operators/define-type-alias/) definitions (*rank-$N$ polymorphism*).
- Those binder names are then available inside related `the`, `declare`, and
  `lisp` annotations in the corresponding body.
- `forall` expressions inside of `define-class` are scoped to their method
  definitions in `define-instance`.
- Without `forall`, top-level declarations are still implicitly quantified (rank-1),
  but the names are not scoped into the body. Nested polymorphic types (rank-2 and
  higher) must always be written with an explicit `forall`.
- An inner declaration's explicit binder shadows an outer binder with the same
  name, including its kind. Directly consecutive `forall` forms within a single
  type are flattened into one binder list and cannot repeat a binder name.
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

## Higher-Rank Polymorphism

A `forall` type can appear inside function inputs, function outputs, and data
constructor or struct fields. Higher-rank functions must have an explicit type
declaration (`declare` or `the`), while callers can pass either polymorphic
functions or inline [`fn`](/manual/operators/fn/) lambdas without extra annotations.

```lisp
(coalton-toplevel
  ;; A Rank-2 function that applies a polymorphic function to two different types
  (declare apply-both ((forall (:a) (:a -> :a)) * :x * :y -> (Tuple :x :y)))
  (define (apply-both f x y)
    (Tuple (f x) (f y)))

  ;; A Rank-2 automatic differentiation operator using Dual numbers
  (declare differentiate ((forall (:a) (Num :a => (:a -> :a))) -> (F32 -> F32)))
  (define (differentiate f)
    (fn (x)
      (dual-part (f (Dual x 1)))))

  ;; A Rank-2 natural transformation between type constructors :f and :g
  (declare map-pair
    (forall (:f :g)
      ((forall (:a) ((:f :a) -> (:g :a)))
       * (:f Integer)
       * (:f String)
       -> (Tuple (:g Integer) (:g String)))))
  (define (map-pair nat xs ys)
    (Tuple (nat xs) (nat ys))))
```

Coalton's higher-rank polymorphism is **predicative**: type variables and type
constructor arguments only ever instantiate to *monotypes* (types that do not
contain `forall`), never to *polytypes* (`forall` types).

### Implications of Predicative Polymorphism

Being predicative determines where `forall` types can appear and how they
interact with generic functions:

What is supported directly:

- **Higher-rank arguments and results:** A `forall` type can be any positional
  or keyword input or the return type of a function, nested to any depth
  (Rank-2, Rank-3, and higher).
- **Unannotated callers:** When a function's parameter is declared with a
  `forall` type, callers can pass either a named polymorphic function (such as
  `id` or `square`) or an unannotated [`fn`](/manual/operators/fn/) lambda
  directly, including across [`if`](/manual/operators/if/) and
  [`match`](/manual/operators/match/) branches.
- **Fields of nominal types:** [`define-type`](/manual/operators/define-type/)
  constructors and [`define-struct`](/manual/operators/define-struct/) fields
  can hold `forall` types directly.
- **Type aliases for higher-rank signatures:** [`define-type-alias`](/manual/operators/define-type-alias/)
  can name a `forall` type for reuse in function signatures and nominal type
  fields.

What is not supported:
- **Applying a type constructor directly to a `forall` type:** A parameterized
  type such as `List`, `Vector`, `Optional`, or `Tuple` cannot take a `forall`
  type as a type argument (even through a `define-type-alias`). Wrap the
  `forall` in a `define-type` or `define-struct` first:
  ```lisp
  ;; Rejected: List cannot be applied to a `forall` type directly
  ;; (declare fns (List (forall (:a) (Num :a => (:a -> :a)))))

  ;; Accepted: wrap the `forall` in a nominal type
  (coalton-toplevel
    (define-type PolyBox
      (PolyBox (forall (:a) (Num :a => (:a -> :a)))))

    (declare fns (List PolyBox))
    (define fns
      (make-list (PolyBox (fn (x) (+ x 1)))
                 (PolyBox (fn (x) (* x x))))))
  ```
- **Passing higher-rank values through rank-1 combinators:** Because an
  ordinary type variable `:t` only unifies with monotypes, a rank-1 combinator
  such as `id` (`:t -> :t`) or `pipe` cannot instantiate `:t` to a `forall`
  type or a function taking a `forall` type. Call the higher-rank function
  directly, or wrap it in a nominal type when storing or forwarding it through
  generic containers.
- **Inferring higher-rank parameters without a declaration:** Unannotated
  function parameters are inferred as monotypes. If a function parameter is
  used polymorphically inside the body (for example, applied to both `Integer`
  and `String`), the function must have an explicit [`declare`](/manual/operators/declare/)
  or [`the`](/manual/operators/the/) annotation.
- **Generalizing expansive mutable allocations:** An unconstrained `forall`
  type must satisfy Coalton's value restriction; allocating a mutable
  container such as `(cell:new (fn (x) x))` cannot be checked at type
  `(forall (:a) (Cell (:a -> :a)))`.

## Examples

```lisp
(coalton-toplevel
  ;; Use scoped type variable `:a` in a local `declare` and `the` expression
  (declare pair-with-self (forall (:a) :a -> (Tuple :a (List :a))))
  (define (pair-with-self x)
    (let ((declare xs (List :a))
          (xs (the (List :a) [x x])))
      (Tuple x xs))))
```

## Other Notes

- Inference can still discover result polymorphism for functions without a type
  declaration. Printed schemes show result binders as `(:r Values)` and show
  ordinary arrow kinds explicitly. For example, an inferred forwarding function
  may print as `∀ (:A Values). (Void → :A) → :A` (or `forall (:A Values). (Void -> :A) -> :A` when Unicode printing is disabled). The dot is part of the
  type display; its declaration syntax is `(forall ((:a Values)) (Void -> :a) -> :a)`.
- Unannotated type inference remains rank-1 (Hindley–Milner), while explicit
  `forall` annotations support arbitrary-rank predicative polymorphism.

