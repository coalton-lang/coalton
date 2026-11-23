---
title: "match"
description: "Pattern matching form."
hideMeta: true
weight: 150
---

`match` evaluates a scrutinee expression, tests it against a sequence of patterns in order, and evaluates the body of the first matching branch.

## Syntax

```lisp
(match ⟨scrutinee-expr⟩
  (⟨pattern⟩ ⟨body-element⟩* ⟨expr⟩)
  ...)

;; ⟨pattern⟩ := ⟨literal⟩
;;            | ⟨var⟩
;;            | _
;;            | (= ⟨var⟩ ⟨pattern⟩)
;;            | (⟨ctor-name⟩ ⟨pattern⟩*)
```

## Semantics

- `⟨scrutinee-expr⟩` is evaluated once and must produce a single value.
- Branches are tested top-to-bottom. The first branch whose `⟨pattern⟩` matches the scrutinee is selected, its pattern variables are bound, and its body is evaluated.
- **Pattern forms**:
  - **Constructor patterns**: `(⟨ctor-name⟩ ⟨pattern⟩*)` matches a constructor of an algebraic data type or struct. Constructor patterns must **always** be parenthesized, including nullary constructors such as `(None)` or `(Nil)`. Writing an unparenthesized `None` or `Nil` creates a variable pattern that matches anything and binds the variable `None` or `Nil`!
  - **Variable patterns**: A bare identifier `⟨var⟩` (not `_` and not `*...*`) matches any value and binds `⟨var⟩` across the branch body. All variables bound within a single pattern must be distinct.
  - **Wildcard pattern**: [`_`](/manual/operators/wildcard/) matches any value without binding a variable.
  - **Alias (`=`) patterns**: `(= ⟨var⟩ ⟨pattern⟩)` matches `⟨pattern⟩` and also binds `⟨var⟩` to the entire subvalue matched by `⟨pattern⟩` (see [`=`](/manual/operators/pattern-bind/)).
  - **Literal patterns**: Numeric, character, and string literals match equal values. Integer literals in patterns are overloaded via `(Num :a) (Eq :a) => ==` and `fromInt`, so they work on any numeric type implementing `Num` and `Eq`.
- Each branch body is an implicit [`progn`](/manual/operators/progn/) (supporting multiple expressions and shorthand `let`).
- All branch bodies must unify to the same result type, which may be a single value, `Void`, or multiple values.
- The final expression of each branch body is in tail position when the `match` expression is in tail position (including for [`rec`](/manual/operators/rec/) tail calls).
- **Exhaustiveness**: The compiler checks pattern exhaustiveness and emits a warning at compile time if cases are missing. If a non-exhaustive `match` falls through at runtime without matching any branch, it signals an error.
- **GADT patterns**: Matching a constructor declared inside [`define-type`](/manual/operators/define-type/) can refine the scrutinee's type arguments inside that branch. Coverage starts from the incoming scrutinee type: a constructor returning `Tag String` is not required for a `Tag Integer`, but it cannot be omitted merely because another branch matches `Tag Integer`.
- Function parameter patterns are checked together. For example, if one parameter matches a witness that `:a` is `Integer`, a second parameter of type `Tag :a` need only cover `Tag Integer`.
- A constructor can also be ruled out when a required field cannot exist, for example a product containing an equality witness for `Integer` and `String`. This search is bounded: if recursive or abstract types prevent a proof, the checker still requires the case.
- Within a branch, an equality witness also applies to scoped annotations and existing typeclass constraints. If `Refl` proves that `:a` and `:b` are equal, an `Eq :b` constraint suffices to compare a value of `:a` with one of `:b`. The equality does not hold outside that branch.
- A match result can use an enclosing index that is equal to the branch result type in every arm. Naming the result with `let` does not make it polymorphic in that index: all uses of the binding must agree on a type. An existential field cannot be returned unless the result can be expressed without exposing the hidden type.
- A shorthand `(let pattern = value)` has the same refinement scope as a one-arm match whose body is the rest of the block. Unlike an explicit `match`, the binding pattern must be exhaustive.

## Examples

```lisp
(coalton-toplevel
  ;; Matching an algebraic data type with nested and alias patterns
  (declare describe-top ((List (Optional Integer)) -> String))
  (define (describe-top xs)
    (match xs
      ((Cons (Some 0) _)          "starts with Some 0")
      ((Cons (= opt (Some _)) _)  (<> "starts with " (show-as-string opt)))
      ((Cons (None) _)            "starts with None")
      ((Nil)                      "empty"))))

;; Common pitfall: always parenthesize nullary constructors!
;; (match opt
;;   ((Some x) x)
;;   (None     0))  ; <-- BUG: `None` is a variable pattern here; write `((None) 0)`!
```

## Other notes

- Matching on a `Boolean` with `(True)` and `(False)` branches compiles directly to `cl:if`. Matching on a [`(repr :enum)`](/manual/operators/repr/) type compiles to a `cl:case` jump table.
- In release mode, matching on a locally constructed data type or struct—including values produced across local `if`, `cond`, and `match` branches—can eliminate the intermediate allocation and keep extracted fields in typed local bindings. When a `match` scrutinizes a freshly constructed value whose pattern bindings do not escape, the scrutinee is also declared `dynamic-extent`. See [Performance and Optimization](/manual/topics/performance-and-optimization/).

