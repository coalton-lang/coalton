---
title: "="
description: "Pattern binding operator and syntax token."
hideMeta: true
weight: 50
---

`=` is the pattern-aliasing ("as-pattern") operator that binds a name to an entire matched value while also destructuring it, and also serves as a delimiter token in shorthand [`let`](/manual/operators/let/) and comprehension `:with` clauses.

## Syntax

```lisp
;; Pattern alias ("as-pattern") in any pattern context
(= ⟨var⟩ ⟨pattern⟩)

;; Shorthand `let` in an expression body
(let ⟨pattern⟩ = ⟨expr⟩)
(let (values ⟨var-or-_⟩*) = ⟨expr⟩)
(let *⟨dynamic-var⟩* = ⟨expr⟩)

;; Lexical binding clause in a collection or association comprehension
[... :with ⟨var⟩ = ⟨expr⟩ ...]
```

## Semantics

- **In a pattern `(= ⟨var⟩ ⟨pattern⟩)`**:
  - Valid wherever patterns are accepted: [`match`](/manual/operators/match/) branches, [`define`](/manual/operators/define/) and [`fn`](/manual/operators/fn/) parameter lists, shorthand `(let ⟨pattern⟩ = ⟨expr⟩)`, and [`do`](/manual/operators/do/) binds `(⟨pattern⟩ <- ⟨expr⟩)`.
  - `⟨var⟩` must be an ordinary variable name (not `_` and not a dynamic variable `*...*`).
  - The value is matched against `⟨pattern⟩`; if `⟨pattern⟩` succeeds, `⟨var⟩` is bound to the entire value matched by `⟨pattern⟩` in addition to any variables bound inside `⟨pattern⟩`.
  - `⟨var⟩` and all variables inside `⟨pattern⟩` must be distinct.
- **In shorthand [`let`](/manual/operators/let/) and comprehension `:with` clauses**:
  - `=` is a syntactic separator between the left-hand pattern/binder and the right-hand initializer expression.

## Examples

```lisp
(coalton-toplevel
  ;; Keep the outer Tuple `tpl` while also extracting its first element `a`
  (declare nest-right ((Tuple :a :b) -> (Tuple :a (Tuple :a :b))))
  (define (nest-right (= tpl (Tuple a _)))
    (Tuple a tpl))

  ;; Alias a nested subpattern inside `match`
  (declare duplicate-nonempty ((List :a) -> (List :a)))
  (define (duplicate-nonempty xs)
    (match xs
      ((Cons x (= tail (Cons _ _)))
       (Cons x (Cons x tail)))
      (other
       other))))
```

## Other Notes

- `=` is purely a syntactic operator in Coalton; for value equality testing, use `==` from the `Eq` type class.
- See [`match`](/manual/operators/match/), [`let`](/manual/operators/let/), and [`[]`](/manual/operators/collection-builder/).
