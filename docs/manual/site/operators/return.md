---
title: "return"
description: "Early function return form."
hideMeta: true
weight: 265
---

`return` exits the nearest enclosing function immediately with zero, one, or multiple values.

## Syntax

```lisp
;; Return zero values (`Void`) from the enclosing function
(return)

;; Return the value(s) of `⟨expr⟩` from the enclosing function
(return ⟨expr⟩)
```

## Semantics

- `return` transfers control out of the nearest enclosing function scope: a function-syntax [`define`](/manual/operators/define/) or instance method definition (including nullary `(define (f) ...)`), a value-syntax `define` or instance method definition that declares positional or keyword parameters, or an anonymous [`fn`](/manual/operators/fn/) (unless the `fn` was created with `:transparent-to-return`).
- `(return)` is equivalent to `(return (values))` and returns `Void` (zero values).
- `(return ⟨expr⟩)` evaluates `⟨expr⟩` and returns its result(s), which may be a single value, `Void`, or multiple values via [`values`](/manual/operators/values/).
- Every `return` targeting the same function, together with the final expression of that function's body, must unify to the same return type.
- Because `return` never produces a value locally at the call site, the `(return ...)` expression itself unifies with any expected type in its surrounding expression.
- `return` is invalid in a top-level value definition without parameters, in a standalone `(coalton ...)` expression outside any function, or inside the right-hand side of a [`do`](/manual/operators/do/) bind `(⟨pattern⟩ <- ⟨expr⟩)`.

## Examples

```lisp
(coalton-toplevel
  ;; Early return from a search loop
  (declare find-first-negative ((Vector Integer) -> (Optional Integer)))
  (define (find-first-negative v)
    (let len = (coalton/vector:length v))
    (for ((i 0 (+ i 1)))
      :while (< i len)
      (let x = (coalton/vector:index-unsafe i v))
      (when (< x 0)
        (return (Some x))))
    None)

  ;; Early return of zero values (`Void`)
  (declare guard-positive (Integer * (coalton/cell:Cell Integer) -> Void))
  (define (guard-positive x cell)
    (when (<= x 0)
      (return))
    (coalton/cell:write! cell x)
    (values)))
```

## Other notes

- In Coalton, `return` is always an imperative early function exit (compiled to Common Lisp `cl:block` and `cl:return-from`), **not** a monadic unit operator. To lift a pure value into a `Monad` or `Applicative` (such as inside a [`do`](/manual/operators/do/) block), use `pure`.
- For early return on `Result` or `Optional` failures, see [`need`](/manual/operators/need/).

