---
title: "cond"
description: "Multi-branch conditional form."
hideMeta: true
weight: 250
---

`cond` evaluates a sequence of `Boolean` test clauses in order and runs the body of the first clause whose test is `True`.

## Syntax

```lisp
(cond
  (⟨test-expr⟩ ⟨body-element⟩* ⟨expr⟩)
  ...)
```

## Semantics

- At least one clause `(⟨test-expr⟩ ⟨body-element⟩* ⟨expr⟩)` is required.
- Each `⟨test-expr⟩` must have type `Boolean`, and each clause must have at least one body expression.
- Clauses are tested top-to-bottom. For the first clause whose `⟨test-expr⟩` evaluates to `True`, its body is evaluated as an implicit [`progn`](/manual/operators/progn/) (supporting multiple expressions and shorthand [`let`](/manual/operators/let/) bindings), and the value(s) of its final `⟨expr⟩` are returned. Remaining clauses are not evaluated.
- All clause bodies must unify to the same result type, which may be a single value, `Void`, or multiple values.
- The final expression of each clause body is in tail position when the `cond` expression is in tail position (including for [`rec`](/manual/operators/rec/) tail calls).
- Writing `True` as the test of the final clause provides an exhaustive catch-all branch. Any clauses written after a literal `True` test are unreachable and discarded by the compiler.
- If no literal `True` clause is present and every test evaluates to `False` at runtime, a runtime error (`"Non-exhaustive COND."`) is signaled.

## Examples

```lisp
(coalton-toplevel
  (declare classify-sign (Integer -> String))
  (define (classify-sign n)
    (cond
      ((< n 0) "negative")
      ((== n 0) "zero")
      (True "positive")))

  ;; Clause bodies are implicit `progn` blocks and support shorthand `let`
  (declare clamp-and-log ((coalton/cell:Cell UFix) * Integer * Integer * Integer -> Integer))
  (define (clamp-and-log clamped-count lo hi x)
    (cond
      ((< x lo)
       (coalton/cell:increment! clamped-count)
       lo)
      ((> x hi)
       (coalton/cell:increment! clamped-count)
       hi)
      (True
       (let delta = (- x lo))
       (+ lo delta)))))
```

## Other Notes

- `cond` is preferred over [`if`](/manual/operators/if/) whenever branches perform side effects, contain multiple expressions or local `let` bindings, or test more than one condition.
- For structural pattern matching on algebraic data types or literals, use [`match`](/manual/operators/match/).

