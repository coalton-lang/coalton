---
title: "or"
description: "Short-circuiting disjunction."
hideMeta: true
weight: 240
---

`or` is a short-circuiting logical disjunction special form on `Boolean` expressions.

## Syntax

```lisp
(or ⟨boolean-expr⟩*)
```

## Semantics

- Every `⟨boolean-expr⟩` must have type `Boolean`, and the `or` expression has type `Boolean`.
- Arguments are evaluated left-to-right:
  - As soon as an argument evaluates to `True`, `or` immediately returns `True` without evaluating any remaining arguments.
  - If all arguments evaluate to `False`, `or` returns `False`.
  - With zero arguments, `(or)` evaluates to `False` (the identity element of disjunction).
- The last argument of a non-empty `or` form is in tail position when the `or` expression is in tail position (including for [`rec`](/manual/operators/rec/) tail calls).

## Examples

```lisp
(coalton-toplevel
  (declare out-of-bounds? (UFix * UFix -> Boolean))
  (define (out-of-bounds? index len)
    (or (== len 0)
        (>= index len))))

(coalton (Tuple (out-of-bounds? 2 5) (out-of-bounds? 5 5)))
; => #.(TUPLE COMMON-LISP:NIL COMMON-LISP:T)
```

## Other Notes

- Unlike Common Lisp's `cl:or`, which accepts arbitrary types and returns the first non-`nil` value, Coalton's `or` requires every operand to have type `Boolean` and always returns a `Boolean`.
- Because `or` is a special form (to support short-circuiting), it cannot be passed as a first-class function value; use `boolean-or` when a function of type `(Boolean * Boolean -> Boolean)` is needed.
- See [`and`](/manual/operators/and/).

