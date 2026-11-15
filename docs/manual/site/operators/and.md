---
title: "and"
description: "Short-circuiting conjunction."
hideMeta: true
weight: 230
---

`and` is a short-circuiting logical conjunction special form on `Boolean` expressions.

## Syntax

```lisp
(and ⟨boolean-expr⟩*)
```

## Semantics

- Every `⟨boolean-expr⟩` must have type `Boolean`, and the `and` expression has type `Boolean`.
- Arguments are evaluated left-to-right:
  - As soon as an argument evaluates to `False`, `and` immediately returns `False` without evaluating any remaining arguments.
  - If all arguments evaluate to `True`, `and` returns `True`.
  - With zero arguments, `(and)` evaluates to `True` (the identity element of conjunction).
- The last argument of a non-empty `and` form is in tail position when the `and` expression is in tail position (including for [`rec`](/manual/operators/rec/) tail calls).

## Examples

```lisp
(coalton-toplevel
  ;; Short-circuiting prevents division by zero in the second test
  (declare divides? (Integer * Integer -> Boolean))
  (define (divides? d n)
    (and (/= d 0)
         (== (mod n d) 0))))

(coalton (Tuple (divides? 0 10) (divides? 5 10)))
; => #.(TUPLE COMMON-LISP:NIL COMMON-LISP:T)
```

## Other Notes

- Unlike Common Lisp's `cl:and`, which accepts arbitrary types as generalized booleans, Coalton's `and` requires every operand to have type `Boolean`.
- Because `and` is a special form (to support short-circuiting), it cannot be passed as a first-class function value; use `boolean-and` when a function of type `(Boolean * Boolean -> Boolean)` is needed.
- See [`or`](/manual/operators/or/).

