---
title: "*"
description: "Type-level separator for multiple inputs and outputs."
hideMeta: true
weight: 35
---

`*` separates multiple positional input types on the left of [`->`](/manual/operators/arrow/) and multiple return value types on the right of `->`.

## Syntax

```lisp
;; Multiple positional inputs and/or multiple return values in a function type
(⟨in-type-1⟩ * ⟨in-type-2⟩ [* ...] -> ⟨out-type-1⟩ * ⟨out-type-2⟩ [* ...])

;; Multiple return values in an inline `lisp` form
(lisp (-> ⟨out-type-1⟩ * ⟨out-type-2⟩ [* ...]) (⟨var-spec⟩*)
  ⟨lisp-form⟩+)
```

## Semantics

- **On the left of [`->`](/manual/operators/arrow/)**:
  - `A * B -> C` denotes a single two-argument function called as `(f a b)`.
  - This is distinct from the curried type `A -> B -> C` (a one-argument function returning a one-argument function, called as `((f a) b)`) and from `(Tuple A B -> C)` (a one-argument function taking a `Tuple` data structure).
  - When [`&key`](/manual/operators/keyword-tail/) is also present, `&key` follows the `*`-separated positional input types: `(A * B &key (:k K) -> C)`.
- **On the right of [`->`](/manual/operators/arrow/)** (and in `(lisp (-> ...) ...)`):
  - `In -> A * B` denotes a function returning two values via [`values`](/manual/operators/values/), which callers can bind with `(let (values a b) = (f x))`.
  - When `->` appears to the right of `->` (as in `A -> B -> C`), the right-hand side is parsed as a curried function return type rather than a `*` pack. Wrap a function component of a multi-value return in parentheses: `(In -> (A -> B) * C)`.
- **Component restrictions**:
  - Every component separated by `*` must be a single-value type; `Void` and nested multi-value packs cannot appear as components of a `*` list.
  - A multi-value pack `A * B` cannot be used as a function input argument, a type constructor argument (such as `List (A * B)`), or a standalone [`declare`](/manual/operators/declare/) type.

## Examples

```lisp
(coalton-toplevel
  ;; Two positional inputs and two return values
  (declare divmod (Integer * Integer -> Integer * Integer))
  (define (divmod a b)
    (values (coalton/math:div a b) (mod a b)))

  ;; Destructure the multiple return values with `let (values ...) = ...`
  (declare euclidean-step (Integer * Integer -> Integer))
  (define (euclidean-step a b)
    (let (values _q r) = (divmod a b))
    r))
```

## Other Notes

- In expression contexts, `*` is the multiplication method of the `Num` type class (`(coalton/classes:* x y)`).
- Surrounding a variable name with `*` earmuffs (`*name*`) designates a dynamic variable in [`define`](/manual/operators/define/) and [`dynamic-bind`](/manual/operators/dynamic-bind/).
- In [`kind-of`](/manual/operators/kind-of/) output, `*` is the printed representation of the `Type` kind.

