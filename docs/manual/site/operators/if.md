---
title: "if"
description: "Boolean conditional form."
hideMeta: true
weight: 200
---

`if` is a two-branch conditional expression controlled by a `Boolean` test.

## Syntax

```lisp
(if ⟨test-expr⟩ ⟨then-expr⟩ ⟨else-expr⟩)
```

## Semantics

- `⟨test-expr⟩`, `⟨then-expr⟩`, and `⟨else-expr⟩` are all required; each is a single expression (not an implicit `progn`).
- `⟨test-expr⟩` must have type `Boolean`.
- `⟨test-expr⟩` is evaluated first:
  - If it evaluates to `True`, `⟨then-expr⟩` is evaluated and its value(s) are returned (`⟨else-expr⟩` is not evaluated).
  - If it evaluates to `False`, `⟨else-expr⟩` is evaluated and its value(s) are returned (`⟨then-expr⟩` is not evaluated).
- `⟨then-expr⟩` and `⟨else-expr⟩` must unify to the same type, which may be a single value, `Void`, or multiple values.
- Both `⟨then-expr⟩` and `⟨else-expr⟩` are in tail position when the `if` expression is in tail position (including for [`rec`](/manual/operators/rec/) tail calls).

## Examples

```lisp
(coalton-toplevel
  (declare abs-int (Integer -> Integer))
  (define (abs-int n)
    (if (< n 0)
        (neg n)
        n))

  ;; Returning multiple values from `if` branches
  (declare ordered-pair (Integer * Integer -> Integer * Integer))
  (define (ordered-pair a b)
    (if (<= a b)
        (values a b)
        (values b a))))
```

## Other notes

- By Coalton style convention, `if` is used for pure two-way expressions. When branches require multiple statements, local `let` bindings, or side effects, or when chaining multiple tests, prefer [`cond`](/manual/operators/cond/). For one-armed side-effecting conditionals, use [`when`](/manual/operators/when/) or [`unless`](/manual/operators/unless/).
- In release mode, locally bound data types, structs, and `Cell`s constructed across `if` branches can be scalar-replaced or unboxed without heap allocation. See [Performance and Optimization](/manual/topics/performance-and-optimization/).

