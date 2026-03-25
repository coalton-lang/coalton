---
title: "noinline"
description: "Prevent a specific application from being inlined."
hideMeta: true
weight: 80
---

`noinline` is an expression-level compiler hint.

## Syntax

```lisp
(noinline ⟨application⟩)
```

## Semantics

- `noinline` only affects syntactic function applications.
- It prevents Coalton's inliner from rewriting that particular call.
- It does not change the result of the wrapped expression.
- Use it when a local call should remain explicit for size, debugging, or
  optimization reasons.
- `noinline` acts as identity otherwise.

## Example

```lisp
(define (call-once x)
  (noinline (expensive-step x)))
```
