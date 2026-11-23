---
title: "protect"
description: "Run cleanup forms however control leaves an expression."
hideMeta: true
weight: 269
---

`protect` evaluates a protected expression and guarantees that its cleanup body runs whenever control leaves the protected expression.

## Syntax

```lisp
(protect ⟨protected-expr⟩
  ⟨cleanup-body-element⟩*
  ⟨cleanup-expr⟩)
```

## Semantics

- `⟨protected-expr⟩` is evaluated first, followed by the cleanup body `(⟨cleanup-body-element⟩* ⟨cleanup-expr⟩)`.
- The cleanup body runs both when `⟨protected-expr⟩` returns normally and when control exits `⟨protected-expr⟩` non-locally via [`throw`](/manual/operators/throw/), [`return`](/manual/operators/return/), [`break`](/manual/operators/break/), [`continue`](/manual/operators/continue/), or [`resume-to`](/manual/operators/resume-to/).
- When `⟨protected-expr⟩` returns normally, the `protect` expression returns the value(s) of `⟨protected-expr⟩` (which may be a single value, `Void`, or multiple values).
- The cleanup body is an implicit [`progn`](/manual/operators/progn/) (supporting multiple expressions and shorthand [`let`](/manual/operators/let/) bindings), and all values produced by the cleanup body are discarded. (As elsewhere in Coalton, discarding a `Result` emits a compiler warning unless explicitly bound with `(let _ = ...)`.)
- If a cleanup form throws an exception or performs a non-local exit, that new control transfer supersedes the original exit.
- Neither `⟨protected-expr⟩` nor the cleanup body is in tail position.

## Examples

```lisp
(coalton-toplevel
  (declare with-temp-counter ((coalton/cell:Cell UFix) * (Void -> :a) -> :a))
  (define (with-temp-counter active-count action)
    (coalton/cell:increment! active-count)
    (protect (action)
      (coalton/cell:decrement! active-count))))
```

## Other notes

- `protect` compiles directly to Common Lisp `cl:unwind-protect`.
- When a [`handle`](/manual/operators/handle/) branch runs at a [`throw`](/manual/operators/throw/) site inside `⟨protected-expr⟩`, the cleanup forms of `protect` have not yet run while the `handle` branch is executing; they run as soon as the `handle` branch either returns normally (unwinding to `handle`) or invokes [`resume-to`](/manual/operators/resume-to/) to a [`resumable`](/manual/operators/resumable/) outside the `protect`.
- See [`catch`](/manual/operators/catch/), [`handle`](/manual/operators/handle/), and [`throw`](/manual/operators/throw/).

