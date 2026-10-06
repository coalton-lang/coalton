---
title: "protect"
description: "Run cleanup forms however control leaves an expression."
hideMeta: true
weight: 269
---

`protect` evaluates an expression and then its cleanup forms, however control
leaves the expression.

## Syntax

```lisp
(protect ⟨expr⟩
  ⟨cleanup⟩...)
```

## Semantics

- The cleanup forms run when `⟨expr⟩` returns normally and when control
  leaves it otherwise: through an exception, [`return`](/manual/operators/return/),
  [`break`](/manual/operators/break/), [`continue`](/manual/operators/continue/),
  or [`resume-to`](/manual/operators/resume-to/).
- The value of the `protect` expression is the value of `⟨expr⟩`, which may be
  `Void` or multiple values. The values of the cleanup forms are discarded.
  As elsewhere, discarding a `Result` produces a warning unless it is
  discarded explicitly with `(let _ = ...)`, as in the example below.
- An exception thrown by a cleanup form propagates as usual.
- Neither `⟨expr⟩` nor the cleanup forms are in tail position.
- `protect` compiles to `cl:unwind-protect`.

## Example

```lisp
(define (process-file path)
  (let stream = (need (file:open path)))
  (protect (process stream)
    (let _ = (file:close stream))
    Unit))
```
