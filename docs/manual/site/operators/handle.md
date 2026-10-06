---
title: "handle"
description: "Exception handling expression whose branches run before unwinding."
hideMeta: true
weight: 267
---

`handle` evaluates an expression and handles any thrown exception that matches
one of its branches. Unlike [`catch`](/manual/operators/catch/), the matching
branch runs where the exception was thrown, before anything unwinds, so it can
resume the computation that threw.

## Syntax

```lisp
(handle ⟨expr⟩
  ((⟨exception-ctor⟩ ⟨pattern⟩ ...) ⟨handler-body⟩ ...)
  ((the ⟨exception-type⟩ ⟨var-or-_⟩) ⟨handler-body⟩ ...)
  ...
  (_ ⟨fallback-body⟩ ...))
```

## Semantics

- Branches use the same patterns as `catch` and are tried in the same order.
  If none matches, the exception propagates to enclosing handlers.
- A matching branch runs where the exception was thrown, so it can transfer
  control with [`resume-to`](/manual/operators/resume-to/) to a resumption
  established by the code that threw, using
  [`resumable`](/manual/operators/resumable/).
- If the branch finishes normally, its value is returned from the `handle`
  expression, as with `catch`.
- The branch sees the dynamic bindings in effect where the exception was
  thrown. It is not in tail position, and the code that threw stays on the
  stack while it runs.
- Exceptions thrown by a branch go to handlers enclosing the `handle`.
- All branches must agree on the result type of the `handle` expression.

Prefer `catch` unless a branch needs to resume.

## Example

```lisp
(define-resumption SkipEgg)

(declare make-breakfast-with (Egg -> (Optional Egg)))
(define (make-breakfast-with egg)
  (resumable (Some (cook (crack egg)))
    ((SkipEgg) None)))

(declare make-breakfast-or-skip (Egg -> (Optional Egg)))
(define (make-breakfast-or-skip egg)
  (handle (make-breakfast-with egg)
    ((DeadlyEgg _) (resume-to SkipEgg))))
```
