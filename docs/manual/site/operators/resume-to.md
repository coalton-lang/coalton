---
title: "resume-to"
description: "Transfer control to a resumption handler."
hideMeta: true
weight: 275
---

`resume-to` transfers control to the nearest enclosing
[`resumable`](/manual/operators/resumable/) handler that matches the resumption
value.

## Syntax

```lisp
(resume-to ⟨resumption-expr⟩)
```

## Semantics

- The argument must be a known resumption value, usually from
  [`define-resumption`](/manual/operators/define-resumption/).
- Control leaves the current computation and enters the matching `resumable`
  branch.
- `resume-to` is typically called from a [`handle`](/manual/operators/handle/)
  branch, which runs before unwinding, to recover from an exception thrown
  inside a `resumable` expression. A [`catch`](/manual/operators/catch/)
  branch runs after unwinding, so it can only resume to resumptions
  established outside the `catch`.
- Unlike `throw`, `resume-to` is not currently polymorphic without an explicit
  type.

## Example

```lisp
(define (serve-raw egg)
  (resume-to (ServeRaw egg)))
```
