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
- Like `throw`, `resume-to` is not currently polymorphic without an explicit
  type.

## Example

```lisp
(define (serve-raw egg)
  (resume-to (ServeRaw egg)))
```
