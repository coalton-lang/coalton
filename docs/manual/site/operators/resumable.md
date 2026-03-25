---
title: "resumable"
description: "Resumption handling expression."
hideMeta: true
weight: 270
---

`resumable` evaluates an expression in a context that can intercept named
resumptions.

## Syntax

```lisp
(resumable ⟨expr⟩
  ((⟨resumption-ctor⟩ ⟨pattern⟩ ...) ⟨handler-body⟩ ...))
```

## Semantics

- The first subform is the expression that may invoke
  [`resume-to`](/manual/operators/resume-to/).
- Each branch must match a resumption constructor, not an arbitrary pattern.
- When a matching resumption is signaled, control transfers to the handler body.
- The result type of each handler must agree with the result type of the whole
  `resumable` expression.

## Example

```lisp
(define (make-breakfast-with egg)
  (resumable (Some (cook (crack egg)))
    ((SkipEgg) None)
    ((ServeRaw _) (Some egg))))
```
