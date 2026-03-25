---
title: "catch"
description: "Exception handling expression."
hideMeta: true
weight: 265
---

`catch` evaluates an expression and handles any thrown exception that matches
one of its branches.

## Syntax

```lisp
(catch ⟨expr⟩
  ((⟨exception-ctor⟩ ⟨pattern⟩ ...) ⟨handler-body⟩ ...)
  ...
  (_ ⟨fallback-body⟩ ...))
```

## Semantics

- The first subform is the expression that may throw.
- Each branch matches either an exception constructor pattern or `_` as a
  catch-all.
- Branch bodies run in order after a matching exception is raised.
- All branches must agree on the result type of the `catch` expression.

## Example

```lisp
(define (crack-safely egg)
  (catch (Ok (crack egg))
    ((DeadlyEgg _) (Err (DeadlyEgg egg)))
    ((UnCracked _) (Err (UnCracked egg)))))
```
