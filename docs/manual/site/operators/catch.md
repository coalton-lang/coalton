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
  ((the ⟨exception-type⟩ ⟨var-or-_⟩) ⟨handler-body⟩ ...)
  ...
  (_ ⟨fallback-body⟩ ...))
```

## Semantics

- The first subform is the expression that may throw.
- Each branch matches an exception constructor pattern, every exception of a
  given type, or `_` as a catch-all.
- A branch written `(the ⟨exception-type⟩ var)` catches any exception of that
  type and binds it to `var`, which can be rethrown with
  [`throw`](/manual/operators/throw/). For a native exception (see
  [`define-exception`](/manual/operators/define-exception/)), this includes
  conditions of Lisp subtypes of its condition type.
- A `_` branch catches every Lisp `error`, including Coalton exceptions.
- Patterns are tried in order, including patterns on constructor fields. The
  first matching branch runs. If none matches, an enclosing handler may handle
  the exception.
- All branches must agree on the result type of the `catch` expression.

## Example

```lisp
(define (crack-safely egg)
  (catch (Ok (crack egg))
    ((DeadlyEgg _) (Err (DeadlyEgg egg)))
    ((UnCracked _) (Err (UnCracked egg)))))

(define (crack-or-rethrow egg)
  (catch (crack egg)
    ((the BadEgg e)
     (trace "bad egg")
     (throw e))))
```
