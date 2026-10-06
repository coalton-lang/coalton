---
title: "throw"
description: "Signal an exception."
hideMeta: true
weight: 260
---

`throw` signals an exception value to the nearest enclosing
[`catch`](/manual/operators/catch/) or [`handle`](/manual/operators/handle/)
that matches it, or invokes the debugger otherwise.

## Syntax

```lisp
(throw ⟨expr⟩)
```

## Semantics

- The argument's type must be an instance of the `Exception` class, which
  holds exactly for types defined with
  [`define-exception`](/manual/operators/define-exception/). `throw` has the
  type `Exception :e => :e -> :a`, so functions that throw a value of unknown
  type are polymorphic over exception types.
- Control transfers out of the current computation until a matching `catch`
  or `handle` branch is found.
- Exception values can be constructed before they are thrown, and a caught
  exception can be rethrown unchanged.
- `coalton/result:try` converts an exception thrown by a function into a
  `Result`, and `coalton/result:ok-or-throw` does the reverse.

## Example

```lisp
(define (crack egg)
  (match egg
    ((Xenomorph)
     (throw (DeadlyEgg egg)))
    (_ egg)))

;; Inferred type: Exception :e => :e -> :a
(define (rethrow e)
  (throw e))
```
