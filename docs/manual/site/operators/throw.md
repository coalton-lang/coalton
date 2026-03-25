---
title: "throw"
description: "Signal an exception."
hideMeta: true
weight: 260
---

`throw` signals an exception value to the nearest enclosing
[`catch`](/manual/operators/catch/) that matches it, or invokes
the debugger otherwise.

## Syntax

```lisp
(throw ⟨expr⟩)
```

## Semantics

- The argument must have a known exception type, typically from
  [`define-exception`](/manual/operators/define-exception/).
- Control transfers out of the current computation until a matching `catch`
  handler is found.
- Exception values can be constructed before they are thrown.
- `throw` is not currently polymorphic without an explicit type.

## Example

```lisp
(define (crack egg)
  (match egg
    ((Xenomorph)
     (throw (DeadlyEgg egg)))
    (_ egg)))
```
