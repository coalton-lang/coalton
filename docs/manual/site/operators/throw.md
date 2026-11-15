---
title: "throw"
description: "Signal an exception."
hideMeta: true
weight: 260
---

`throw` signals an exception value, transferring control to the nearest matching [`catch`](/manual/operators/catch/) or [`handle`](/manual/operators/handle/) branch.

## Syntax

```lisp
(throw ⟨exception-expr⟩)
```

## Semantics

- `⟨exception-expr⟩` must have a type `:e` that is an instance of the `Exception` type class (which holds exclusively for types defined with [`define-exception`](/manual/operators/define-exception/)).
- `throw` has the polymorphic behavior `Exception :e => :e -> :a` (and unifies with any expected return type, including `Void` or multiple values, because it never returns normally).
- Evaluating `(throw ⟨exception-expr⟩)` evaluates `⟨exception-expr⟩` and signals the resulting condition, searching dynamically enclosing [`catch`](/manual/operators/catch/) and [`handle`](/manual/operators/handle/) forms from innermost to outermost.
- If no enclosing handler matches the thrown exception, the unhandled condition reaches the host Common Lisp condition system (typically invoking the interactive debugger).
- Exception values are ordinary first-class Coalton values: they can be constructed in advance, stored in data structures, passed to helper functions, or bound in a `catch`/`handle` branch and rethrown.

## Examples

```lisp
(coalton-toplevel
  (define-exception ParseError
    EmptyInput
    (InvalidToken String))

  (declare parse-token (String -> String))
  (define (parse-token s)
    (cond
      ((== s "") (throw EmptyInput))
      ((== s "?") (throw (InvalidToken s)))
      (True s)))

  ;; Polymorphic helper that logs and rethrows any exception
  (declare trace-and-rethrow ((Exception :e) (Show :e) => :e -> :a))
  (define (trace-and-rethrow e)
    (show e)
    (throw e)))
```

## Other Notes

- `throw` compiles directly to `(cl:error ⟨exception-expr⟩)`.
- To convert between thrown exceptions and `Result` values, use `coalton/result:try` (which catches exceptions into `Result`) and `coalton/result:ok-or-throw` (which unwraps `Ok` or `throw`s the `Err` payload).
- See [`define-exception`](/manual/operators/define-exception/), [`catch`](/manual/operators/catch/), [`handle`](/manual/operators/handle/), and [`protect`](/manual/operators/protect/).

