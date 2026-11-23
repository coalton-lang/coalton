---
title: "resume-to"
description: "Transfer control to a resumption handler."
hideMeta: true
weight: 275
---

`resume-to` transfers control to the nearest enclosing [`resumable`](/manual/operators/resumable/) branch that matches the supplied resumption value.

## Syntax

```lisp
(resume-to ⟨resumption-expr⟩)
```

## Semantics

- `⟨resumption-expr⟩` must have a type `:r` that is an instance of the `Resumption` type class (which holds exclusively for types defined with [`define-resumption`](/manual/operators/define-resumption/)).
- `resume-to` has the polymorphic behavior `Resumption :r => :r -> :a` and unifies with any expected return type because it never returns locally.
- Evaluating `(resume-to ⟨resumption-expr⟩)` searches dynamically enclosing [`resumable`](/manual/operators/resumable/) forms from innermost to outermost for a branch matching the resumption value, unwinds the stack to that `resumable` form (running any intervening [`protect`](/manual/operators/protect/) cleanup forms), and evaluates the matching branch body.
- If no active `resumable` branch matches the resumption value, a runtime error is signaled.
- `resume-to` is most commonly called inside a [`handle`](/manual/operators/handle/) branch, which executes before unwinding the stack and can therefore reach resumptions established inside the `handle`'s guarded expression. Calling `resume-to` inside a [`catch`](/manual/operators/catch/) branch can only reach `resumable` handlers outside the `catch`.

## Examples

```lisp
(coalton-toplevel
  (define-resumption (UseDefault Integer))

  ;; Polymorphic helper that invokes any resumption
  (declare invoke (Resumption :r => :r -> :a))
  (define (invoke r)
    (resume-to r))

  (declare compute-with-fallback (Boolean -> Integer))
  (define (compute-with-fallback use-default?)
    (resumable
        (progn
          (when use-default?
            (resume-to (UseDefault 42)))
          100)
      ((UseDefault n) n))))
```

## Other notes

- When the concrete resumption type is statically known at the `resume-to` site, Coalton compiles `(resume-to expr)` directly to `(cl:invoke-restart 'ResumptionName ...)`; when the resumption type is a type variable constrained by `Resumption :r`, it dispatches through `coalton-impl/runtime:invoke-resumption`.
- See [`define-resumption`](/manual/operators/define-resumption/), [`resumable`](/manual/operators/resumable/), and [`handle`](/manual/operators/handle/).

