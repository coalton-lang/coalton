---
title: "catch"
description: "Exception handling expression."
hideMeta: true
weight: 265
---

`catch` evaluates an expression and handles any thrown exception that matches one of its branches after unwinding the stack.

## Syntax

```lisp
(catch ⟨expr⟩
  ⟨catch-branch⟩+)

;; ⟨catch-branch⟩ := (⟨catch-pattern⟩ ⟨body-element⟩* ⟨expr⟩)
;; ⟨catch-pattern⟩ := (⟨exception-ctor⟩ ⟨pattern⟩*)
;;                  | (the ⟨exception-type⟩ ⟨var-or-_⟩)
;;                  | _
;;                  | (= ⟨var⟩ ⟨catch-pattern⟩)
```

## Semantics

- If `⟨expr⟩` completes normally without throwing, `catch` returns the value(s) of `⟨expr⟩`.
- Each `⟨catch-branch⟩` matches one of the following pattern forms:
  - `((⟨exception-ctor⟩ ⟨pattern⟩*) ...)`: matches a specific constructor of a Coalton exception type defined with [`define-exception`](/manual/operators/define-exception/) (including nullary constructors written `((MyException) ...)`).
  - `((the ⟨exception-type⟩ ⟨var-or-_⟩) ...)`: matches every exception of `⟨exception-type⟩` and binds the exception value to `⟨var-or-_⟩` (which can be inspected or rethrown with [`throw`](/manual/operators/throw/)). For a `(repr :native ...)` exception, this also matches any Common Lisp condition subtype.
  - `(_ ...)`: catches every Common Lisp `cl:error`, including all Coalton exceptions and panics. (To catch only panics from `error`, `unwrap`, `unreachable`, and `assert`, use `((the Panic p) ...)`.)
  - `((= ⟨var⟩ ⟨catch-pattern⟩) ...)`: matches a constructor or wildcard `⟨catch-pattern⟩` and binds the caught exception object to `⟨var⟩` (with type `coalton/exception:LispError` when aliasing `_`).
- Branches are tested in source order at the point where the exception is signaled. Within a pattern, a field test that needs a GADT witness waits until that witness matches; independent tests keep their source order. If no branch matches, the exception propagates to outer handlers without unwinding the stack.
- Once a branch matches, the stack unwinds to the `catch` expression (running any intervening [`protect`](/manual/operators/protect/) cleanup forms and restoring dynamic bindings) before the selected branch body executes.
- In the selected branch:
  - Dynamic variables have the bindings in effect at the `catch` expression rather than at the [`throw`](/manual/operators/throw/) site.
  - The final expression of the branch body is in tail position when the `catch` expression is in tail position (including for [`rec`](/manual/operators/rec/) tail calls).
  - Exceptions thrown by the branch body itself are not caught by the same `catch`.
  - Resumptions established *inside* `⟨expr⟩` have already unwound and cannot be invoked with [`resume-to`](/manual/operators/resume-to/); use [`handle`](/manual/operators/handle/) when a handler needs to resume into `⟨expr⟩`.
- `⟨expr⟩` and all branch bodies must unify to the same result type, which may be a single value, `Void`, or multiple values.

## Examples

```lisp
(coalton-toplevel
  (define-exception BadEgg
    (DeadlyEgg String)
    (UnCracked Integer))

  (declare crack-safely ((Void -> String) -> (Result String String)))
  (define (crack-safely attempt)
    (catch (Ok (attempt))
      ((DeadlyEgg reason) (Err reason))
      ((UnCracked _)      (Err "egg did not crack"))))

  (declare crack-or-rethrow ((Void -> String) -> String))
  (define (crack-or-rethrow attempt)
    (catch (attempt)
      ((the BadEgg e)
       (show "caught BadEgg, rethrowing")
       (throw e)))))
```

## Other notes

- `catch` compiles to Common Lisp `cl:handler-bind` inside a `cl:block`: the handler tests constructor field patterns before unwinding and, only upon a match, transfers the branch index and condition out of the inner block so the handler body executes after unwinding.
- See [`handle`](/manual/operators/handle/), [`throw`](/manual/operators/throw/), [`define-exception`](/manual/operators/define-exception/), and [`protect`](/manual/operators/protect/).

