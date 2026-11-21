---
title: "handle"
description: "Exception handling expression whose branches run before unwinding."
hideMeta: true
weight: 267
---

`handle` evaluates an expression and intercepts matching thrown exceptions in-place before unwinding the stack, allowing handler branches to invoke [`resume-to`](/manual/operators/resume-to/).

## Syntax

```lisp
(handle ⟨expr⟩
  ⟨handle-branch⟩+)

;; ⟨handle-branch⟩ := (⟨catch-pattern⟩ ⟨body-element⟩* ⟨expr⟩)
;; ⟨catch-pattern⟩ := (⟨exception-ctor⟩ ⟨pattern⟩*)
;;                  | (the ⟨exception-type⟩ ⟨var-or-_⟩)
;;                  | _
;;                  | (= ⟨var⟩ ⟨catch-pattern⟩)
```

## Semantics

- `handle` uses the same branch pattern syntax and top-to-bottom matching rules as [`catch`](/manual/operators/catch/). If no branch matches a thrown exception, the exception propagates to outer handlers without unwinding.
- Unlike `catch`, a matching `handle` branch runs **in-place at the point where the exception was thrown**, before the stack unwinds:
  - The branch can transfer control via [`resume-to`](/manual/operators/resume-to/) to a resumption established inside `⟨expr⟩` by [`resumable`](/manual/operators/resumable/).
  - The branch observes the dynamic bindings ([`dynamic-bind`](/manual/operators/dynamic-bind/)) in effect at the [`throw`](/manual/operators/throw/) site.
  - Because the signaling frames are still on the stack while the branch runs, `handle` branches are not in tail position.
- If a matching branch completes normally (without calling `resume-to` or `throw`), the stack unwinds to the `handle` expression and the branch's value(s) are returned from `handle`.
- Exceptions thrown inside a `handle` branch propagate to handlers enclosing the `handle`.
- `⟨expr⟩` and all branch bodies must unify to the same result type, which may be a single value, `Void`, or multiple values.

## Examples

```lisp
(coalton-toplevel
  (define-exception BadEgg
    (DeadlyEgg String))

  (define-resumption SkipEgg)

  (declare make-breakfast-with (String -> (Optional String)))
  (define (make-breakfast-with egg)
    (resumable
        (if (== egg "rotten")
            (throw (DeadlyEgg egg))
            (Some (<> "cooked " egg)))
      ((SkipEgg) None)))

  (declare make-breakfast-or-skip (String -> (Optional String)))
  (define (make-breakfast-or-skip egg)
    (handle (make-breakfast-with egg)
      ((DeadlyEgg _) (resume-to SkipEgg)))))

(coalton
  (Tuple (make-breakfast-or-skip "fresh")
         (make-breakfast-or-skip "rotten")))
; => #.(TUPLE "cooked fresh" #.NONE)
```

## Other Notes

- Prefer [`catch`](/manual/operators/catch/) unless a handler branch needs to invoke [`resume-to`](/manual/operators/resume-to/) or inspect dynamic state from the throw site.
- `handle` compiles to Common Lisp `cl:handler-bind` where the handler body runs directly inside the condition handler function and returns from the enclosing `cl:block` if it finishes normally.
- See [`catch`](/manual/operators/catch/), [`resumable`](/manual/operators/resumable/), [`resume-to`](/manual/operators/resume-to/), and [`throw`](/manual/operators/throw/).

