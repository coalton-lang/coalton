---
title: "resumable"
description: "Resumption handling expression."
hideMeta: true
weight: 270
---

`resumable` evaluates an expression while establishing named resumption handlers that can be invoked with [`resume-to`](/manual/operators/resume-to/).

## Syntax

```lisp
(resumable ⟨expr⟩
  ((⟨resumption-ctor⟩ ⟨pattern⟩*) ⟨body-element⟩* ⟨expr⟩)
  ...)
```

## Semantics

- If `⟨expr⟩` finishes without invoking a matching resumption, `resumable` returns the value of `⟨expr⟩`.
- Each branch head `(⟨resumption-ctor⟩ ⟨pattern⟩*)` (optionally wrapped in an alias pattern `(= ⟨var⟩ ...)`) must match a constructor for a resumption type defined with [`define-resumption`](/manual/operators/define-resumption/) (bare variable or `_` branch heads are not permitted).
- Multiple branches may match the same resumption constructor with different field subpatterns; patterns on constructor fields are tested in source order before unwinding.
- If [`resume-to`](/manual/operators/resume-to/) is called with a resumption value that does not match any branch of the inner `resumable`, the value is forwarded to the next enclosing `resumable` handler for that resumption type. If no enclosing `resumable` matches, a runtime error is signaled.
- Once a branch matches, control unwinds to the `resumable` expression and evaluates the selected branch body (an implicit [`progn`](/manual/operators/progn/)).
- The final expression of each branch body is in tail position when the `resumable` expression is in tail position (including for [`rec`](/manual/operators/rec/) tail calls).
- `⟨expr⟩` and all branch bodies must unify to the same single-value result type.

## Examples

```lisp
(coalton-toplevel
  (define-exception ParseIntError
    (BadDigit Char))

  (define-resumption SkipDigit)
  (define-resumption (SubstituteDigit Integer))

  (declare parse-digit (Char -> (Optional Integer)))
  (define (parse-digit c)
    (resumable
        (match c
          (#\0 (Some 0))
          (#\1 (Some 1))
          (_   (throw (BadDigit c))))
      ((SkipDigit)         None)
      ((SubstituteDigit d) (Some d))))

  (declare parse-binary-default-zero (Char -> (Optional Integer)))
  (define (parse-binary-default-zero c)
    (handle (parse-digit c)
      ((BadDigit _) (resume-to (SubstituteDigit 0))))))
```

## Other notes

- `resumable` compiles to Common Lisp `cl:restart-bind`: each distinct resumption type establishes a restart that tests branch field patterns before unwinding, chains to any outer restart of the same name on mismatch, and unwinds to execute the matching branch body upon selection.
- See [`define-resumption`](/manual/operators/define-resumption/), [`resume-to`](/manual/operators/resume-to/), and [`handle`](/manual/operators/handle/).

