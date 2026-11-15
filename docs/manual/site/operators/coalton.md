---
title: "coalton"
description: "Expression-level entry point from Common Lisp."
hideMeta: true
weight: 20
---

`coalton` compiles and evaluates one or more Coalton expressions from Common Lisp.

## Syntax

```lisp
;; Evaluate a single Coalton expression
(coalton ⟨expr⟩)

;; Evaluate a sequence of Coalton expressions
(coalton ⟨expr⟩+)
```

## Semantics

- `coalton` is the expression-level bridge from Common Lisp into Coalton; it evaluates expressions rather than introducing top-level definitions.
- At least one `⟨expr⟩` is required. When multiple expressions are supplied, they are evaluated left to right as an implicit [`progn`](/manual/operators/progn/), and the value(s) of the final expression are returned to Common Lisp.
- Top-level shorthand `let` forms (`(let x = ...)`) are not permitted directly as arguments to `coalton`; wrap the body in [`progn`](/manual/operators/progn/) or [`let`](/manual/operators/let/) when local bindings are needed.
- The expression is typechecked, type-defaulted, and compiled as a standalone unit against the current global Coalton environment.
- After type defaulting, the expression's inferred type must have no unresolved typeclass constraints. If a constraint remains ambiguous, compilation fails with `"Unable to codegen"` unless disambiguated with [`the`](/manual/operators/the/).
- The return value is the underlying Common Lisp representation of the final Coalton expression: a single value, zero values (`(values)` for `Void`), or multiple Common Lisp values (`T1 * T2 * ...`).

## Examples

```lisp
;; Evaluate a Coalton expression from Common Lisp
(coalton (+ 1 2))
; => 3

;; Disambiguate a polymorphic expression with `the`
(coalton (the (List F32) [1 2 3]))
; => (1.0 2.0 3.0)

;; Receive multiple Coalton return values in Common Lisp
(cl:multiple-value-bind (q r)
    (coalton (values (coalton/math:div 17 5) (mod 17 5)))
  (cl:list q r))
; => (3 2)
```

## Other Notes

- `coalton` cannot directly reference surrounding Common Lisp lexical variables. To pass Common Lisp values into Coalton, define a Coalton function with [`coalton-toplevel`](/manual/operators/coalton-toplevel/) and call it from Common Lisp, or read Common Lisp state inside Coalton with [`lisp`](/manual/operators/lisp/).
- When read with the `coalton:coalton` readtable (or in a `.ct` file), `coalton` preserves exact source character spans for compiler diagnostics.
- See [Lisp Interop](/manual/topics/lisp-interop/) for how Coalton types map to Common Lisp values.

