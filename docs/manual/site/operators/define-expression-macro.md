---
title: "define-expression-macro"
description: "Define a macro for Coalton expression context."
hideMeta: true
weight: 75
---

`define-expression-macro` defines a macro that expands into a Coalton expression.

## Syntax

```lisp
(define-expression-macro ⟨name⟩ ⟨lambda-list⟩
  ⟨docstring⟩?
  ⟨declaration⟩*
  ⟨lisp-body⟩+)
```

## Semantics

- `define-expression-macro` is a Common Lisp top-level form (used outside `coalton-toplevel`).
- `⟨lambda-list⟩` is a standard Common Lisp macro destructuring lambda list. (When writing macros in a package that uses `#:coalton` rather than `#:cl`, qualify lambda-list keywords such as `cl:&optional`, `cl:&rest`, `cl:&body`, and `cl:&key`.)
- The macro body executes as Common Lisp at compile time and must return a single s-expression representing a valid Coalton expression.
- When `⟨name⟩` appears as the operator of a list in a Coalton expression context, the parser expands the macro and recursively parses the returned form as a Coalton expression.
- Nested expression macro expansions are capped at 500 expansion steps per expression to catch infinite expansion loops.
- Hygiene is manual: use `cl:gensym` or `alexandria:with-gensyms` for temporary Coalton bindings in the expansion to avoid variable capture and duplicate evaluation.

## Examples

```lisp
;; Bind `name` when an Optional is `Some`, or evaluate `else`
(define-expression-macro if-some ((name expr) then else)
  "Evaluate `then` with `name` bound if `expr` is `Some`, otherwise `else`."
  `(match ,expr
     ((Some ,name) ,then)
     ((None) ,else)))

(coalton
  (if-some (x (Some 41))
    (+ x 1)
    0))
; => 42

;; Avoid evaluating an argument more than once with gensym
(define-expression-macro square-expr (x)
  (cl:let ((tmp (cl:gensym "X-")))
    `(let ((,tmp ,x))
       (* ,tmp ,tmp))))
```

## Other notes

- `define-expression-macro` records `:expression` as the symbol's `:coalton-repl-context` property so Coalton-aware editors, REPLs, and documentation generators recognize it as an expression macro.
- For macros that expand into top-level definitions (`define`, `declare`, `define-instance`, etc.), use [`define-toplevel-macro`](/manual/operators/define-toplevel-macro/).
- See [Macros](/manual/topics/macros/) for more details on writing macros in Coalton.

