---
title: "define-expression-macro"
description: "Define a macro for Coalton expression context."
hideMeta: true
weight: 75
---

`define-expression-macro` defines a macro that expands into Coalton expression
syntax.

## Syntax

```lisp
(define-expression-macro name lambda-list
  body ...)
```

## Semantics

- It has the same expansion model as `cl:defmacro`.
- The macro body runs as Common Lisp and returns Coalton source forms.
- The macro is intended for use where a Coalton expression is expected.
- Coalton-aware REPLs may use this role to evaluate bare calls as Coalton
  expressions.

## Example

```lisp
(define-expression-macro if-some ((name expr) then else)
  `(match ,expr
     ((Some ,name) ,then)
     ((None) ,else)))

(coalton
  (if-some (x maybe-count)
    (+ x 1)
    0))
```
