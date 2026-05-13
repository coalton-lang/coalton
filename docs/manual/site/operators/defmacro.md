---
title: "defmacro"
description: "Compatibility alias for define-expression-macro."
hideMeta: true
weight: 77
---

`defmacro` in the `coalton` package is a compatibility alias for
[`define-expression-macro`](/manual/operators/define-expression-macro/).

## Syntax

```lisp
(defmacro name lambda-list
  body ...)
```

## Semantics

- It defines a Coalton expression macro.
- New code should prefer `define-expression-macro`.
- Use `cl:defmacro` for ordinary Lisp macros, including macros that are meant
  to be called bare and expand into a complete `(coalton-toplevel ...)` form.

## Example

```lisp
(defmacro if-some ((name expr) then else)
  `(match ,expr
     ((Some ,name) ,then)
     ((None) ,else)))
```
