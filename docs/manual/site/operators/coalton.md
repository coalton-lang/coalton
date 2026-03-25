---
title: "coalton"
description: "Expression-level entry point from Common Lisp."
hideMeta: true
weight: 20
---

`coalton` evaluates one or more Coalton expressions from Common Lisp.

## Syntax

```lisp
(coalton ⟨expr⟩ ...)
```

## Semantics

- `coalton` is for expressions, not new definitions.
- When more than one expression is supplied, they are evaluated as an implicit
  `progn`.
- It is especially useful in the REPL.
- The result is the underlying Lisp value produced by the last Coalton
  expression.

## Example

```lisp
(coalton (fst pair))
```
