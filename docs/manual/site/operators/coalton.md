---
title: "coalton"
description: "Expression-level entry point from Common Lisp."
hideMeta: true
weight: 20
---

`coalton` evaluates a single Coalton expression from Common Lisp.

## Syntax

```lisp
(coalton ⟨expr⟩)
```

## Semantics

- `coalton` is for expressions, not new definitions.
- It is especially useful in the REPL.
- The result is the underlying Lisp value produced by the Coalton expression.

## Example

```lisp
(coalton (fst pair))
```
