---
title: "set-type-printing-mode"
description: "Control how Coalton prints types and aliases."
hideMeta: true
weight: 320
---

`set-type-printing-mode` changes how debugging helpers render types.

## Syntax

```lisp
(set-type-printing-mode ⟨mode⟩)
```

## Semantics

- `mode` must be one of `:types`, `:aliases`, or `:types-and-aliases`.
- The setting affects functions such as
  [`type-of`](/manual/operators/type-of/),
  [`describe-type-of`](/manual/operators/describe-type-of/), and
  [`describe-type-alias`](/manual/operators/describe-type-alias/).
- This is a Common Lisp REPL helper for inspection workflows.

## Example

```lisp
(set-type-printing-mode :types-and-aliases)
```
