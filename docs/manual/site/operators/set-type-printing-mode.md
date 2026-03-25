---
title: "set-type-printing-mode"
description: "Control how Coalton prints types and aliases."
hideMeta: true
weight: 320
---

`set-type-printing-mode` changes how Coalton renders ordinary type displays.

## Syntax

```lisp
(set-type-printing-mode ⟨mode⟩)
```

## Semantics

- `mode` must be one of `:types`, `:aliases`, or `:types-and-aliases`.
- The setting affects displays such as [`type-of`](/manual/operators/type-of/)
  and other type strings shown by Coalton.
- [`describe-type-of`](/manual/operators/describe-type-of/) and
  [`describe-type-alias`](/manual/operators/describe-type-alias/) always print
  their alias-rich diagnostic view directly.
- This is a Common Lisp REPL helper for inspection workflows.

## Example

```lisp
(set-type-printing-mode :types-and-aliases)
```
