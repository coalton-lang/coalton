---
title: "describe-type-of"
description: "Print type information for a Coalton value."
hideMeta: true
weight: 310
---

`describe-type-of` prints type information for a symbol in Coalton's global
environment.

## Syntax

```lisp
(describe-type-of '⟨name⟩)
```

## Semantics

- `describe-type-of` is a Common Lisp debugging helper, not an in-language
  Coalton expression form.
- It prints the type associated with a symbol together with its aliases.
- If the symbol names a macro, it prints `macro`.
- If the symbol is not known to Coalton, it prints `unknown`.
- It returns no values.

## Example

```lisp
(describe-type-of 'shifted-coordinate)
```
