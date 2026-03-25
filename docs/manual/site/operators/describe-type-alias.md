---
title: "describe-type-alias"
description: "Print the definition behind a type alias."
hideMeta: true
weight: 315
---

`describe-type-alias` prints the type associated with a Coalton type alias.

## Syntax

```lisp
(describe-type-alias '⟨alias-name⟩)
```

## Semantics

- `describe-type-alias` is a Common Lisp debugging helper.
- It prints a diagnostic representation of the alias together with its aliased
  type.
- If the symbol is not known as a type alias, it prints `unknown`.
- It returns no values.

## Example

```lisp
(describe-type-alias 'Pair)
```
