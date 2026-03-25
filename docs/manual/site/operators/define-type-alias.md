---
title: "define-type-alias"
description: "Type alias definition form."
hideMeta: true
weight: 60
---

`define-type-alias` gives a new name to an existing type. It does *not* make a new type.

## Syntax

```lisp
(define-type-alias ⟨name⟩ ⟨type⟩)
(define-type-alias (⟨name⟩ ⟨var⟩ ...) ⟨type⟩)
```

## Semantics

- Aliases do not create a new type. If `A` is an alias for `AA`, then
  one cannot write class instances for both `A` and `AA` simultaneously.
- They are useful for readability, documentation, and local domain naming.
- Parametric aliases must still be fully applied where used.

## Example

```lisp
(define-type-alias (Pair :a) (Tuple :a :a))
```
