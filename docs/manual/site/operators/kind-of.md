---
title: "kind-of"
description: "Inspect the kind of a Coalton type constructor."
hideMeta: true
weight: 305
---

`kind-of` is a debugging helper for type constructors.

## Syntax

```lisp
(kind-of '⟨type-name⟩)
```

## Semantics

- `kind-of` is called from the surrounding Common Lisp environment.
- It looks up a Coalton type and returns its kind, or `NIL` if the type is not
  known.
- This is useful when working with higher-kinded types and aliases.

## Example

```lisp
(kind-of 'Result)
```
