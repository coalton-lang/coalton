---
title: "unlikely"
description: "Branch-likelihood hint for `Boolean` expressions."
hideMeta: true
weight: 90
---

`unlikely` is the inverse of [`likely`](/manual/operators/likely/).

## Syntax

```lisp
(unlikely ⟨predicate⟩)
```

## Semantics

- `unlikely` has type `(Boolean -> Boolean)`.
- It returns the Boolean unchanged.
- The hint tells the compiler that the condition is expected to be `False`.
- The hint is only for optimization; it does not alter semantics.

## Example

```lisp
(if (unlikely failed?)
    recovery
    normal-result)
```
