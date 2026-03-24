---
title: "likely"
description: "Branch-likelihood hint for `Boolean` expressions."
hideMeta: true
weight: 85
---

`likely` is a Boolean hint used by code generation and optimization.

## Syntax

```lisp
(likely ⟨expr⟩)
```

## Semantics

- `likely` has type `(Boolean -> Boolean)`.
- It returns the predicate unchanged.
- The hint tells the compiler that the condition is expected to be `True`.

## Example

```lisp
(if (likely ready?)
    fast-path
    slow-path)
```
