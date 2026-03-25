---
title: "_"
description: "Wildcard pattern."
hideMeta: true
weight: 70
---

`_` is a wildcard pattern. It matches anything and does not bind a
name.

## Syntax

```lisp
_
```

## Semantics

- `_` matches any value of the expected type.
- Unlike `_x`, the bare `_` does not introduce a variable binding.
- It is most common in `match`, parameter patterns, and destructuring bindings.

## Example

```lisp
(match pair
  ((Tuple x _) x))
```
