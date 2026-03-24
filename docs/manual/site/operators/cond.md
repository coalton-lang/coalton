---
title: "cond"
description: "Multi-branch conditional form."
hideMeta: true
weight: 250
---

`cond` is Coalton's multi-branch conditional.

## Syntax

```lisp
(cond
  (⟨test-expr⟩ ⟨result-expr⟩...)
  ...
  (True ⟨fallback-expr⟩...))
```

## Semantics

- Branches are considered in order.
- The first `True` test wins.
- `True` is commonly used as the final catch-all branch.
- Each branch result has an implicit `progn`, and therefore
  multiple expressions and short-`let`s are permitted.
- `cond` is preferred over `if` if the branches have side-effects.

## Example

```lisp
(cond
  ((< n 0) -1)
  ((== n 0) 0)
  (True 1))
```
