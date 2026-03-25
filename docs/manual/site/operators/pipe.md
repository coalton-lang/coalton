---
title: "pipe"
description: "Threading macro."
hideMeta: true
weight: 310
---

`pipe` is a convenience macro for left-to-right function application.

## Syntax

```lisp
(pipe ⟨expr⟩...)
```

## Semantics

- `(pipe x f g h)` is equivalent to `(h (g (f x)))`.
- It is useful when the data value should read first and the transformations
  should read in execution order.
- Because `pipe` is a macro, it is not itself a higher-order function.

## Example

```lisp
(pipe xs
      reverse
      sort
      show)
```
