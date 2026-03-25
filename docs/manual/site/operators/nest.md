---
title: "nest"
description: "Nested application macro."
hideMeta: true
weight: 320
---

`nest` is a convenience macro for nested function application.

## Syntax

```lisp
(nest ⟨expr⟩...)
```

## Semantics

- `(nest f g h x)` is equivalent to `(f (g (h x)))`.
- It reads like direct nesting rather than data threading.
- Because `nest` is a macro, it is not itself a higher-order function.

## Example

```lisp
(nest show reverse sort xs)
```
