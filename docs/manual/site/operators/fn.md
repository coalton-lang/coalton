---
title: "fn"
description: "Anonymous function form."
hideMeta: true
weight: 140
---

`fn` creates an anonymous function.

## Syntax

```lisp
(fn (⟨arg⟩ ...) ⟨body⟩)
```

## Semantics

- `fn` is the standard way to write lambdas in Coalton.
- A closure is allocated when variables are captured.
- `fn` also supports keyword arguments with `&key`.
- `return` may be used to return a value from the function.

## Example

```lisp
(fn (x)
  (+ x 2))
```
