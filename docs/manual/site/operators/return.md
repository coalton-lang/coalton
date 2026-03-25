---
title: "return"
description: "Function return."
hideMeta: true
weight: 265
---

`return` returns a value from the nearest enclosing function.

## Syntax

```lisp
(return)
(return ⟨value⟩)
```

## Semantics

- Returns the value from the nearest enclosing function.

## Example

```lisp
(define (f x)
  (when (even? x)
    (return Even))
  Odd)
```
