---
title: "try-as"
description: "Partial conversion shorthand."
hideMeta: true
weight: 120
---

`try-as` is the `TryInto` counterpart to `as`.

## Syntax

```lisp
(try-as ⟨type⟩ ⟨expr⟩)
(try-as ⟨type⟩)
```

## Semantics

- `(try-as ⟨type⟩ expr)` is shorthand for `(the (Optional ⟨type⟩) (tryInto ⟨expr⟩))`.
- The result is `Some` on success and `None` on failure.
- With one argument, it produces a unary function.

## Example

```lisp
(try-as U8 maybe-large-integer)
```
