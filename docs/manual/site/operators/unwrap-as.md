---
title: "unwrap-as"
description: "Unchecked conversion shorthand."
hideMeta: true
weight: 130
---

`unwrap-as` combines `TryInto` with `unwrap`.

## Syntax

```lisp
(unwrap-as ⟨type⟩ ⟨expr⟩)
(unwrap-as ⟨type⟩)
```

## Semantics

- It attempts a `TryInto` conversion and then unwraps the resulting `Optional`.
- Use it only when failure is genuinely exceptional.
- With one argument, it produces a unary function.

## Example

```lisp
(unwrap-as U16 exact-small-value)
```
