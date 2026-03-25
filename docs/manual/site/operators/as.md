---
title: "as"
description: "Total conversion shorthand."
hideMeta: true
weight: 110
---

`as` is a convenience macro for total conversions via `Into`.

## Syntax

```lisp
(as ⟨type⟩ ⟨expr⟩)
(as ⟨type⟩)
```

## Semantics

- `(as <type> <expr>)` is shorthand for `(the <type> (into <expr>))`.
- With one argument, `(as <type>)` produces a unary converting function.
- Use it only for total conversions, not `TryInto`.

## Example

```lisp
(as (List Char) "mississippi")
```
