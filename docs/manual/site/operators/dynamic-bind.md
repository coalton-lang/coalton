---
title: "dynamic-bind"
description: "Dynamically rebind one or more dynamic variables."
hideMeta: true
weight: 155
---

`dynamic-bind` temporarily rebinds dynamic variables for the duration of a
body.

## Syntax

```lisp
(dynamic-bind ((*⟨name⟩* ⟨expr⟩) ...)
  ⟨body⟩)
```

## Semantics

- `dynamic-bind` only works with dynamic variables, which are variables defined
  with `define` and named like `*name*`.
- Bindings are parallel and non-recursive: each initializer is checked in the
  outer environment.
- Each rebinding must preserve the variable's existing type.
- When the body finishes, the old dynamic bindings are restored.

## Example

```lisp
(dynamic-bind ((*base* 20))
  (base-value))
```
