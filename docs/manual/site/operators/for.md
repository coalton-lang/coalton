---
title: "for"
description: "Imperative loop form."
hideMeta: true
weight: 270
---

`for` is Coalton's built-in imperative loop form.

## Syntax

```lisp
(for (⟨binding-clause⟩...)
  [:returns ⟨expr⟩]
  [{:while | :until | :repeat} ⟨expr⟩]
  ⟨body⟩...)

;; ⟨binding-clause⟩ := (⟨var⟩ ⟨init-expr⟩ ⟨step-expr⟩)
```

## Semantics

- Initializers are established before the first iteration.
- Step expressions are evaluated in parallel from the previous iteration's
  bindings.
- `:while` loops while the condition is true.
- `:until` loops until the condition is true.
- `:repeat` loops a certain fixed number of times.
- `:returns` specifies what to return when the loop exits, either normally or by `(break)`.
- No `:returns` means the loop unifies with `Void`.
- Use `for*` when later steps should see earlier updated values.

## Example

```lisp
(for ((i 0 (1+ i)))
  :until (>= i 10)
  (show i))
```
