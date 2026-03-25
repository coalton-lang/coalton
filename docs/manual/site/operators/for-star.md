---
title: "for*"
description: "Sequential imperative loop form."
hideMeta: true
weight: 280
---

`for*` is the sequential counterpart to `for`.

## Syntax

```lisp
(for (⟨binding-clause⟩...)
  [:returns ⟨expr⟩]
  [{:while | :until | :repeat} ⟨expr⟩]
  ⟨body⟩...)

;; ⟨binding-clause⟩ := (⟨var⟩ ⟨init-expr⟩ ⟨step-expr⟩)
```

## Semantics

- Initializers follow `let*`-style top-to-bottom scoping.
- Step expressions are also performed top to bottom.
- Use `for*` when later loop state should depend on earlier updated values.
- `:while` loops while the condition is true.
- `:until` loops until the condition is true.
- `:repeat` loops a certain fixed number of times.
- `:returns` specifies what to return when the loop exits, either normally or by `(break)`.
- No `:returns` means the loop unifies with `Void`.
- Use `for*` when later steps should see earlier updated values.

## Example

```lisp
(for* ((x 1 (+ x y))
       (y 1 x))
  :until (> x 10)
  (show x))
```
