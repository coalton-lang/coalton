---
title: "declare"
description: "Type declaration form."
hideMeta: true
weight: 30
---

`declare` gives an explicit type to a value, function, or local binding.

## Syntax

```lisp
(declare ⟨name⟩ ⟨type⟩)
```

## Semantics

- Top-level `declare` is commonly paired with `define`.
- Local `declare` also works inside binding lists of `let`, `let*`, `rec`,
  `for`, and `for*`.
- Coalton infers types, but `declare` is the main way to make intent explicit.
- Using `forall` makes the type variables scoped to the variable's definition.

## Example

```lisp
(declare x Integer)

;; Scoped type variables
(declare keep-first (forall (:a :b) :a -> :b -> :a))

;; Inside of a binding list
(let ((declare x Integer)
      (x 5))
  (1+ x))
```
