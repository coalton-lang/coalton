---
title: "="
description: "Pattern binding operator."
hideMeta: true
weight: 50
---

`=` is a binding form that lets you
keep the original value while also destructuring it.

## Syntax

```lisp
(= ⟨name⟩ ⟨pattern⟩)
```

## Semantics

- The left side receives the entire matched value.
- The right side is an ordinary pattern that can destructure the same value.
- This is useful when you need both the original value and some of its parts.

## Example

```lisp
(define (nest-right (= tpl (Tuple a _)))
  (Tuple a tpl))
```

Here `tpl` names the entire tuple, while `a` names its first field.

## Other Notes

`=` is a token used in a variety of places in Coalton, including short-`let` and comprehensions:

```
(let x = 5)
[x :with n = 10 :for ...]
```