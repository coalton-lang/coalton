---
title: "ƒ"
description: "Short fn syntax."
hideMeta: true
weight: 142
---

`ƒ` produces an equivalent `fn` form. The characters between `ƒ` and `.` are
positional arguments. Single alphabetic letters are named arguments, while `_`
indicates that the positional argument is ignored. The body of the function is
the next entire expression after `.`, which itself may be another `ƒ`
expression.

## Syntax

```lisp
ƒ⟨chars⟩.⟨expr⟩
```

`⟨chars⟩` may be empty. When it is present, it is a sequence of underscores or
non-repeated alphabetic characters. The underscore character may be repeated.

## Semantics

- `ƒx.x` is equivalent to `(fn (x) x)`.
- `ƒ.x` is equivalent to `(fn () x)`.
- `ƒxy.(+ x y)` is equivalent to `(fn (x y) (+ x y))`.
- `ƒx.ƒy.(+ x y)` is equivalent to `(fn (x) (fn (y) (+ x y)))`.
- `ƒ_x_.x` is equivalent to `(fn (_ x _) x)`.

The shorthand is available in Coalton source, including `.ct` files.

## Example

```lisp
(map ƒx.(+ x 2) [1 2 3])
```
