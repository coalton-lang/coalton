---
title: "let"
description: "Recursive lexical binding form."
hideMeta: true
weight: 160
---

`let` introduces lexical bindings. It is similar to Scheme's `letrec`.

## Syntax

```lisp
(let (⟨binding⟩...)
  ⟨body⟩...)

;; ⟨binding⟩ := ⟨declare-form⟩
;;            | (⟨var⟩ ⟨expr⟩)
;;            | ((values ⟨var⟩...) ⟨expr⟩)
```

## Semantics

- `let` binds recursively and in parallel.
- It can introduce local helper functions as well as simple values.
- If you need top-to-bottom binding order, use `let*`.
- Bound variables may be declared with `declare`.
- Multiple values may be destructured with `values`.
- `⟨body⟩` forms an implicit `progn`.

## Example

```lisp
(let ((declare x Integer)
      (x (+ y 1))
      (y 2))
  (+ x y))
```
