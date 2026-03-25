---
title: "let*"
description: "Sequential lexical binding form."
hideMeta: true
weight: 170
---

`let*` introduces lexical bindings sequentially, and is the sequential counterpart to `let`.

## Syntax

```lisp
(let* (⟨binding⟩...)
  ⟨body⟩...)

;; ⟨binding⟩ := ⟨declare-form⟩
;;            | (⟨var⟩ ⟨expr⟩)
;;            | ((values ⟨var⟩...) ⟨expr⟩)
```

## Semantics

- `let*` binds non-recursively and sequentially top-to-bottom.
- Bound variables may be declared with `declare`.
- Multiple values may be destructured with `values`.
- `⟨body⟩` forms an implicit `progn`.

## Example

```lisp
(let* ((x 1)
       (y (+ x 1)))
  (+ x y))
```
