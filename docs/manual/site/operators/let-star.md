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
- Non-escaping local functions bound by `let*` are declared `dynamic-extent`, allowing the Lisp compiler to stack-allocate their closures.
- In release mode, a locally bound `coalton/cell:Cell` created with `cell:new` that does not escape its lexical scope is unboxed into a typed mutable lexical variable with no heap allocation.
- Bound variables may be declared with `declare`.
- Multiple values may be destructured with `values`.
- `⟨body⟩` forms an implicit `progn`.

## Example

```lisp
(let* ((x 1)
       (y (+ x 1)))
  (+ x y))
```
