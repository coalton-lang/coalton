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
- Non-escaping local functions bound by `let` or `let*` (whose names are only called directly within their lexical scope and not returned, stored, or captured by an escaping closure) are declared `dynamic-extent`, allowing the Lisp compiler to stack-allocate their closures.
- In release mode, a locally bound `coalton/cell:Cell` created with `cell:new` that does not escape its lexical scope (used only through `cell:read`, `cell:write!`, `cell:swap!`, and inlined helpers such as `cell:update!`, `cell:increment!`, and `cell:decrement!`, including inside `for` loops, `match`/`if` branches, and non-escaping local functions) is unboxed into a typed mutable lexical variable with no heap allocation.
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
