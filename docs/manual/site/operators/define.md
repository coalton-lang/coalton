---
title: "define"
description: "Value and function definition form."
hideMeta: true
weight: 40
---

`define` introduces top-level values and functions.

## Syntax

```lisp
⟨monomorphize-directive⟩?
⟨inline-directive⟩?
⟨declare-directive⟩?
(define ⟨def⟩ ⟨docstring⟩? ⟨expr⟩)

;; ⟨def⟩ := ⟨name⟩
;;        | *⟨name⟩*
;;        | (⟨name⟩ ⟨arg⟩...)
;;
;; ⟨arg⟩ := ⟨name⟩
;;        | _
;;        | ⟨pattern⟩
```

## Semantics

- Defines a new value or function.
- Type is automatically inferred, but a `declare` is recommended.
- Can be inlined, monomorphized, or specialized.
- Parameters which are not referenced can be named `_` or `_x` where `x` can be
  any name.
- If an argument is a data type with one constructor, a direct pattern match
  can be used.
- The body of a function definition can `return` explicitly.
- When `⟨name⟩` begins and ends with `*`, as in `*base*`, then `define` defines
  a dynamic variable. Otherwise, it defines an ordinary lexical variable.

## Example

```lisp
(define (add2 x)
  (+ x 2))

;; dynamic variable
(define *base* 10)
```
