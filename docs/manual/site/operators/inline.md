---
title: "inline"
description: "Inlining attribute and application hint."
hideMeta: true
weight: 75
---

`inline` is a toplevel attribute on definitions, as well as
a call-site hint around an application.

## Syntax

```lisp
;; toplevel attribute
(inline)
(define ...)

;; applicationm hint
(inline (⟨function⟩ ⟨arg⟩ ...))
```

## Semantics

- As a standalone attribute, `(inline)` must appear immediately before
  `declare`, `define`, or an instance method definition.
- On a definition, it requests that Coalton inline the function at all sites.
- As an expression, `(inline application)` asks Coalton to inline that
  syntactic function application if possible.
- `inline` is a performance hint and should not change observable behavior.

## Example

```lisp
(coalton-toplevel
  (inline)
  (define (double x)
    (+ x x))
  
  (define (triple x)
    (+ x (double x)))

  (define (sextuple x)
    (inline (triple (double x)))))
```
