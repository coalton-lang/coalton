---
title: "define-toplevel-macro"
description: "Define a macro for Coalton toplevel context."
hideMeta: true
weight: 76
---

`define-toplevel-macro` defines a macro that expands into Coalton toplevel
forms.

## Syntax

```lisp
(define-toplevel-macro name lambda-list
  body ...)
```

## Semantics

- It has the same expansion model as `cl:defmacro`.
- The macro body runs as Common Lisp and returns Coalton source forms.
- The macro is intended for use inside `coalton-toplevel`, `coalton-codegen`,
  `pprint-coalton-codegen`, and related forms.
- Coalton-aware REPLs may use this role to evaluate bare calls as Coalton
  toplevel forms.

## Example

```lisp
(define-toplevel-macro define-constant (name type value)
  `(progn
     (declare ,name ,type)
     (define ,name ,value)))

(coalton-toplevel
  (define-constant answer Integer 42))
```
