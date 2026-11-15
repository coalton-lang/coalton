---
title: "define-toplevel-macro"
description: "Define a macro for Coalton toplevel context."
hideMeta: true
weight: 76
---

`define-toplevel-macro` defines a macro that expands into one or more Coalton top-level forms.

## Syntax

```lisp
(define-toplevel-macro ⟨name⟩ ⟨lambda-list⟩
  ⟨docstring⟩?
  ⟨declaration⟩*
  ⟨lisp-body⟩+)
```

## Semantics

- `define-toplevel-macro` is a Common Lisp top-level form (used outside `coalton-toplevel`).
- `⟨lambda-list⟩` is a standard Common Lisp macro destructuring lambda list. (When writing macros in a package that uses `#:coalton` rather than `#:cl`, qualify lambda-list keywords such as `cl:&optional`, `cl:&rest`, `cl:&body`, and `cl:&key`.)
- The macro body executes as Common Lisp at compile time and returns a Coalton top-level form.
- When `⟨name⟩` is invoked inside [`coalton-toplevel`](/manual/operators/coalton-toplevel/), [`coalton-codegen`](/manual/operators/coalton-codegen/), [`pprint-coalton-codegen`](/manual/operators/pprint-coalton-codegen/), or related wrapper forms, the parser expands it and parses the result as a top-level form.
- To emit multiple declarations, attributes, or definitions from a single macro invocation, wrap the returned forms in `(coalton:progn ...)`.

## Examples

```lisp
;; Emit both a declaration and a definition
(define-toplevel-macro define-constant (name type value)
  "Declare and define a top-level constant."
  `(progn
     (declare ,name ,type)
     (define ,name ,value)))

;; Emit a transparent wrapper type together with an Into instance
(define-toplevel-macro define-tagged-id (name underlying-type)
  `(progn
     (repr :transparent)
     (derive Eq Hash Show)
     (define-type ,name
       (,name ,underlying-type))
     (define-instance (Into ,name ,underlying-type)
       (define (into (,name x)) x))))

(coalton-toplevel
  (define-constant default-port U16 8080)
  (define-tagged-id UserId U64))
```

## Other Notes

- `define-toplevel-macro` records `:toplevel` as the symbol's `:coalton-repl-context` property for editor integration, REPL support, and documentation generation.
- If a macro is meant to be called directly at the Common Lisp top level (outside `coalton-toplevel`) and expand into `(coalton-toplevel ...)`, define it with `cl:defmacro` instead.
- See [Macros](/manual/topics/macros/) for a comparison of expression macros, top-level macros, and Common Lisp macros.

