---
title: "Macros"
description: "Defining Coalton expression macros, Coalton toplevel macros, and ordinary Lisp macros."
hideMeta: true
weight: 55
---

Coalton macros are Common Lisp macros with an additional role: expression macros
expand where Coalton expressions are expected, and toplevel macros expand where
Coalton definitions and directives are expected.

Use ordinary `cl:defmacro` for Lisp macros. This includes bare Lisp macros that
expand into a complete `(coalton-toplevel ...)`, `(coalton-codegen ...)`, or
similar wrapper form.

## Expression Macros

Use [`define-expression-macro`](/manual/operators/define-expression-macro/) for
macros that expand into Coalton expression syntax.

```lisp
(define-expression-macro if-some ((name expr) then else)
  `(match ,expr
     ((Some ,name) ,then)
     ((None) ,else)))

(coalton
  (if-some (x maybe-count)
    (+ x 1)
    0))
```

## Toplevel Macros

Use [`define-toplevel-macro`](/manual/operators/define-toplevel-macro/) for
macros that expand inside `coalton-toplevel`, `coalton-codegen`,
`pprint-coalton-codegen`, and related forms.

```lisp
(define-toplevel-macro define-constant (name type value)
  `(progn
     (declare ,name ,type)
     (define ,name ,value)))

(coalton-toplevel
  (define-constant answer Integer 42))
```

## Lisp Macros

Use `cl:defmacro` when the macro is an ordinary Lisp macro. A macro that should
be used bare at the Lisp REPL and expand into a Coalton wrapper form belongs in
this category.

```lisp
(cl:defmacro define-answer ()
  `(coalton-toplevel
     (declare answer Integer)
     (define answer 42)))

(define-answer)
```

## Compatibility

[`defmacro`](/manual/operators/defmacro/) in the `coalton` package is retained
as a compatibility alias for `define-expression-macro`. New code should prefer
the explicit name.
