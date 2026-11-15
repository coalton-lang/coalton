---
title: "defmacro"
description: "Compatibility alias for define-expression-macro."
hideMeta: true
weight: 77
---

`coalton:defmacro` is a compatibility alias for [`define-expression-macro`](/manual/operators/define-expression-macro/).

## Syntax

```lisp
(defmacro ⟨name⟩ ⟨lambda-list⟩
  ⟨docstring⟩?
  ⟨declaration⟩*
  ⟨lisp-body⟩+)
```

## Semantics

- `(coalton:defmacro ⟨name⟩ ⟨lambda-list⟩ ...)` expands directly to `(define-expression-macro ⟨name⟩ ⟨lambda-list⟩ ...)`.
- It defines a macro valid in Coalton expression contexts and marks `⟨name⟩` with `:expression` REPL context metadata.
- New code should prefer [`define-expression-macro`](/manual/operators/define-expression-macro/) for Coalton expression macros and [`define-toplevel-macro`](/manual/operators/define-toplevel-macro/) for Coalton top-level macros.

## Examples

```lisp
(defmacro when-some ((name expr) cl:&body body)
  `(match ,expr
     ((Some ,name)
      ,@body
      Unit)
     ((None)
      Unit)))
```

## Other Notes

- Because packages using `#:coalton` do not `:use #:cl`, Common Lisp lambda-list keywords such as `cl:&body`, `cl:&rest`, `cl:&optional`, and `cl:&key` must be `cl:`-qualified in the macro lambda list.
- To define an ordinary Common Lisp macro (such as a macro called outside `coalton-toplevel` that expands into `(coalton-toplevel ...)`), write `cl:defmacro` explicitly.
- See [Macros](/manual/topics/macros/) for guidance on choosing between `define-expression-macro`, `define-toplevel-macro`, and `cl:defmacro`.

