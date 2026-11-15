---
title: "kind-of"
description: "Inspect the kind of a Coalton type constructor."
hideMeta: true
weight: 305
---

`kind-of` is a Common Lisp function that returns the kind of a Coalton type, struct, or type alias.

## Syntax

```lisp
(kind-of ⟨symbol⟩)
```

## Semantics

- `kind-of` is an ordinary Common Lisp function (called outside `coalton`), and `⟨symbol⟩` must evaluate to a `cl:symbol`.
- It looks up `⟨symbol⟩` in Coalton's global type environment (which includes types defined with [`define-type`](/manual/operators/define-type/), [`define-struct`](/manual/operators/define-struct/), [`define-exception`](/manual/operators/define-exception/), [`define-resumption`](/manual/operators/define-resumption/), and [`define-type-alias`](/manual/operators/define-type-alias/)).
- If `⟨symbol⟩` is found, `kind-of` returns its `coalton-impl/typechecker:kind` object; otherwise it returns `cl:nil`.
- Printed kinds display the base kind `Type` as `*` and higher-order kinds with `->` (or `→` when Unicode printing is enabled), associating to the right.

## Examples

```lisp
(kind-of 'Integer)
; => *

(kind-of 'List)
; => * → *

(kind-of 'Result)
; => * → (* → *)

(kind-of 'UnknownType)
; => COMMON-LISP:NIL
```

## Other Notes

- In [`forall`](/manual/operators/forall/) binder annotations, kinds are written using `Type`, `Values`, and `->` (for example, `(:f (Type -> Type))`), even though `kind-of` prints `Type` as `*`.
- To print the kinds of all types in a package, call `(coalton:print-type-db OptionalPackage)` at the Common Lisp REPL.

