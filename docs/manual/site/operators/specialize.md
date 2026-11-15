---
title: "specialize"
description: "Register a specialized implementation for a generic function."
hideMeta: true
weight: 95
---

`specialize` is a top-level directive that registers a type-directed rewrite rule replacing calls to a general function with a more specific implementation when static types match.

## Syntax

```lisp
(specialize ⟨generic-fn⟩ ⟨specialized-fn⟩ ⟨specialized-type⟩)
```

## Semantics

- `specialize` is a top-level form valid inside [`coalton-toplevel`](/manual/operators/coalton-toplevel/). It does not accept attributes.
- Both `⟨generic-fn⟩` and `⟨specialized-fn⟩` must name existing top-level value or function definitions (not constructors or types).
- `⟨specialized-type⟩` must be an unconstrained type (without [`=>`](/manual/operators/constraint-arrow/) or [`forall`](/manual/operators/forall/)) that:
  - Matches the type scheme of `⟨specialized-fn⟩` (which may still be polymorphic over unconstrained type variables, such as `List :a -> List :a`).
  - Is strictly more specific than the unconstrained type of `⟨generic-fn⟩` (obtained by substituting one or more type variables of `⟨generic-fn⟩`).
- Two specializations for the same `⟨generic-fn⟩` whose `⟨specialized-type⟩` patterns overlap (unify) are rejected at compile time.
- During compilation, when a call to `⟨generic-fn⟩` has statically known types matching `⟨specialized-type⟩`, the optimizer rewrites the call to `⟨specialized-fn⟩` and drops any dictionary arguments that `⟨generic-fn⟩` would have required.
- Specialization is an optimization only and is not guaranteed to fire when types remain abstract; `⟨specialized-fn⟩` must have the same observable behavior as `⟨generic-fn⟩` on all inputs of `⟨specialized-type⟩`.
- Specialization is skipped when rewriting a call would discard constraints involving classes with [`overlap`](/manual/operators/overlap/) instances (or their subclasses), preserving dynamic dictionary selection.

## Examples

```lisp
(coalton-toplevel
  (declare inc (Num :a => :a -> :a))
  (define (inc x)
    (+ x 1))

  (declare inc-ufix (UFix -> UFix))
  (define (inc-ufix x)
    (lisp (-> UFix) (x)
      (cl:1+ x)))

  (specialize inc inc-ufix (UFix -> UFix)))

;; A call `(inc (the UFix 10))` is rewritten at compile time to `(inc-ufix 10)`.
```

## Other Notes

- Specialization rules are applied both during initial optimization and after [`monomorphize`](/manual/operators/monomorphize/) or [`inline`](/manual/operators/inline/) exposes concrete types at inner call sites.
- Use [`print-specializations`](/manual/operators/print-specializations/) at the Common Lisp REPL to list all active specialization rules.

