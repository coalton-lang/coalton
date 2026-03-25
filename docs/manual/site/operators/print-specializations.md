---
title: "print-specializations"
description: "List registered Coalton function specializations."
hideMeta: true
weight: 325
---

`print-specializations` shows which `specialize` declarations are currently
registered.

## Syntax

```lisp
(print-specializations)
(print-specializations ⟨package-designator⟩)
```

## Semantics

- The optional package designator narrows the output.
- Each entry shows the generic function type together with its specialized
  targets.
- This is mainly useful when checking whether a `specialize` directive was
  accepted and loaded.

## Example

```lisp
(print-specializations)
```
