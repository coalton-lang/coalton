---
title: "break"
description: "Loop exit form."
hideMeta: true
weight: 290
---

`break` exits a `for` or `for*` loop.

## Syntax

```lisp
(break)
(break ⟨label⟩)
```

## Semantics

- Without a label, `break` exits the innermost loop.
- With a label, it exits the named enclosing loop.
- It is the main structured early-exit mechanism for imperative loops.

## Example

```lisp
(for :outer ()
  (for :inner ()
    (when satisfied?
      (break))
    (when done?
      (break :outer))))
```
