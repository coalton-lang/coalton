---
title: "continue"
description: "Loop-iteration skip form."
hideMeta: true
weight: 300
---

`continue` skips the rest of the current `for` or `for*` iteration.

## Syntax

```lisp
(continue)
(continue ⟨label⟩)
```

## Semantics

- Without a label, it applies to the innermost loop.
- With a label, it targets a named enclosing loop.
- It proceeds to the next iteration after the loop's normal step handling.

## Example

```lisp
(for :outer ()
  (for ()
    (when satisfied?
      (continue :outer))
    (when skip?
      (continue))
    (process item)))
```
