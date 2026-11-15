---
title: "ƒ"
description: "Short fn syntax."
hideMeta: true
weight: 142
---

`ƒ` is reader syntax that expands to an anonymous [`fn`](/manual/operators/fn/) expression.

## Syntax

```lisp
ƒ⟨param-char⟩*.⟨expr⟩

;; ⟨param-char⟩ := ⟨alphabetic-char⟩ | _
```

## Semantics

- The characters between `ƒ` (U+0192, Latin Small Letter F with Hook) and `.` specify the positional parameters of the generated [`fn`](/manual/operators/fn/):
  - Each alphabetic character (`cl:alpha-char-p`) becomes a single-character parameter variable interned in the current package (respecting the current readtable case).
  - Duplicate alphabetic parameter characters (after readtable case conversion) signal a reader error.
  - Each `_` character becomes a wildcard parameter [`_`](/manual/operators/wildcard/) and may appear multiple times.
  - When no characters appear between `ƒ` and `.` (`ƒ.⟨expr⟩`), the result is a nullary function `(fn () ⟨expr⟩)`.
- The body of the function is the single expression `⟨expr⟩` immediately following `.`, which may itself be another `ƒ` form:
  - `ƒx.x` expands to `(fn (x) x)`.
  - `ƒ.x` expands to `(fn () x)`.
  - `ƒxy.(+ x y)` expands to `(fn (x y) (+ x y))`.
  - `ƒx.ƒy.(+ x y)` expands to `(fn (x) (fn (y) (+ x y)))`.
  - `ƒ_x_.x` expands to `(fn (_ x _) x)`.
- `ƒ` does not support multi-character parameter names, constructor destructuring patterns, or [`&key`](/manual/operators/keyword-tail/) parameters; use [`fn`](/manual/operators/fn/) for those.

## Examples

```lisp
;; Unary short lambda
(coalton (map ƒx.(+ x 2) (the (List Integer) [1 2 3])))
; => (3 4 5)

;; Binary short lambda in a fold
(coalton (fold ƒab.(+ (* a 10) b) 0 (the (List Integer) [1 2 3 4])))
; => 1234

;; Ignoring arguments with `_`
(coalton (fold ƒn_.(+ n 1) 0 (the (List String) ["a" "b" "c"])))
; => 3
```

## Other Notes

- `ƒ` is available whenever the `coalton:coalton` readtable is active, including inside `.ct` files and inside `(coalton ...)` / `(coalton-toplevel ...)` forms.
- In the `mine` editor, `ƒ` can be entered by typing `Ctrl+\ fn`. In Emacs, it can be entered with `C-x 8 RET 0192 RET`.

