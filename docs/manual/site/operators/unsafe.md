---
title: "unsafe"
description: "Experimental escape hatch from runtime safety checks."
hideMeta: true
weight: 100
---

`unsafe` is an experimental expression form exported from `#:coalton++`.

## Syntax

```lisp
(unsafe
  ⟨expr⟩...)
```

## Semantics

- `unsafe` lexically disables generated safety for its body.
- It is intended for tightly controlled low-level code, usually around interop
  or performance-sensitive internals.
- Coalton still typechecks the surrounding program, but generated runtime
  checks inside the body are relaxed.
- Misuse can produce undefined behavior in the generated Lisp.
- Specifically disables type checking in `lisp` forms.

## Example

```lisp
(coalton
  (coalton++:unsafe
    (lisp (-> Integer) ()
      (cl:the fixnum 42))))
```
