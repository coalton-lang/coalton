---
title: "define-resumption"
description: "Resumption definition form."
hideMeta: true
weight: 54
---

`define-resumption` defines a single-constructor resumption type used with [`resumable`](/manual/operators/resumable/) and [`resume-to`](/manual/operators/resume-to/).

## Syntax

```lisp
;; Nullary resumption
(define-resumption ⟨name⟩
  ⟨docstring⟩?)

;; Resumption carrying payload fields
(define-resumption (⟨name⟩ ⟨field-type⟩*)
  ⟨docstring⟩?)
```

## Semantics

- `define-resumption` is a top-level definition form valid inside [`coalton-toplevel`](/manual/operators/coalton-toplevel/).
- It defines both a resumption type `⟨name⟩` and its single constructor `⟨name⟩` in the current package:
  - When written `⟨name⟩` or `(⟨name⟩)`, `⟨name⟩` is a nullary constructor value of type `⟨name⟩`.
  - When written `(⟨name⟩ t1 ... tn)` with $n \ge 1$ fields, `⟨name⟩` is a constructor function of type `(t1 * ... * tn -> ⟨name⟩)`.
- Resumption types do not accept type variables, [`repr`](/manual/operators/repr/), or [`derive`](/manual/operators/derive/) attributes.
- Every resumption type automatically receives an instance of the marker class `Resumption` (required by [`resume-to`](/manual/operators/resume-to/)) and `RuntimeRepr`. Manual instances of `Resumption` cannot be written, and a resumption type cannot be redefined as a non-resumption type.
- Resumptions represent structured recovery protocols established by [`resumable`](/manual/operators/resumable/) and invoked via [`resume-to`](/manual/operators/resume-to/), typically from inside a [`handle`](/manual/operators/handle/) branch before the stack unwinds.

## Examples

```lisp
(coalton-toplevel
  (define-exception MissingKey
    (KeyNotFound String))

  ;; Nullary resumption and payload-carrying resumption
  (define-resumption SkipEntry
    "Skip the missing entry entirely.")

  (define-resumption (UseFallback Integer)
    "Supply a fallback integer for the missing entry.")

  (declare lookup-with-resumptions ((Optional Integer) -> (Optional Integer)))
  (define (lookup-with-resumptions entry)
    (resumable
        (match entry
          ((Some v) (Some v))
          ((None)   (throw (KeyNotFound "x"))))
      ((SkipEntry)       None)
      ((UseFallback alt) (Some alt))))

  (declare lookup-defaulting ((Optional Integer) -> (Optional Integer)))
  (define (lookup-defaulting entry)
    (handle (lookup-with-resumptions entry)
      ((KeyNotFound _) (resume-to (UseFallback 0))))))
```

## Other notes

- Under the hood, `define-resumption` compiles to a single-constructor data type together with a `coalton-impl/runtime:invoke-resumption` method that invokes the corresponding Common Lisp restart established by [`resumable`](/manual/operators/resumable/).
- See [`resumable`](/manual/operators/resumable/), [`resume-to`](/manual/operators/resume-to/), and [`handle`](/manual/operators/handle/).

