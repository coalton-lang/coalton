---
title: "define-exception"
description: "Exception type definition form."
hideMeta: true
weight: 52
---

`define-exception` defines an exception type whose values can be signaled with [`throw`](/manual/operators/throw/) and caught with [`catch`](/manual/operators/catch/) or [`handle`](/manual/operators/handle/).

## Syntax

```lisp
;; Coalton-defined exception algebraic data type
(define-exception ⟨name⟩
  ⟨docstring⟩?
  ⟨constructor-spec⟩*)

;; Existing Common Lisp condition type
(repr :native ⟨lisp-condition-type⟩)
(define-exception ⟨name⟩
  ⟨docstring⟩?)

;; ⟨constructor-spec⟩ := ⟨ctor-name⟩ ⟨docstring⟩?
;;                     | (⟨ctor-name⟩ ⟨field-type⟩*) ⟨docstring⟩?
```

## Semantics

- `define-exception` is a top-level definition form valid inside [`coalton-toplevel`](/manual/operators/coalton-toplevel/).
- Its constructor syntax matches [`define-type`](/manual/operators/define-type/), including optional docstrings on the exception type and after each constructor.
- Exception types cannot be parameterized by type variables.
- Exception constructors are ordinary Coalton constructors: they can be called to construct exception values, stored in data structures, or passed to functions before being passed to [`throw`](/manual/operators/throw/).
- Every exception type automatically receives an instance of the marker class `Exception` (required by `throw`) and `RuntimeRepr`. Manual instances of `Exception` cannot be written, and an exception type cannot be redefined as a non-exception type.
- **Native Lisp conditions**: With `(repr :native ⟨lisp-condition-type⟩)`:
  - `⟨name⟩` wraps an existing Common Lisp condition type rather than defining new constructors (`⟨constructor-spec⟩*` must be empty).
  - `⟨lisp-condition-type⟩` must be known at compile time and must be a subtype of `cl:serious-condition`. (If defined via `cl:define-condition` in the same file, wrap the `cl:define-condition` in `(eval-when (:compile-toplevel :load-toplevel :execute) ...)`.)
  - Native exception values are caught with `((the ⟨name⟩ var) ...)` in [`catch`](/manual/operators/catch/) or [`handle`](/manual/operators/handle/) (which also matches any Common Lisp subtype of `⟨lisp-condition-type⟩`) or constructed via [`lisp`](/manual/operators/lisp/).
- `define-exception` does not accept [`derive`](/manual/operators/derive/) or any [`repr`](/manual/operators/repr/) mode other than `:native`.

## Examples

```lisp
(coalton-toplevel
  ;; Exception sum type with multiple constructors
  (define-exception HttpError
    "Errors raised while handling HTTP requests."
    (NotFound String)
    (Unauthorized String)
    Timeout)

  ;; Bind an existing Common Lisp condition type
  (repr :native cl:division-by-zero)
  (define-exception DivisionByZero
    "Common Lisp's DIVISION-BY-ZERO condition.")

  (declare safe-recip (Integer -> (Optional Fraction)))
  (define (safe-recip n)
    (catch (Some (/ 1 (fromInt n)))
      ((the DivisionByZero _) None))))
```

## Other notes

- Coalton-defined exception types compile to Common Lisp condition types inheriting from `cl:error`, with a distinct condition subclass for each constructor.
- The `coalton/exception` standard library package predefines native exception types for standard Common Lisp conditions (including `LispError` for `cl:error` and `Panic` for Coalton panics).
- See [`throw`](/manual/operators/throw/), [`catch`](/manual/operators/catch/), and [`handle`](/manual/operators/handle/).

