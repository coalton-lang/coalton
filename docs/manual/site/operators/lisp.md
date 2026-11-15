---
title: "lisp"
description: "Inline Common Lisp form."
hideMeta: true
weight: 190
---

`lisp` embeds raw Common Lisp code inside a Coalton expression.

## Syntax

```lisp
;; Single return value
(lisp (-> ⟨type⟩) (⟨var-spec⟩...)
  ⟨lisp-form⟩+)

;; Zero return values
(lisp (-> Void) (⟨var-spec⟩...)
  ⟨lisp-form⟩+)

;; Multiple return values
(lisp (-> ⟨type-1⟩ * ⟨type-2⟩ * ...) (⟨var-spec⟩...)
  ⟨lisp-form⟩+)

;; ⟨var-spec⟩ := ⟨coalton-var⟩
;;             | (⟨lisp-var⟩ ⟨coalton-var⟩)
```

## Semantics

- `lisp` evaluates `⟨lisp-form⟩+` as an implicit `cl:progn` in Common Lisp with lexical bindings established by `(⟨var-spec⟩...)`.
- Each `⟨var-spec⟩` captures a Coalton variable in scope:
  - A bare `⟨coalton-var⟩` binds the same symbol in the Common Lisp body.
  - `(⟨lisp-var⟩ ⟨coalton-var⟩)` binds the Common Lisp symbol `⟨lisp-var⟩` to the value of `⟨coalton-var⟩`.
  - If a captured Coalton variable has typeclass constraints, its required typeclass dictionaries are resolved and applied before binding.
- Coalton does not typecheck the interior of `⟨lisp-form⟩+`; it trusts the declared output specification `(-> ...)`:
  - `(-> ⟨type⟩)` wraps the final Lisp form in `(cl:values ...)` so any extra Common Lisp return values are discarded and a single value of `⟨type⟩` is returned.
  - `(-> Void)` evaluates the final Lisp form for side effects and returns zero values `(cl:values)`.
  - `(-> ⟨type-1⟩ * ⟨type-2⟩ * ...)` returns the multiple Common Lisp values of the final form directly.
  - `(-> :r)`, where `:r` is a scoped [`Values`](/manual/operators/forall/) type variable, forwards all values returned by the final Lisp form without arity truncation.
- Scoped type variables introduced by an enclosing [`forall`](/manual/operators/forall/) may be referenced in the return type.

## Examples

```lisp
;; Call a single-value Common Lisp function
(declare random-below (Integer -> Integer))
(define (random-below n)
  (lisp (-> Integer) (n)
    (cl:random n)))

;; Rename captured variables for the Lisp body
(declare format-pair (Integer * String -> String))
(define (format-pair x label)
  (lisp (-> String) ((num x) (str label))
    (cl:format cl:nil "~A: ~D" str num)))

;; Return multiple values from Common Lisp
(declare divmod (Integer * Integer -> Integer * Integer))
(define (divmod a b)
  (lisp (-> Integer * Integer) (a b)
    (cl:floor a b)))

;; Perform a side effect returning zero values (Void)
(declare write-newline (Void -> Void))
(define (write-newline)
  (lisp (-> Void) ()
    (cl:terpri)))
```

## Other Notes

- By default, when host type annotations are emitted, Coalton wraps the `lisp` form in `(cl:the (cl:values ...) ...)` and enables runtime return-type checking (`(sb-c::type-check 1)` on SBCL) so type mismatches at the Lisp boundary are caught immediately. Wrapping the expression in [`unsafe`](/manual/operators/unsafe/) disables this runtime check.
- To call a first-class Coalton function `f` passed into a `lisp` block, use `(coalton:call-coalton-function f arg...)` or `(cl:funcall f arg...)`.
- Do not use `cl:return` or `cl:return-from` to jump out of a `lisp` form into surrounding Coalton control flow; use Coalton's [`return`](/manual/operators/return/) outside the `lisp` block instead.

