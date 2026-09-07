---
title: "overlap"
description: "Permit explicit overlapping type class instances."
hideMeta: true
weight: 91
---

`overlap` permits an instance to overlap other explicitly marked instances.
Put the attribute immediately before `define-instance`:

```lisp
(define-class (Label :a)
  (label (:a -> String)))

(overlap)
(define-instance (Label :a)
  (define (label _) "value"))

(overlap)
(define-instance (Label Integer)
  (define (label _) "integer"))

(label True)                ; "value"
(label (the Integer 42))    ; "integer"
```

Every distinct instance participating in an overlap must have the attribute.
It takes no arguments and does not assign priority.

## Selection

Selection compares only instance heads. A head is more specific if it can be
obtained by substituting types into another head, but not conversely. Definition
order has no effect. The unique most-specific matching head wins; its context
must then be satisfied. An unsatisfied context is an error, not a reason to try
another instance.

Sometimes two matching heads are incomparable: neither is more specific than
the other. These definitions can coexist, but a use matching both is ambiguous
unless another marked instance resolves their intersection. See the examples
below.

The attribute does not relax functional-dependency consistency, kind checking,
or the requirements on method definitions. Equivalent heads remain instance
redefinitions under the usual redefinition rules.

## Polymorphic functions

Using the `Label` instances above, a function that calls `label` must use the
appropriate instance for each argument type:

```lisp
(declare describe (Label :a => :a -> String))
(define (describe x) (label x))

(describe True)             ; "value"
(describe (the Integer 42)) ; "integer"
```

When compiling the body of `describe`, the compiler knows only that `x` has
type `:a`. Choosing the general `Label :a` instance at that point would make
both calls return `"value"`, ignoring the `Label Integer` instance. The
`Label :a =>` constraint keeps that choice with the caller: the compiler passes
the selected `label` implementation to `describe` in an implicit dictionary
argument. The call to `label` inside `describe` uses that dictionary.

If you omit the entire `declare` form, the compiler infers
`(Label :a => :a -> String)` from the body. If you instead explicitly declare
`(declare describe (:a -> String))`, the definition is rejected because the
declaration omits the required `Label :a` constraint.

This requirement also holds when only the general marked instance has been
defined: a more-specific instance may be added in another compilation unit.
Constraints such as `Label (List :a)` are retained too, since the element type
is still unknown and could affect instance selection.

## Efficiency

Instance selection happens during compilation. Calling an overloaded method
does not search an instance table at runtime. A polymorphic function instead
receives dictionaries containing the methods for its retained constraints.
Retaining a constraint can therefore add dictionary arguments and indirect
method calls when the dictionaries are unknown to the optimizer.

The compiler preserves passed dictionaries and superclass fields because the
chosen methods affect program behavior. Type-based
[`specialize`](/manual/operators/specialize/) replacements are skipped when
they would discard constraints involving marked instances or their subclasses.
This restriction also applies to unmarked instances of a class that has marked
instances.
Inlining known dictionary methods remains available, so concrete calls can
still become direct calls or inline code.

Separate-compilation validation adds work when compiling and loading units,
and when adding or redefining instances. It does not add a validation check to
each method call.

## Separate compilation

Compiled units record the instance choices used during checking and code
generation. Loading a unit validates those choices against the current instance
environment before installing its definitions. Adding or redefining instances
also validates previously recorded choices. A new instance that would change a
compiled choice causes an error in either load order.

For example, after compiling a concrete call to the general `Label :a` instance
at `Integer`, adding `Label Integer` requires rebuilding the affected code.
A generic `describe` that retains its `Label :a` dictionary parameter can accept
the new instance without recompilation.

To rebuild after an incompatible extension, use a fresh Lisp/compiler environment,
load the complete instance set, and then recompile its clients. Validation is
conservative and may retain assumptions from compiler constraint solving as well
as emitted calls. There is no operation to unload compiled assumptions selectively
or silently retarget already compiled calls. The checks apply to both Coalton
source files and `coalton-toplevel`/`coalton` forms compiled into Lisp FASLs.
Use a clean rebuild when updating the compiler: every participating compiled
unit must carry its instance assumptions.

## Examples

### Resolving an intersection

These first two instances describe pairs with an integer in either position.
The third specifies the behavior when both positions are integers:

```lisp
(define-class (PairLabel :a :b)
  (pair-label (:a * :b -> String)))

(overlap)
(define-instance (PairLabel :a Integer)
  (define (pair-label _ _) "integer on the right"))

(overlap)
(define-instance (PairLabel Integer :b)
  (define (pair-label _ _) "integer on the left"))

(overlap)
(define-instance (PairLabel Integer Integer)
  (define (pair-label _ _) "two integers"))

(pair-label True (the Integer 1))           ; "integer on the right"
(pair-label (the Integer 1) True)           ; "integer on the left"
(pair-label (the Integer 1) (the Integer 2)) ; "two integers"
```

Without the third instance, the last call is ambiguous. Reordering the first
two definitions does not resolve it.

### Generic library conversions

The standard library uses overlapping instances to provide identity conversions
alongside generic collection conversions:

```lisp
(the (Tuple Integer String) (into (Tuple (the Integer 42) "answer")))
; (Tuple 42 "answer")
(the (Seq Integer) (into (Some (the Integer 42))))
```

The last expression builds a one-element sequence through the generic
`Foldable` conversion. Identity conversion of a `Seq` is handled by an explicit
intersection instance and returns the existing sequence.

A polymorphic wrapper retains the conversion constraint:

```lisp
(declare to-seq
  (Into (:f :a) (Seq :a) => :f :a -> Seq :a))
(define (to-seq xs) (into xs))

(to-seq (Some (the Integer 42)))
(to-seq (the (List Integer) (make-list 1 2)))
```

The wrapper works with user-defined foldable collections as well. Its callers
select the conversion, so a later specialization is respected. Declaring only
`Foldable :f` and `RuntimeRepr :a` would not suffice: those are the
context of one candidate instance, and another instance may be selected.
Likewise, a polymorphic identity conversion needs `Into :a :a`; ordinary
identity functions can simply return their argument.
