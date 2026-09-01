---
title: "Advanced Coalton Features"
description: "Guide to advanced Coalton features."
hideMeta: true
weight: 20
---

Coalton contains several advanced features. These can be used to create powerful and expressive code, but they require a good understanding of Coalton's fundamentals to use. Before reading this document, make sure that you have a good grasp of the [Coalton Language Guide](./manual/site/topics/language-guide.md). In particular, these features build on Algebraic Data Types (uses of `define-type`) and Typeclasses (uses of `define-class`).

{{<toc>}}

## Instance Defaulting

Coalton has a similar [type defaulting system](https://www.haskell.org/onlinereport/decls.html#sect4.3.4) as Haskell. Type defaulting is invoked on implicitly typed definitions and code compiled with the `coalton` macro. Defaulting is applied to a set of ambiguous predicates, with the goal to resolve an ambiguous type variable to a valid type. Coalton defaults ambiguous numeric variables by trying `Integer`, then `F64`, then `F32`, taking the first type that satisfies the relevant predicates. Unlike Haskell 98, Coalton can still default some predicates involving multi-parameter classes or more structured types, provided the ambiguous predicates do not introduce any other type variables.


Differences from Haskell 98. Haskell would consider `Num (List :a)` to be ambiguous, Coalton would default it to `Num Integer`. Haskell would consider (`Num :a` `CustomTypeClass :a`) to be ambiguous, Coalton would default to (`Num Integer` `CustomTypeClass Integer`) assuming `CustomTypeClass Integer` was a valid instance.

## Functional Dependencies

Functional dependencies allow enforcing relations on the type variables of a class to improve type inference.

A class `C` can be given a functional dependency `(:a -> :b)` like so:

`(define-class (C :a :b (:a -> :b)))`

`(:a -> :b)` can be read as: foreach `:a` there will be only one `:b` or alternatively the value of `:b` is uniquely determined by `:a`. 

If the instance `(C String Integer)` was defined, then it would be invalid to define `(C String Char)` because there are multiple values of `:b` for the same value of `:a`.

Classes can have multiple functional dependencies, each dependency can list multiple class variables on each side `(:a :b -> :c :d :e)`, and dependencies can be recursive `(:a -> :b) (:b -> :a)`.

## Specialization

Coalton supports optimistic type based function specialization. Function specializations are declared with a `specialize` form:

```
(coalton-toplevel
  (declare inc (Num :a => :a -> :a))
  (define (inc x)
    (trace "standard call")
    (+ x 1))

  (declare inc-int (Integer -> Integer))
  (define (inc-int x)
    (trace "int specialized call")
    (+ x 1))

  (specialize inc inc-int (Integer -> Integer)))
```

When `inc` is called with an integer, the call will be transparently rewritten to call `inc-int`.

```
COALTON-USER> (coalton (inc 1.2))
standard call
2.2
COALTON-USER> (coalton (inc 1))
int specialized call
2
```

Specialization can only apply when the argument types at a call site are known. Because specialization is not guaranteed, specialized functions must have the same behavior as their unspecialized variants. Specialization should only be used for performance. See the following example:

```
(coalton-toplevel
  (declare inc2 (Num :a => :a -> :a))
  (define (inc2 x)
    (inc x)))
```

Because the type of `x` in the body of `inc2` is not known, specialization will not apply.

```
COALTON-USER> (coalton (inc2 1))
standard call
2
```

Specialization can be listed in the repl with `print-specializations`.
