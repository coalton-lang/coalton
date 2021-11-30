# Coalton-Lisp Interoperation

## Introduction

Coalton is a language embedded in Lisp, and indeed, Coalton compiles to Lisp code. This document describes what Coalton promises for interoperation.

## Interaction Mode

First and foremost, there are two ways to globally compile Coalton. This is determined by an environment variable `COALTON_ENV` which in turns sets a global variable `*interaction-mode*`. There are two options for this variable:

- `COALTON_ENV=development`, which sets `*interaction-mode*` to `:development`
- `COALTON_ENV=release`, which sets `*interaction-mode*` to `:release`

As the names imply, these two modes have different behaviors that optimize for either development or release.

**WARNING**: *All* Coalton code, *including* the standard library, *must* be compiled in the same mode. We advise application writers to set the environment variable during their application's build process before Coalton is compiled, and to ensure caches are not stale.

By default, the interaction mode is `:development`.

### Development Mode

Development mode allows most structures to be re-defined. This means:

* Most types turn into CLOS classes.
* Most types are not flattened or unwrapped in any way.
* Certain optimizations that obscure debugging are disabled.

### Release Mode

Release mode freezes most structures and optimizes them.

* Most types are defined as frozen `defstruct`s or flattened Lisp data types.
* Many optimizations are performed.

### Pitfalls of Having Two Modes

Unfortunately, having two modes may mean one inadvertently depend on the behavior in one mode that is not supported in the other. We advise testing your code against both modes.

## Promises of Data and Basic Data Types

**PROMISE**: Every Coalton value exists as a Lisp value. The structure of said value may not always be defined, however, and must be considered an element of type `T` in general circumstances.

**PROMISE**: Coalton `Integer`, `Character`, `String`,  `Single-Float`, and `Double-Float` values correspond to their Lisp counterparts.

**PROMISE**: `COALTON:Boolean` is a Lisp `BOOLEAN`; `COALTON:True` is Lisp `T`, and `COALTON:False` is Lisp `NIL`.

**WARNING**: Lists and other data types may not correspond to Lisp's conception of them.

## Promises of  `define-type`

For the most part, `define-type` will not provide guarantees across interaction modes. One reason to understand why is to consider the type

```
(define-type Wrapper
  (Wrap Integer))
```

Depending on the interaction mode, `Wrap` may actually be similar to the `IDENTITY` function, and `Wrapper` may be representation-equivalent to `Integer`.

Nonetheless, there is one practical guarantee: the existence of Lisp functions for unconstrained constructors. *Unconstrained* means that there are no type class constraints on any type variables present in the type that describes the value. (For instance, in `Num :t => :s -> :t`, we say that `:s` is unconstrained and `:t` is constrained.) Consider the following type:

```
(define-type Foo
  (Ctor1 ...)
  Ctor2
  ...)
```

**PROMISE**: For each unconstrained constructor function (e.g., `Ctor1`), there will be a corresponding Lisp function function-bound to the constructor's name. This function will have the same arity as specified.

**PROMISE**: For each unconstrained nullary constructor (e.g., `Ctor2`), there will be a corresponding Lisp value bound to the constructor's name.

**WARNING**: Coalton gives no guarantee about the structure of the values that are constructed by the constructors in Lisp.

### Achieving Guaranteed Lisp Compatibility with `(REPR :LISP)`

The pragma `(repr :lisp)` helps achieve Lisp compatibility of structures regardless of interaction mode, at the expense of optimization opportunities. Consider a type:

```
(repr :lisp)
(define-type (Foo ...)
  (Ctor1 ...)
  Ctor2
  ...)
```

**PROMISE**: `FOO`, `FOO/CTOR1`, `FOO/CTOR2`, etc. will be valid symbols denoting disjoint class names checkable by `typep` and dispatchable by `defmethod`.

**WARNING**: It is undefined whether these classes are `standard-class` or `structure-class` or something else.

**PROMISE**: `(subtypep 'FOO/* 'FOO)` will be true for valid substitutions of `*`.

**PROMISE**: Even if there is only a single constructor, a separate disjoint type will be created.

**PROMISE**: Non-nullary, unconstrained constructors (e.g., `CTOR1`) will be Lisp functions that return the type as specified in the previous promise.

**PROMISE**: Nullary, unconstrained constructors (e.g., `CTOR2`) will be Lisp values that are of the return type specified in the previous promise.

**HALF-HEARTED PROMISE**: For each non-nullary constructor (say `Ctor1`), a `setf`-able accessor function called `<class-name>/<ctor-name>-_<k>` will be defined for the `k`th member of that constructor. For the example above, suppose that `Ctor1` has two fields. Then the accessor functions `FOO/CTOR1-_0` and `FOO/CTOR1-_1` will be defined. (*Note*: The naming here is subject to change.)

## Promises of `define`

Consider the following definitions:

```
(define v       x1)
(define (f ...) x2)
```

**PROMISE**: Assuming they are unconstrained, `v` and `f` will be lexical variables in Lisp bound to those symbols.

**PROMISE**: Assuming it is unconstrained, `f` will be a function in Lisp of the same arity found at the definition site.

## Lisp-Calls-Coalton Bridge

There are two ways to call Coalton from Lisp.

### Safe Calls

The safe way to call Coalton is to use the `coalton` operator. This will type check, compile, and evaluate a Coalton expression and return its value to Lisp. Note that `coalton` does not accept definitions or top-level forms. For example:

```lisp
CL-USER> (format t "~R" (coalton:coalton coalton-library::(length (cons 1 (cons 2 nil)))))
two     ; <- printed output
NIL     ; <- returned value
CL-USER> (format t "~R" (coalton:coalton coalton-library::(length 1)))
; (Guaranteed Compile-Time Error:)
;
; Failed to unify types COALTON:INTEGER 
;                   and (COALTON-LIBRARY:LIST :B)
; in unification of types (COALTON:INTEGER → :A)
;                     and ((COALTON-LIBRARY:LIST :B) → COALTON:INTEGER)
; in COALTON
```

### Unsafe Calls

Using the aforementioned promises, it's possible to call into raw Coalton-compiled code by using the generated functions. These calls are not checked in any way!

```lisp
CL-USER> (format t "~R" coalton-library::(length (cons 1 (cons 2 nil))))
two     ; <- printed output
NIL     ; <- returned value
CL-USER> (format t "~R" coalton-library::(length 1))
; (Possible Run-Time Error #1:)
;
; The value
;   1
; is not of type
;   COALTON-LIBRARY:LIST
; when binding COALTON-LIBRARY::L-35

; (Possible Run-Time Error #2:)
;
; SEGMENTATION FAULT
```

We do *not* recommend raw calls of Coalton-compiled functions, as they may be optimized and may be unsafe to call.

## Coalton-Calls-Lisp Bridge

Coalton can call Lisp code with the `lisp` operator, which has the syntax:

```
(lisp <return-type> (<captured-variable> ...)
  <lisp-code-body>)
```

Here, `<return-type>` should be a valid Coalton type which _you_ are guaranteeing to Coalton is the correct type of the return value of `<lisp-code-body>`. A programmer should see this assertion as analogous to `cl:the`.

**WARNING**: Coalton does not even guarantee run-time type safety with a dynamic check. It may in the future, though.

Each `<captured-variable>` refers to a lexical variable in the surrounding Coalton code that you would like to capture. The values in these lexical variables obey the aforementioned promises of this document. For instance, here is Coalton's definition of the greatest common divisor function `coalton-library:gcd`:

```
(declare gcd (Integer -> Integer -> Integer))
(define (gcd a b)
  "Compute the greatest common divisor of A and B."
  (lisp Integer (a b)
    (cl:gcd a b)))
```

Here, the values of the parameters `a` and `b` are captured for use inside of the `lisp` form.

## Coalton's `Lisp-Object` Type

For low-level code, such as in the standard library, it is sometimes necessary to stash a Lisp object in a Coalton data type. This is done by specifying a field in an algebraic data type to be `Lisp-Object`.

**PROMISE**: `Lisp-Object`s are not wrapped in a runtime representation in any way.

**PROMISE**: For a suitable, unconstrained constructor function that takes a `Lisp-Object`, one may safely call that constructor directly on a Lisp object of interest.

`Lisp-Object`s should, in general, be hidden from users.

## Soundness of Coalton-Lisp Interop

As with almost any "foreign function interface" (especially those interfacing with C), there's lots of potential for type errors to be made that Coalton simply cannot check for at compile time. We recommend being extra cautious and being liberal with `check-type`s and other assertions when putting a value into the hands of Coalton-managed code.

Always be vigilant about _all_ possible return types of Lisp functions. Lisp, being a dynamically typed language, is sometimes lax about the possible return types, especially as they relate to the input type. Things like numerical contagion, `nil`, and error conditions can be serious sources of type errors.