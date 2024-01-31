# Coalton-Lisp Interoperation

## Introduction

Coalton is a language embedded in Lisp, and indeed, Coalton compiles to Lisp code. This document describes what Coalton promises for interoperation.

## Interaction Mode

First and foremost, there are two ways to globally compile Coalton. This is determined by
an environment variable `COALTON_ENV` which in turn sets a Lisp feature `:coalton-release`
and controls a Lisp function `(coalton-release-p)`.

If `COALTON_ENV=release`, then `:coalton-release` will appear in `*features*`, and
`(coalton-release-p)` will return a true (non-null) value.

Otherwise, Coalton is in development mode, `*features*` will not contain `:coalton-release`, and `(coalton-release-p)` will return `nil`.

As the names imply, these two modes have different behaviors that optimize for either development or release.

**WARNING**: *All* Coalton code, *including* the compiler and the standard library, *must*
be compiled in the same mode.

The default is development mode. If you want to run Coalton in release mode, you must set
the appropriate environment variable before compiling and loading Coalton, and you must
ensure that ASDF is not loading cached fasls compiled for development mode.

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

Unfortunately, having two modes may mean that a user might inadvertently depend on the behavior of one mode that is not supported in the other. We advise testing your code against both modes.

### Developing the compiler

In release mode, the Coalton compiler will `declaim freeze-type` on its internal data
structures. Altering one of these structures in release mode will likely require you to
restart your Lisp process and reload Coalton.

## Promises of Data and Basic Data Types

**PROMISE**: Every Coalton value exists as a Lisp value. The structure of said value may not always be defined, however, and must be considered an element of type `T` in general circumstances.

**PROMISE**: Coalton `Integer`, `Character`, `String`,  `Single-Float`, and `Double-Float` values correspond to their Lisp counterparts.

**PROMISE**: `COALTON:Boolean` is a Lisp `BOOLEAN`; `COALTON:True` is Lisp `T`, and `COALTON:False` is Lisp `NIL`.

**PROMISE**: Every Coalton `List` is a non-circular, homogeneous Common Lisp list.

**WARNING**: Not every Common Lisp list is a Coalton `List`! It must be homogeneous, and the element values must also be valid Coalton objects. (For instance, a Common Lisp list of strings is a valid Coalton list.)

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

### Simple enumerations with `(REPR :ENUM)`

Types that are a just a disjoint union of nullary constructors can be represented internally as Lisp symbols by using `(REPR :ENUM)`. This can be used for both Lisp interoperation matters as well as certain efficiency matters.

For example, we could represent the class of plane traveler like so:

```
(repr :enum)
(define-type TravelerClass
  Economy
  Business
  First)
```

These will be compiled into symbols:

```
COALTON-USER> (coalton (make-list Economy Business First))
(TRAVELERCLASS/ECONOMY TRAVELERCLASS/BUSINESS TRAVELERCLASS/FIRST)
COALTON-USER> cl::(mapcar #'type-of *)
(COMMON-LISP:SYMBOL COMMON-LISP:SYMBOL COMMON-LISP:SYMBOL)
```

### Wrapper types with `(REPR :TRANSPARENT)`

Types with a single construtor consisting of a single field can be annotated with `(REPR :TRANSPARENT)`. This guarentees the wrapper type does not exist at runtime. The constructor function will still be generated, but it will be the identity function.

For example, we could represent an 8-bit gray value like so:

```
(repr :transparent)
(define-type Gray (Gray U8))
```

At runtime, a `Gray` value would just be a single byte.

### Passing Lisp types through Coalton with `(REPR :NATIVE)`

Coalton types can be unboxed lisp types under the hood with `(REPR :NATIVE <type>)`, where `<type>` is a Common Lisp type.

For example, we could wrap Common Lisp's `cl:random-state` objects like so:

```
(repr :native cl:random-state)
(define-type RandomState)

(declare make-state (Unit -> RandomState))
(define (make-state)
  (lisp RandomState ()
    (cl:make-random-state cl:t)))

(declare random (RandomState -> Integer -> Integer))
(define (random rs n)
  (lisp Integer (rs n)
    (cl:random n rs)))
```

Here, we used `lisp` to actually construct, type, and return our `RandomState` object, since our `define-type` doesn't itself have constructors.

See [`Vector`](https://github.com/coalton-lang/coalton/blob/main/library/vector.lisp) for a more extensive example.

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

## Soundness of Coalton-Lisp Interop

As with almost any "foreign function interface" (especially those interfacing with C), there's lots of potential for type errors to be made that Coalton simply cannot check for at compile time. We recommend being extra cautious and being liberal with `check-type`s and other assertions when putting a value into the hands of Coalton-managed code.

Always be vigilant about _all_ possible return types of Lisp functions. Lisp, being a dynamically typed language, is sometimes lax about the possible return types, especially as they relate to the input type. Things like numerical contagion, `nil`, and error conditions can be serious sources of type errors.

## Recipes for calling Coalton functions from inside a Coalton-Calls-Lisp Bridge

The simplest example is calling a function without type class constraints. This allows you to call it directly from lisp:
```
(coalton-toplevel

  (declare int-square (Integer -> Integer))
  (define (int-square x)
    "Returns the square of an Integer."
    (* x x))

  (define (print-int-square x)
    (lisp String (x)
      (cl:format cl:nil "~a squared = ~a" x (int-square x)))))
```
Functions with type class constraints cannot be called this way, but instead must be called with a nested lisp form containing the argument:
```
(coalton-toplevel

  (declare num-square ((Num :a) => :a -> :a))
  (define (num-square x)
    "Returns the square of an object of any type with a defined instance of num."
    (* x x))

  (define (print-num-square1 x)
    (lisp String (x)
      (cl:format cl:nil "~a squared = ~a" x (coalton (num-square (lisp :a () x)))))))
```
If you have a function with no input, Coalton declares it as a function taking `Unit` as input. Thus, it must be wrapped in a Coalton form:
```
(coalton-toplevel

  (declare spell-2 (Unit -> String))
  (define (spell-2)
    "two")

  (define (how-do-you-spell-2)
    (lisp String ()
      (cl:format cl:nil "2 is spelled '~a'" (coalton (spell-2))))))
```
Lambda/anonymous functions in Coalton `let` bindings can be used from Lisp, as long they are unconstrained by a type class:
```
(coalton-toplevel
  (define (lambda-print-square x)
    (let ((declare square (Integer -> Integer))
          (square (fn (a)
                    (* a a))))
      (lisp :a (x square)
        (cl:format cl:nil "~a squared = ~a" x (call-coalton-function square x))))))
```



