# Intro to Coalton

Coalton is a statically typed language that is embedded in, and compiles to, Common Lisp.

This document is aimed toward people who are already familiar with functional programming languages. If you are already familiar with Common Lisp, the [glossary](./glossary.md) may be useful.

## Systems

### Explicit Component Definition

Coalton uses ordinary Common Lisp packages (and ASDF systems) to organize code. If you're starting a new project, add `#:coalton` to your ASD's `:depends-on` list. For improved error messages, also depend on `#:named-readtables`.

### Package Inferred System

For simple projects, you can use the `package-inferred-system` utility to automatically create an ASDF system based on your package structure. However, this project, coalton, does not use these utilities. By this incompatibility, the ASDF loader skips loading `#:coalton-prelude` when using this utility on your own project. Hence, you must explicitly register `coalton-prelude` in your own ASDF system definition before specifying `#:coalton` in your `:depends-on` list.

```lisp
;; In your ASDF system definition; e.g., my-project.asd
(register-system-packages '#:coalton '(#:coalton-prelude))
```

## Packages

Unlike Common Lisp, Coalton's standard library is organized as a large collection of packages. For example, string-related functions are in the `#:coalton/string` package. Refer to the [Coalton Reference](https://coalton-lang.github.io/reference) for a complete list of standard library packages.

When making a new project, you will want to define a new package and establish your standard library imports. *Do not `:use` the `#:cl` or `#:common-lisp` packages!* Instead, you'll want to, at minimum, `:use` both the `#:coalton` package (for core language features) and `#:coalton-prelude` (for extremely common standard library definitions):

```lisp
(defpackage #:my-package
  (:use 
   #:coalton 
   #:coalton-prelude))
```

The `#:coalton-prelude` package does not contain any functionality that isn't also found elsewhere in the standard library, and hence, it is a convenience.

In idiomatic usage, this minimal package definition would be far too limiting to be useful. For instance, if we wanted to use the standard library function `strip-prefix` (which strips a prefix off of a string), we would have to type `coalton/string:strip-prefix` in our program. Instead, we can create a local nickname for this package:

```lisp
(defpackage #:my-package
  (:use 
   #:coalton)
  (:local-nicknames 
   (#:str #:coalton/string)))
```

This way, we can now just type `str:strip-prefix`. Any time we make use of a new function, either from the Coalton standard library or from a third-party library, we might add nicknames that function's package to our `:local-nicknames` list. For example:

```lisp
(defpackage #:my-package
  (:use 
   #:coalton)
  (:local-nicknames 
   (#:str  #:coalton/string)
   (#:vec  #:coalton/vector)
   (#:math #:coalton/math)))
```

**We do not recommend `:use`ing any other Coalton built-in packages besides `#:coalton` and `#:coalton-prelude`.** Historically, in Common Lisp, this has caused backwards-compatibility issues when the packages themselves introduce new exported symbols. Moreover, unlike Common Lisp, Coalton follows a pattern of using the same symbol name for similarly defined functions of different packages. For example, both the string package and the vector package have a symbol named `length`, i.e., `str:length` and `vec:length` both exist.

### Coalton in the REPL

If you're playing around with Coalton in the REPL, we recommend changing your package to `#:coalton-user` like so:

```lisp
(in-package #:coalton-user)
```

This is a convenience package that is defined purely for your interactive and experimental use. (No software should ever depend on it, just like `#:cl-user`.)

As we shall see in the next section, just because you change to the `#:coalton-user` package doesn't mean you can immediately start typing Coalton code; you will still need to use `coalton-toplevel` or `coalton` forms.

Since `#:coalton-user` does not `:use` the `#:common-lisp` package, you will need to qualify any such symbols with `cl:`.

## Program Structure

After creating your package `#:my-package`, you must switch to it with:

```lisp
(in-package #:my-package)

(named-readtables:in-readtable coalton:coalton)
```

The `named-readtables:in-readtable` form is optional but encouraged. Using Coalton's reader allows compiler errors to accurately refer to source code and provide correct line numbers.

The first primary entry points for Coalton code. Definitions and the like sit in a toplevel-form called `coalton-toplevel`.

```lisp
(coalton-toplevel
  ;; <Coalton definition forms>
  )
```

Currently, in SLIME/SLY, there's no way to `C-c-c` in any finer-grained way than a whole `coalton-toplevel` form.

The second primary entry point is calling Coalton from Lisp. In this case, one uses the `coalton` operator:

```lisp
;; Lisp code
;; ...
     (coalton #|coalton expression|#)
;; ...
```

Note that one _cannot_ make new definitions in a `coalton` form, only evaluate expressions.

Whereas `coalton-toplevel` expects one or more toplevel definitions or declarations, the `coalton` form takes a single expression, evaluates it relative to the current environment, and returns its (underlying) Lisp value. This can be useful for working with Coalton from a Lisp REPL.

Remember that Coalton packages, including `#:coalton-user`, do *not* `:use` the `#:common-lisp`/`#:cl` package, so you must prepend Common Lisp symbols with `cl:` if you need them.


## Variables and Functions

Variables and functions are defined with `define`. Here are some variable definitions.

```lisp
(coalton-toplevel
  ;; Variables are defined with the define keyword
  (define x 5)
  (define y 6)
  (define z (+ x y))
  (define p (Tuple 1.0 2.0))

  ;; Coalton supports integers, strings, booleans, and unit as primitive types
  (define name "Alyssa P. Hacker")
  (define hungry True)
  (define data Unit))
```

One can get the first element of the tuple `p` defined above from the REPL using the `coalton` operator:
```lisp
(coalton (fst p))
```

**Note**: It may be tempting to elide the `coalton` form and simply evaluate `(fst p)` directly in a Lisp REPL, but such behavior should not be relied upon!

Functions are defined similarly to variables. Unlike Common Lisp, Coalton functions occupy the same namespace as variables. This makes high-order functional programming easier.

```lisp
(coalton-toplevel
  ;; Functions are also defined with the define keyword
  (define (add2 x)
    (+ 2 x))

  ;; Functions exist in the same namespace as variables
  (define addTwo add2)

  (define x (addTwo 3))

  ;; Anonymous functions can be defined with fn
  (define z (map (fn (x) (+ 2 x)) (make-list 1 2 3 4))))
```


### Functions and Currying

*All* functions in Coalton take *exactly* one input, and produce *exactly* one output. Consider this function:

```lisp
(coalton-toplevel
  (define (fma a b c)
    (+ c (* a b))))
```

Truth be known, `fma` actually technically takes *one argument*: `a`. To a Common Lisper, this function is roughly equivalent to

```lisp
(defun fma (a)
  (lambda (b)
    (lambda (c)
      (+ c (* a b)))))
```

However, Coalton hides this reality from you unless you need it:

```lisp
(coalton-toplevel
  (define fma1 (fma 2))      ; equiv: b -> (c -> (c + 2*b))
  (define fma2 (fma 2 3))    ; equiv: c -> (c + 6)
  (define fma3 (fma 2 3 4))) ; equiv: 10
```

We can see that we can call `fma` as if it's a three argument function, but that's merely convenient syntax. We can also call it with fewer arguments. Sometimes this property is called *curried functions*.

Coalton does work to optimize these functions to reduce as much closure allocation as possible. In fact, `(fma x y z)` will get compiled as a simple add and multiply, without any closure allocations.

Here is an example of using a curried function to transform a list.

```lisp
(coalton-toplevel
  ;; Lists can be created with the make-list macro
  (define nums (make-list 2 3 4 5)))

(coalton
  ;; Functions in Coalton are curried
  (map (+ 2) nums)) ;; 4 5 6 7
```

### Pipelining Syntax and Function Composition

There are convenient syntaxes for composing functions with the `pipe` and `nest` macros.

```lisp
(nest f g ... h x)

;; is equivalent to

(f (g (... (h x))))

;; is equivalent to

(pipe x h ... g f)
```

These are useful to make code less noisy.

Note that since these are macros (indicated by their variadic arguments), they *cannot* be used as higher-order functions. Consider either currying or the `compose` function if you're thinking in that direction.

### Ignoring Function Parameters

Consider the function

```lisp
(coalton-toplevel
  (define (f x y)
    x))
```

Here, `y` is unused and will produce a warning:

```
COMMON-LISP:WARNING: warn: Unused variable
  --> <unknown>:3:15
   |
 3 |    (define (f x y)
   |                 ^ variable defined here
help: prefix the variable with '_' to declare it unused
 3 |   (define (f x _y)
   |                --
```

As suggested, one can replace `y` with `_y`, which tells the Coalton compiler that the parameter might be intentionally unused.

**Note**: Variables prefixed with `_` like `_y` are still normal variables, and can be read. The following is valid Coalton:

```lisp
(define (f _x) _x)
```

One should treat underscore prefixed variables as ignored whenever possible, and use a name not prefixed with `_` if it may be used. Reading from underscore-prefixed variables is permitted so that generated code (e.g., using macros or read-conditionals) may avoid unused variable warnings for variables which will be used in some compilation contexts but not others.

## Polymorphism, Mutation, and the Value Restriction

Coalton infers polymorphic types for implicitly typed `define` and `let` bindings using a syntactic ML-style value restriction plus a relaxed variance check. This ensures the Coalton type system is sound in the presence of mutable data structures.

Informally:

- A **non-expansive** expression is syntactically value-like (for example: variables, literals, lambdas, and constructor applications over non-expansive arguments).
- An **expansive** expression is everything else. In particular, ordinary function application is treated as expansive, even when the call is observationally pure.
- Expansive bindings introduce **weak** type-variable candidates. Weak variables are generalized only when all observed occurrences are **covariant** and they do not occur in retained predicates (constraints).

This still allows many covariant expansive expressions to remain polymorphic.

At top level, unresolved weak variables are rejected with an error.

A non-expansive constructor expression can still generalize:

```lisp
(coalton-toplevel
  (define wrapped-none (Some None)))
;; wrapped-none : Optional (Optional :a)
```

An expansive expression can still generalize when the weak variable is only covariant:

```lisp
(coalton-toplevel
  (define wrapped-id
    ((fn (x) x) None)))
;; wrapped-id : (Optional :a)
```

Even though this example is an ordinary function application (and thus expansive), `:a` appears covariantly, so it can still be generalized.

But expansive bindings involving invariant or contravariant occurrences (or retained constraints on weak variables) do not generalize. For example, mutable containers such as `Vector` remain monomorphic:

```lisp
(coalton-toplevel
  (define wrapped-new
    (Some (coalton/vector:new))))
;; error
```

If you hit this, there are two common fixes:

1. Move the allocation into a function body so each call gets fresh state (often via eta-expansion).
2. Add an explicit top-level type declaration when you intentionally want a monomorphic mutable value.

For example, this is intentionally monomorphic:

```lisp
(coalton-toplevel
  (declare int-vec (Vector Integer))
  (define int-vec (coalton/vector:new)))
```

Coalton does not track mutability as an intrinsic quality of every type constructor. For opaque parametric type definitions, variance is assumed to be invariant by default.

References:

- OCaml manual: Polymorphism and its limitations.
- Jacques Garrigue, "Relaxing the Value Restriction".
- Andrew K. Wright, "Simple Imperative Polymorphism".

### Variance

Coalton uses type-parameter variance to decide which weak type variables from expansive bindings can still be generalized.

- **Covariant**: the type parameter is only produced (for example, `Optional :a`, `List :a`).
- **Contravariant**: the type parameter is consumed (for example, function argument position in `:a -> :b`).
- **Invariant**: both directions are possible, or mutability allows writes (common for mutable containers).

Under the relaxed value restriction, weak variables are generalized only when all observed occurrences are covariant; any invariant or contravariant occurrence keeps them monomorphic.

### Opaque Native Types

For `define-type` forms with no constructors and no alias body (typically used with `repr :native`), Coalton cannot inspect the type's internal structure to infer variance from fields. In those cases, type parameters are treated as invariant by default.

This is conservative by design. For mutable wrappers, invariance is usually required for soundness.

Current parametric opaque stdlib types include:

- `Vector :a`
- `Cell :a`
- `Queue :a`
- `Slice :a`
- `Hashtable :key :value`
- `LispArray :t`
- `FileStream :a`

These are all modeled invariantly; each either exposes mutation directly or mixes read/write capabilities in one type.

## Data Types

Coalton allows the definition of parametric algebraic data types.

```lisp
(coalton-toplevel
  ;; New types are created with the DEFINE-TYPE operator
  (define-type Point3D (Point3D Integer Integer Integer))

  ;; Coalton supports sum types
  (define-type Color
    Red
    Blue
    Green)

  ;; Coalton supports generic type variables
  ;;
  ;; Type parameters are defined using keyword arguments
  (define-type (Tree :a)
    (Branch (Tree :a) :a (Tree :a))
    (Leaf :a)))
```

Type definitions introduce type constructors. For example, we may construct a somewhat festive tree as follows.

```lisp
(coalton
  (Branch (Leaf Red)
          Green
          (Branch (Leaf Red) Green (Leaf Red))))
```

We'll see how to unpack these types using `match` later in this document.

## Type Aliases

Coalton allows the definition of parametric type aliases. Type aliases can be defined on primitive types and types created with `define-type` or `define-type-alias`.

```lisp
(coalton-toplevel
  ;; New type aliases are created with the DEFINE-TYPE-ALIAS operator
  (define-type-alias Coordinate Integer)
  (define-type-alias (Pair :a) (Tuple :a :a))
  (define-type-alias Translation (Pair Coordinate -> Pair Coordinate))
  
  (declare shift-right Translation)
  (define (shift-right (Tuple x y))
    (Tuple (1+ x) y))
    
  (define shifted-coordinate (shift-right (Tuple 0 0))))

  ;; Type aliases can have multiple parameters
  (define-type-alias (MyTuple3 :a :b :c) (Tuple :a (Tuple :b :c)))

  ;; Type aliases can have parameters that do not have a kind of *
  (define-type-alias (IntegerCollection :col) (:col Integer))

  ;; Type aliases can alias types that do not have a kind of *
  (define-type-alias MyCollection List)
```

Parametric type aliases must be fully applied.
```lisp
(coalton-toplevel

  (define-type (T :a) (ConstrT (:a Integer)))
  
  (define-type-alias (MyCollection1 :a) (List :a))
  (define-type-alias MyCollection2 List)

  ;; This line will not compile, because MyCollection1 has a
  ;; parameter :A which is not applied
  (define-type-alias A (T MyCollection1))

  ;; However, this line will compile
  (define-type-alias A (T MyCollection2)))
```

There are several debugging tools which are useful when working with type aliases. Outside of a Coalton expression, `describe-type-of` can be used to display the type of a symbol, including its aliases, and to return the type. `describe-type-alias` displays the alias along with the aliased type and returns the aliased type. Additionally, Coalton can be configured to display only aliases, only types, or both, when displaying the type associated with a symbol. The preference can be set before compiling Coalton using `(setf (get ':coalton-config ':type-printing-mode) mode)` where `mode` is one of `:types`, `:aliases`, and `:types-and-aliases`. Thereafter, the mode can be changed among those three options using the function `set-type-printing-mode`. The default mode is `:types`.

```lisp
COALTON-USER> (coalton-toplevel
                (define-type-alias A Integer)
                (define x (the A 5)))
; No value

COALTON-USER> (set-type-printing-mode :aliases)
:ALIASES

COALTON-USER> (type-of 'x)
A

COALTON-USER> (set-type-printing-mode :types-and-aliases)
:TYPES-AND-ALIASES

COALTON-USER> (type-of 'x)
[A := INTEGER]

COALTON-USER> (set-type-printing-mode :types)
:TYPES

COALTON-USER> shifted-coordinate ;; from the example above
#.(TUPLE 1 0)

COALTON-USER> (type-of 'shifted-coordinate)
(TUPLE INTEGER INTEGER)

COALTON-USER> (describe-type-of 'shifted-coordinate)
[(PAIR COORDINATE) := (TUPLE [COORDINATE := INTEGER] [COORDINATE := INTEGER])]

COALTON-USER> (describe-type-alias 'Pair)
[(PAIR :A) := (TUPLE :A :A)]
```

### Structs

There is also support for defining structs.

```lisp
(coalton-toplevel
  (define-struct Point
    (x Integer)
    (y Integer)))
```

Structs are like single constructor ADTs, and are constructed equivalently:

```lisp
(coalton (Point 1 2))
```

Field accessors can be used to read individual fields:

```lisp
(coalton (.x (Point 1 2)))
```

Field accessors can be passed by value:

```lisp
(coalton (map .x (make-list (Point 1 2) (Point 2 3))))
```

Structs can also be parametric:

```lisp
(coalton-toplevel
  (define-struct (Pair :a)
    (first :a)
    (second :a)))
```

## Looping & Iteration

Coalton offers three main ways to do explicit loops:

- Tail-recursion and `rec`,
- Built-in iterator-based looping constructs, and
- Experimental Lisp-like iteration macros (e.g., `dotimes`).

### Tail-Recursion and `rec`

Coalton strives to ensure tail calls are always eliminated. As such, tail recursion is idiomatic in Coalton.

```lisp
(coalton-toplevel
  (define (find-integer predicate limit)
    (cond
      ((negative? limit) False)
      ((predicate limit) True)
      (True              (find-integer predicate (1- limit))))))
```

This function can be run for arbitrarily large `limit` without affecting the stack:

```lisp
> (coalton (find-integer (fn (x) (== 65536 (* x x))) 1000000))
COMMON-LISP:T
```

Coalton has a special built-in operator `rec` for doing "named-let"-style iteration. It's general syntax is:

```lisp
(rec <name> (<binding>*)
  <body>)

;; <name> := <symbol>
;;         | (<symbol> <type>)
;;
;; <binding> := (<symbol> <value>)
```

The `<name>` is in essence a local (tail-recursive) function in the `<body>`. Optionally, the type of this function can be declared, which is useful to avoid unwanted polymorphism. The `rec` operator does not require the invocation to be in tail position.

It is idiomatic in Coalton to choose the name `%` when there is no other reasonable name.

Here is a first example of `rec` to implement a list reversal.

```lisp
(coalton-toplevel
  (define (rev l)
    (rec % ((result Nil) (remaining l))
      (match remaining
        ((Nil) result)
        ((Cons x xs) (% (Cons x result) xs))))))
```

Here is an example of a Fibonacci function that works for any numeric input and output:

```lisp
(coalton-toplevel
  (define (fib-polymorphic n)
    (rec % ((i n) (a 0) (b 1))
      (cond
        ((== 0 i) a)
        ((== 1 i) b)
        (True (% (- i 1) b (+ a b)))))))
```

The type is `∀ A B. (NUM B) (NUM A) ⇒ (A → B)` and thus can be used for whatever numeric types:

```
> (coalton (the F32 (fib-polymorphic (the Integer 10))))
55.0
```

We can make a monomorphic variant for `Integer` by declaring the type of the recursion.

```lisp
(define (fib-monomorphic n)
  (rec (% (Integer -> Integer -> Integer -> Integer))
       ((i n)
        (a 0)
        (b 1))
    (cond
      ((== 0 i) a)
      ((== 1 i) b)
      (True (% (- i 1) b (+ a b))))))
```

Now it works without any type declarations on use:

```
> (coalton (fib-monomorphic 10))
55
```

### Built-In Looping Constructs

Coalton supports infinite looping, conditional looping, and `for`-loop styled iteration. 

#### `loop`, `while`, `while-let`, and `for`

You can loop forever 

```lisp
(loop (trace "hi"))
```

You can loop while some condition is true

```lisp
(coalton
 (let ((counter (cell:new 0))
       (limit 10))
   (while (< (cell:read counter) limit)
     (trace "hi") 
     (cell:increment! counter))))
```

You can loop so long as a pattern matches 

```lisp
(coalton
 (let ((xs (vector:make 4 3 2 1)))
   (while-let (Some x) = (vector:pop! xs)
              (traceobject "x" x))))
```

You can loop over instances of `IntoIterator`

```lisp
(coalton
 (for x in "coalton"
      (traceobject "x" x)))
```


#### `break` and `continue`

Each of the above looping forms supports `break` and `continue`.

The `break` form immediately terminates iteration.  The following
prints out `c`, `o`, and `a` and then terminates.

```lisp
(coalton
 (for x in "coalton"
      (when (== x #\l)
        (break))
      (traceobject "x" x)))
```

The `continue` form skips the remainder of the loop's body and starts
on its next iteration. The following prints out `c`, `o`, `a`, `t`,
`o`, and `n`, having skipped the printing of `l`.

```lisp
(coalton
 (for x in "coalton"
      (when (== x #\l)
        (continue))
      (traceobject "x" x)))
```


#### Loop Labels

Each of the above looping forms takes an optional loop label
keyword. These labels can be used in conjunction with `break` and
`continue` to achieve complex control flow.

For each of the looping forms, a label may immediately follow the
opening term of the loop:

```lisp 

(loop :outer (do-stuff))

(while :a-label (is-true?) (do-stuff))

(while-let :another-label 
   (Some thing) = (get-something)
   (do-stuff thing))

(for :iter word in words 
   (do-stuff-with word))

```

In the following entirely artificial example, the outermost loop is
labeled `:outer`. This label is passed to `break` from inside the
inner `while` loop to terminate iteration whenever the sum of the
accumulator and the counter exceeds 500.  Without the `:outer` label,
`break` would have only broken out of the inner `while` loop.

```lisp
(coalton 
  (let ((counter (cell:new 0))
        (acc (cell:new Nil)))
    (loop :outer
          (while (< (cell:increment! counter) 10)
            (let x = (fold + (cell:read counter) (cell:read acc)))
            (when (< 500 x)
              (break :outer))
            (when (== 0 (mod (cell:read counter) 3)) 
              (continue))
            (cell:push! acc x))
          (when (< (length (cell:read acc)) 500)
            (cell:swap! counter 0)
            Unit))
    (cell:read acc)))
```

### Experimental Lisp Iteration Macros

> [!WARNING]
> These constructs are experimental and may change.

The standard library package [`coalton/experimental/loops`](https://coalton-lang.github.io/reference/#coalton/experimental/loops-package) contains a plethora of Lisp-style iteration macros, including `dotimes` and `dolist` which operate similar to their Common Lisp counterparts.

The functions generally return `Unit` unless they're specifically collecting or accumulating a value. Counts (like in `dotimes`) and indices (like in `dolist-enumerated`) are generally `UFix`.

Assuming we have `dotimes` accessible from our package, we might write a function to print out Pythagorean triples in the following way:

```lisp
(coalton-toplevel
  (define (print-pythagorean-triples limit)
    (dotimes (c limit)
      (dotimes (b c)
        (dotimes (a b)
          (when (== (^ c 2) (+ (^ a 2) (^ b 2)))
            (trace (Tuple3 a b c))))))))

;; > (coalton (print-pythagorean-triples 20))
;; #.(TUPLE3 3 4 5)
;; #.(TUPLE3 6 8 10)
;; #.(TUPLE3 5 12 13)
;; #.(TUPLE3 9 12 15)
;; #.(TUPLE3 8 15 17)
```

## Numbers

Coalton supports a few numeric types. The main ones are `Integer`, `F32`, and `F64`.

```lisp
(coalton-toplevel
  (define num-int 5)
  (define num-sf  5.0f0)
  (define num-df  5.0d0))
```

One can leave off the suffix and just write `5.0`, which will be resolved
depending on the read-time value of `cl:*read-default-float-format*`. That
variable is set to `cl:single-float` by default, meaning unadorned floats will
be single-precision.

There are also other, more restricted integer types, like fixed-width
signed types (`I32` and `I64`) and fixed-width unsigned types (`U8`,
`U32`, `U64`).

Lastly, there's a ratio type called `Fraction`, which is a ratio of two `Integer` values.

Numbers implement the `Num` type class, which has methods `+`, `-`, `*`, and `fromInt`.

### Division, in short

Division is complicated; see the next section. But here are some quick tips.

- The division operator `/` and its variants can produce run-time errors if the divisor is zero. Use `safe/` if you prefer an `Optional` return type.

- If you have integers and you want a double-float, use `inexact/`. (For a single-float, use `into`.)

- If you have integers and you want an integer answer, use `floor/`, `ceiling/`, or `round/`.

- If you're trying to divide two integers to get an exact rational answer, use `exact/`.

- If you're writing generic code that uses division, or you want to remain abstract, learn `/` and how to constrain it properly.

### Details about Division

Why doesn't the `Num` type class have division, i.e., `/`?

Coalton does have a division operator `/`, but it's a separate, slightly more difficult concept. Division is tricky for two reasons:

1. Division can fail if we divide by something like zero,

2. Dividing two numbers doesn't necessarily result in the same type. (In fact, division may not even be possible!)

To address the first concern, division may result in a run-time error. We don't use `Optional` because it is quite cumbersome to use in mathematical contexts. (One may use `safe/` for a variant that does a division-by-zero check and produces an `Optional`.)

To address the second concern, we need to introduce a new type class called `Dividable`. The type expression

```
(Dividable :s :t)
```

says that division of two items of type `:s` may result in an item of type `:t`. With all of this, we have the final type of `/`.

```
COALTON-USER> (type-of '/)
∀ :A :B. DIVIDABLE :A :B ⇒ (:A → :A → :B)
```

Because of [Instance Defaulting](#instance-defaulting), division of `Integer` constants without any additional context defaults to `F64` division:

```
COALTON-USER> (coalton (/ 1 2))
0.5d0
```

We can inform Coalton that our constants are of another type by constraining them with `the` or relying on type inference. For example, in order to get a non-F64 result from `Integer` inputs, you have to constrain the result type to your desired type (as long as the type has a defined instance of the `Dividable` type class):

```
COALTON-USER> (coalton (the F32 (/ 4 2)))
2.0
COALTON-USER> (coalton (the Fraction (/ 4 2)))
#.(COALTON::%FRACTION 2 1)
```

An `Integer` result from division with `/` is not possible, as the instance `Dividable Integer Integer` is not defined:

```
COALTON-USER> (coalton (the Integer (/ 4 2)))
; error: Unable to codegen
;   --> repl input:1:22
;    |
;  1 |  (COALTON (THE INTEGER (/ 4 2)))
;    |                        ^^^^^^^ expression has type ∀. (RECIPROCABLE INTEGER) => INTEGER with unresolved constraint (RECIPROCABLE INTEGER)
;    |                        ------- Add a type assertion with THE to resolve ambiguity
;    [Condition of type COALTON-IMPL/TYPECHECKER/BASE:TC-ERROR]
```

Why shouldn't this just be `2`?! The unfortunate answer is because `/` might not *always* produce an integer `2`, and when it doesn't divide exactly, Coalton doesn't force a particular way of rounding. As such, the proper way to do it is divide exactly, then round as you please with `floor`, `ceiling`, or `round`.

```
COALTON-USER> (coalton (floor (the Fraction (/ 4 2))))
2
```

You can see what happens when you choose values that don't divide evenly.

```
COALTON-USER> (coalton (floor (the Fraction (/ 3 2))))
1
COALTON-USER> (coalton (ceiling (the Fraction (/ 3 2))))
2
COALTON-USER> (coalton (round (the Fraction (/ 3 2))))
2
```

All of these cases are sufficiently common that we provide a few shorthands:

- `safe/` to do a division-by-zero check and produce `None` if so,

- `exact/` for integer-to-fraction division (which replaces all of the `the Fraction` business above),

- `inexact/` for integer-to-double division,

- `floor/`, `ceiling/`, and `round/` for integer-to-integer division, and

Fractions can be converted to other dividable types using `fromfrac` (Note: This may result in precision loss):

```
COALTON/MATH/REAL> (coalton (the F64 (fromfrac 1/2)))
0.5d0
COALTON/MATH/REAL> (coalton (the F32 (fromfrac 999/1000)))
0.999
```


## Lists

Coalton uses Lisp lists under the hood. Lists can be constructed with `make-list`.

```lisp
(coalton-toplevel
  (define x (make-list 1 2 3))
  (define y (make-list "a" "b" "c")))
```


Lists must be homogeneous. This means the following produces a type error.

```
COALTON-USER> (coalton-toplevel
                (define wut (make-list 1.0d0 2.0d0 3.0)))
; error: Type mismatch
;   --> repl input:3:4
;    |
;  3 |      (MAKE-LIST 1.0d0 2.0d0 3.0)))
;    |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^ Expected type '(LIST DOUBLE-FLOAT)' but got '(LIST SINGLE-FLOAT)' 
;    [Condition of type COALTON-IMPL/TYPECHECKER/BASE:TC-ERROR]
```

Lists can also be deconstructed with `match`.

```lisp
(coalton-toplevel
  (define (is-empty lst)
    (match lst
      ((Cons _ _) "is not empty")
      ((Nil) "is empty"))))
```

## Static Typing

Coalton code is statically type checked. Types are inferred.

```lisp
(coalton-toplevel
  (define (fun x)
    (map (+ 2) (str:parse-int x))))
```

The type of a variable or function can be checked with `coalton:type-of`.

```
COALTON-USER> (type-of 'fun)
(STRING -> (OPTIONAL INTEGER)
```

Type declarations can always be added manually.

```lisp
(coalton-toplevel
  (declare fun (String -> (Optional Integer)))
  (define (fun x)
    (map (+ 2) (str:parse-int x))))
```

Type declarations can also be added in `let` expressions

```lisp
(coalton-toplevel
  (define (f a b)
    (let ((declare g (Integer -> Integer -> Integer))
          (g +))
      (g a b))))
```

### Type Casting, Coercing, and Conversion

Coalton manages type conversions similar to the Common Lisp function `cl:coerce` by way of a type class called `Into` (of the package `#:coalton/classes`) and its sole method `into`. However, the `into` method only takes a single argument. How should Coalton know which data type to convert to? It determines this either by type inference (i.e., by the surrounding context) as in this example, where `substring` expects a `String`:

```lisp
(coalton-toplevel
  (define integer-part (substring (into 12.34d0) 0 2)))

;; ==> "12"
```

or by an explicit declaration with `the`:

```lisp
(coalton-toplevel
  (define unique-letters
    (remove-duplicates (the (List Char) (into "mississippi")))))

;; ==> (#\m #\s #\p #\i)
```

The `the`-`into` pattern is so common that Coalton provides a shorthand called `as`. The above example can be written more succinctly as:

```lisp
(coalton-toplevel
  (define unique-letters
    (remove-duplicates (as (List Char) "mississippi"))))

;; ==> (#\m #\s #\p #\i)
```

The `into` method is used only when a conversion can always be performed from one type to another. If not values of a type can be converted, then another type class `TryInto` with a method `tryInto` is used. The `tryinto` method returns a `Result` type which indicates whether the conversion was successful or not. 

**Note that `as` only works for conversions via `into`, i.e., conversions that are total.** There is no corresponding syntax for `tryInto`.

## Pattern Matching

`match` expressions can be used to pattern-match and deconstruct algebraic data types.

```lisp
(coalton-toplevel
  (define-type Color
    Red
    Blue
    Green)

  ;; Constructors must be wrapped in parentheses
  (declare color-to-string (Color -> String))
  (define (color-to-string c)
    (match c
      ((Red) "Red")
      ((Blue) "Blue")
      ((Green) "Green")))

  ;; Variables are not wrapped in parentheses
  (declare map-optional ((:a -> :b) -> (Optional :a) -> (Optional :b)))
  (define (map-optional f x)
    (match x
      ((Some x_) (Some (f x_)))
      ((None) None)))

  ;; Patterns can be nested, and wildcard "_" patterns are supported
  (declare flatten-optional ((Optional (Optional :a)) -> (Optional :a)))
  (define (flatten-optional x)
    (match x
      ((Some (Some x_)) (Some x_))
      (_ None)))

  ;; Submatches can be captured in a variable
  (declare dedup-head (Eq :a => List :a -> List :a))
  (define (dedup-head xs)
    "If the first and second member of list are equal, drop the first"
    (match xs
      ((Cons a (= tl1 (Cons b _))) 
       (if (== a b) tl1 xs))
      (_ xs)))

  ;; Integers or Strings can also be matched on
  (define (is-5-or-7 x)
    (match x
      (5 True)
      (7 True)
      (_ False)))
      
  (define (is-five-or-seven x)
    (match x
      ("five"  True)
      ("seven" True)
      (_       False))))
    
```

Functions can pattern match on their arguments, but the patterns must be exhaustive.

```lisp
(coalton-toplevel
  (declare first (Tuple :a :b -> :a))
  (define (first (Tuple a _)) a)

  (declare second (Tuple :a :b -> :b))
  (define second (fn ((Tuple _ b)) b))

  ;; pattern capture works here too
  (declare nest-right (Tuple :a :b -> (Tuple :a (Tuple :a :b))))
  (define (nest-right (= tpl (Tuple a _))) (Tuple a tpl)))

```

The operator `coalton:if` can be used as a shorthand when matching on Booleans:

```lisp
(coalton-toplevel
  (define (is-even x)
    (if (== 0 x)
        True
        (is-odd (- x 1))))

  (define (is-odd x)
    (if (== 0 x)
        False
        (is-even (- x 1)))))
```

Several `if` expressions can be combined with a `cond`:

```lisp
(coalton-toplevel
  (define (fizz-buzz n)
    (cond
      ((and (== 0 (mod n 5))
            (== 0 (mod n 3)))
            "Fizzbuzz")
      ((== 0 (mod n 3))
        "Fizz")
      ((== 0 (mod n 5))
        "Buzz")
      (True (into n)))))
```

The Boolean operators `and` and `or` (of `coalton`) are actually variadic macros that short-circuit. Their functional counterparts are `boolean-and` and `boolean-or`.

```lisp
(coalton
  (or (cheap 5) True (really-expensive (expt 2 1000000))))
```

In this case, `really-expensive` will never get called due to short-circuiting. Also note that both `and` and `or` can take one or more arguments.


## `COALTON:PROGN`

Coalton has a `coalton:progn` construct similar to lisp.

```lisp
(coalton-toplevel
  (declare f (Integer -> Integer -> (Tuple Integer Integer)))
  (define (f x y)
    (progn
      (+ x y)
      (* x y)
      (Tuple x y))))
```

Coalton's `progn` can have flattened `let` syntax.

```lisp
(coalton-toplevel
 (declare f (Integer -> Integer -> String))
 (define (f x y)
   (progn
     (let x_ = (into x))
     (let y_ = (into y))
     (<> x_ y_))))
```

Flattened `let` expressions support pattern matching:

```lisp
(coalton-toplevel
  (declare f (Tuple Integer Integer -> Integer))
  (define (f t)
    (let (Tuple fst snd) = t)
    (+ fst snd)))

```

Flattened `let` expression are non recursive, and do not support `let` polymorphism. Thus the following is invalid:

```
(coalton
  (progn
    (let id = (fn (x) x))
    (id Unit)
    (id "hello")))
```

It does however work with standard `let` expressions:

```
(coalton
  (let ((id (fn (x) x)))
    (progn
      (id Unit)
      (id "hello"))))
```

Function definitions create an implicit `progn` block

```lisp
(coalton-toplevel
  (declare f (Integer -> Integer -> String))
  (define (f x y)
    (let x_ = (into x))
    (let y_ = (into y))
    (<> x_ y_)))
```

## Unless and When

The `coalton` package also includes `unless` and `when`, which work
similarly to their definitions in Lisp. We recommend only using these operators
for conditionalizing stateful operations.

```lisp
(coalton-toplevel
  (define (f x)
    (when (== x 5)
      (error "I only want the number 5"))))
```

`unless` and `when` both form implicit `progn` blocks.

```lisp
(coalton-toplevel
  (define (f b)
    (when b
      (let x = 5)
      (let y = 7)
      (traceObject "sum" (+ x y)))))
```

## Early Returns

Functions can be returned from early with `return`.

```lisp
(coalton-toplevel
  (define (fizz-buzz n)
    (when (== 0 (mod n 15))
      (return "fizzbuzz"))

    (when (== 0 (mod n 3))
      (return "fizz"))

    (when (== 0 (mod n 5))
      (return "buzz"))

    (into n)))
```

## Type Classes

Coalton supports type classes.

Currently, *all* member functions must be defined for each type class instance.

```lisp
(coalton-toplevel
  ;; Type classes are defined with the define-class keyword
  (define-class (Eq :a)
    (== (:a -> :a -> Boolean)))

  (define-type Color
    Red
    Green
    Blue)

  ;; Type class instances are defined with the define-instance keyword
  (define-instance (Eq Color)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Red) (Red)) True)
        ((Tuple (Blue) (Blue)) True)
        ((Tuple (Green) (Green)) True)
        (_ False)))
    (define (/= a b) (not (== a b))))

  ;; Type declarations can have constraints
  (declare is-eql (Eq :a => (:a -> :a -> String)))
  (define (is-eql a b)
    (if (== a b)
      "They are equal"
      "They are not equal"))

  ;; Multiple constraints must be wrapped in parentheses
  (declare double-is-eql ((Eq :a) (Eq :b) => (:a -> :a -> :b -> :b -> String)))
  (define (double-is-eql a b c d)
    (if (and (== a b) (== c d))
      "Both pairs are equal"
      "The pairs are not both equal")))
```


## Builtin Type Classes

The following are the main type classes defined in the standard library.

* `Eq` - defined on types that are comparable
* `Ord` - defined on types that are orderable
* `Num` - defined on types that are numeric

* `Semigroup` - defined on types which support an associative binary operation
* `Monoid` - defined on types that are semigroups and have an identity element


Each of the following type classes resembles the class of the same name in
Haskell, aside from meager differences.

* `Functor` - `fmap` is just `map` in Coalton
* `Applicative`
* `Monad` - monad does not have `return`, use `pure` from applicative instead
* `Alternative` - `<|>` is called `alt` in Coalton
* `Foldable`
* `Traversable`

These type classes are inspired by traits of the same name in Rust:

* `Into` - total conversions between one type and another
* `TryInto` - non-total conversions between one type and another

## Automatically Deriving Type Class Instances from Type Definitions

Instances of some type classes can be derived so that they are defined automatically for user-defined types.

```lisp
(coalton-toplevel
  (derive Eq Hash)
  (define-struct Point
    (x UFix)
    (y UFix)))
```

Now you can use the `==` and `hash` methods on `Point` structures:

```lisp
(coalton
  ;; Test `Point' equality
  (== (Point 1 2) (Point 3 3))

  ;; Make a map using `Point' as a key
  (let map = (the (hashmap:HashMap Point UFix) hashmap:empty))
  (hashmap:insert map (Point 0 0) 1))
```

The instance that is generated will be the "obvious" one in each case. For example, equality will test that every sub-field of each case is equal. This may not be desired for every type, and hence sometimes custom instances are still needed.

### Builtin `derive` Type Classes

Instances of the following classes can be derived:

- `Eq`
- `Hash`
- `Default`

Currently these are the only derivable classes in the standard library, but more may be added in the future.

Writing custom derivers does not yet have an official API, but for the adventurous, it can be done relatively easily. For guidance, see [derivers.lisp](./../library/derivers.lisp).

## Do Notation

Coalton has a `do` macro that works similarly to do notation in Haskell.

```lisp
(coalton-toplevel
  (define (f ax bx)
    (do
      (a <- ax)
      (b <- bx)
      (let c = (+ a b))
      (pure c)))

    ;; [6+3, 5+3, 4+3, 6+2, 5+2, 4+2, 6+1, 5+1, 4+1]
   (define xs (f (make-list 1 2 3) (make-list 4 5 6))))
```

## Inline Type Annotations

Inline type annotations can be added to resolve ambiguities when using type classes.

```lisp
(coalton-toplevel
  (define f (the U32 (+ (fromInt 5) (fromInt 7)))))
```

## Shorthand Function Syntax

Coalton does not have nullary functions. However, a function with the type signature `Unit -> *` can be called in Coalton without explicitly passing `Unit`. For instance, the Coalton form `(coalton/vector:new)` is a shorthand for `(coalton/vector:new Unit)`.

Functions can also be defined with an implicit parameter using `(fn () 5)`. This creates a function with a single implicit parameter of type `Unit`.

## Inline Lisp

Coalton can embed raw Common Lisp forms with `lisp`:

```lisp
(coalton-toplevel
  (declare random-int (Integer -> Integer))
  (define (random-int n)
    (lisp Integer (n)
      (cl:random n))))
```

The form is:

```lisp
(lisp <return-type> (<coalton-variables>) <lisp-form>...)
```

This is unsafe; Coalton makes no attempt to analyze anything that is happening inside of a `lisp` form. That means it's possible (and easy) to create type errors, among other things.

### Multiple Values Directive `multiple-values`

When the return type is a `Tuple`, you can request a multiple-value return convention by using the `multiple-values` directive:

```lisp
(coalton-toplevel
  (declare quot-rem (Integer -> Integer -> (Tuple Integer Integer)))
  (define (quot-rem x y)
    (lisp multiple-values (Tuple Integer Integer) (x y)
      (cl:truncate x y))))
```

`lisp multiple-values` requires a `Tuple` return type. The compiler can keep values unboxed through tuple-consuming code paths (for example immediate `match`, `fst`, or `snd`) and only allocate a tuple object when one is actually needed.

## Inspecting the Coalton System

The `coalton` package defines several debugging functions.

`type-of` and `kind-of` can be used to inspect the types and kinds of definitions.

```
COALTON-USER> (type-of 'map)
∀ :A :B :C. FUNCTOR :C ⇒ ((:A → :B) → (:C :A) → (:C :B))
COALTON-USER> (kind-of 'Result)
* -> (* -> *)
```

The following functions all take an optional package parameter.

* `print-type-db` - print every known type
* `print-value-db` - print the type of every toplevel value
* `print-class-db` - print every class and their methods
* `print-instance-db` - print the instances of every class

## Instance Defaulting

Coalton has a similar [type defaulting system](https://www.haskell.org/onlinereport/decls.html#sect4.3.4) as Haskell. Type defaulting is invoked on implicitly typed definitions and code compiled with the `coalton` macro. Defaulting is applied to a set of ambiguous predicates, with the goal to resolve an ambiguous type variable to a valid type. Coalton will only default if one or more of the predicates is a numeric type class (`Num`, `Quantizable`, `Reciprocable`, `ComplexComponent`, `Remainder`, `Integral`). Coalton will default an ambiguous variable to either `Integer`, `F32`, or `F64`; taking the first type that is valid for all predicates referencing that type variable. Coalton will not default when one or more of the predicates containing an ambiguous variable is a multi-parameter type class.


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

## Quirks and Differences from Common Lisp

* Coalton uses `True` (not `t`).  As such, `t` may be used as an ordinary variable name.
* For testing equality, Coalton uses double-equals, `==`.
* Lists in Coalton must be homogeneous.
* To denote anonymous functions, Coalton uses `fn` (*not* `lambda`).
* Numerical operators like `+` only take 2 arguments.
* Negation is done with `negate`.  The form `(- x)` is a curried function equivalent to `(fn (z) (- x z))`.

For more details, see the [glossary](./glossary.md).

# Incomplete Features

Coalton presently supports these features, but more work remains to be
done to improve upon them.

## Exception Handling

Coalton includes syntax for defining, signaling, handling and
resuming from exceptional conditions.  

Briefly, the relevant syntactic forms are:

- `define-exception`: Defines an exception type. Other than its name, the syntax is identical to `define-type`
- `define-resumption`: Defines a named resumption type. 
- `catch`: An expression for catching and handling exceptions. Handlers pattern match on exception constructors.
- `throw`: Signals an exception.
- `resumable`: An expression that intercepts and handles a possible resumption. Again, resumption cases are executed by pattern matching on intercepted resumption constructors.
- `resume-to`: An expression that takes a resumption instance.  Transfers control to a `resumable` block that includes a handler for the indicated resumption.

Coalton's exception handling system is incomplete and evolving. The design has been chosen to allow for experimentation and forward-compatibility as its features mature. See the Caveats section below. 

### Defining, Throwing, and Catching Exceptions

If you want to catch any exception, including Common Lisp error conditions, you can use a wildcard pattern:

```lisp
(declare divide-by-random (Integer -> Integer -> Integer))
(define (divide-by-random r m)
    "Divide `r` by a random integer between `0` and `m`. 
     If the divisor is `0`, then print the divide by zero error
     and then return `0.0`"
    (catch (lisp Integer (r m) (cl:/ r (cl:random m)))
        (_ (trace "An error was received")
           0)))
```

More generally

```lisp 
(define-type Egg
  ;;     cracked? cooked?
  (Goose Boolean Boolean)
  (Xenomorph))

;; We define an exception type BadEgg with a few variants 
(define-exception BadEgg
  (UnCracked Egg)
  (DeadlyEgg Egg))

;; If we try to crack open a Xenomorph egg, throw a DeadlyEgg error
(declare crack (Egg -> Egg))
(define (crack egg)
  (match egg
    ((Goose _ cooked?)
     (Goose True cooked?))
    ((Xenomorph)
     (throw (DeadlyEgg egg)))))

;; crack an egg open safely. 
(declare crack-safely (Egg -> (Result BadEgg Egg)))
(define (crack-safely egg)
  (catch (Ok (crack egg))
    ((DeadlyEgg _) (Err (DeadlyEgg egg)))
    ((UnCracked _) (Err (UnCracked egg)))))
```

### Defining, Invoking, and Handling Resumptions 

Resumptions allow the coalton programmer to recover from an error
without unwinding the call stack.

The `define-resumption` form accepts a single "Constructor". The name
of the constructor is also the name of the type of the resumption.

The following example, building on the above, should elucidate

```lisp
(define-resumption SkipEgg)
(define-resumption (ServeRaw Egg) 
  "Suggest the egg be served raw.")

(declare cook (Egg -> Egg))
(define (cook egg)
  (let ((badegg (Uncracked egg)))     ; exceptions can be constructed outside throw
    (match egg
      ((Goose (True) _)  (Goose True True))
      ((Goose (False) _) (throw badegg))
      ((Xenomorph)       (throw (DeadlyEgg egg))))))

;; Return None if a SkipEgg resumption is received.
(declare make-breakfast-with (Egg -> (Optional Egg)))
(define (make-breakfast-with egg)
  (resumable (Some (cook (crack egg)))
    ((SkipEgg) None)))
```

Now define a function that makes breakfast for `n` people.  It tries to cook each egg, but if it errors by encountering a deadly egg, it resumes `make-breakfast` by skipping that egg. 

```lisp 
(declare make-breakfast-for (UFix -> (Vector Egg)))
(define (make-breakfast-for n)
  (let ((eggs (vector:make))
        (skip  SkipEgg))              ; can construct outside of resume-to
    (for i in (iter:up-to n)
      (let egg = (if (== 0 (mod i 5)) Xenomorph (Goose False False)))
      (do
       (cooked <- (catch (make-breakfast-with egg)
                    ((DeadlyEgg _)    (resume-to skip))))
       (pure (vector:push! cooked eggs))))
    eggs))
```

Every 5th egg is deadly, so making breakfast for 10 people will result in 8 cooked eggs.

The Call stack looks like

```
make-breakfast-for 
      │
      └─ make-breakfast-with 
                │
                └─ cook  
```

But `cook` signals a `DeadlyEgg` error on `Xenomorph`
eggs. `make-breakfast-for` catches that error and resumes to
`SkipEgg`, where `make-breakfast-with` receives that resumption and
handles it.


### Caveats 

For the time being, the following caveats apply;

1. No support for polymorphism for `throw` or `resume-to`
   expressions. E.g. the following will not compile without type
   annotation:
   - `(define (th a) (throw a))` 
   - `(define (res a) (resume-to a))`

2. No way to `catch` a Lisp condition and bind it to a variable in a
   `catch` handler case. However Lisp conditions can be caught using a
   wildcard pattern. In particular, this means that you cannot rethrow
   a Lisp exception.  Furthermore, you may only rethrow an exception by
   re-constructing one.  E.g.
   - `(catch (bad-thing) (_ Unit))` 
   - `(catch (bad-thing) ((MyBad x) (trace "my bad") (throw (MyBad x))))`
   
3. `resumable` branches are even more restrictive. You cannot match
   against anything _other_ than a resumption constructor pattern.

4. No type class is associated with exception-signaling forms. We are
   pursuing different approaches to static checking of forms that
   might hop the call stack. In the end, a type class approach may win
   out. Whatever we do, we will endeavor to make it compatible with
   the existing syntax and semantics.


   
