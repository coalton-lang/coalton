---
title: "Whirlwind Tour of Coalton"
description: "A broad, practical tour of Coalton's core language and workflow."
hideMeta: true
weight: 20
---

Coalton is a statically typed language that is embedded in, and compiles to, Common Lisp.

This document is aimed toward people who are already familiar with functional programming languages. If you are already familiar with Common Lisp, the [glossary](https://github.com/coalton-lang/coalton/blob/main/docs/glossary.md) may be useful.

{{<toc>}}

## Systems

A "system" is a Common Lisp library of code that expresses project structure and dependencies. Coalton makes use of the *de facto* standard ASDF and ordinary Common Lisp packages to organize code. If you're starting a new project, add `#:coalton` to your ASD's `:depends-on` list. For special ASDF support, add `#:coalton-asdf` to `:defsystem-depends-on`. For improved error messages, also depend on `#:named-readtables`.

### `.ct` Files and ASDF

Coalton code is often written in ordinary `.lisp` files, but Coalton also
supports `.ct` files in ASDF definitions. A `.ct` file is still a Lisp source
file: it contains ordinary Lisp forms such as `defpackage` and `in-package`. Unlike an ordinary `.lisp` file, a `.ct`
file is already read in Coalton's readtable, so it does not need an explicit
`named-readtables:in-readtable` form. The main role of the `.ct` file is to
easily distinguish code that is primarily Coalton code in a project, and to
make using Coalton a little more ergonomic in such files.

To use `.ct` files in an ASDF system, add `coalton-asdf` to
`:defsystem-depends-on`, then use `:ct-file` in the component list:

```lisp
(asdf:defsystem "my-project"
  :defsystem-depends-on ("coalton-asdf")
  :depends-on ("coalton")
  :components ((:file "lisp-functions")   ; .lisp file
               (:ct-file "core")))        ; .ct file
```

For example, a preceding Lisp file can define helper functions that `core.ct`
uses:

```lisp
;;;; lisp-functions.lisp
(cl:in-package #:my-package)

(cl:defun native-add1 (x)
  (cl:1+ x))
```

```lisp
;;;; core.ct
(in-package #:my-package)

(coalton-toplevel
  (define (add1-through-lisp n)
    (lisp (-> Integer) (n)
      (native-add1 n)))))
```

In other words, `:ct-file` is an ASDF convenience for Coalton-style Lisp source.
It does not introduce a separate surface language.


### Package Inferred System

For simple projects, you may use the `package-inferred-system` utility to automatically create an ASDF system based on your package structure. However, this project, coalton, does not use these utilities. By this incompatibility, the ASDF loader skips loading `#:coalton-prelude` when using this utility on your own project. Hence, you must explicitly register `coalton-prelude` in your own ASDF system definition before specifying `#:coalton` in your `:depends-on` list.

```lisp
;; In your ASDF system definition; e.g., my-project.asd
(register-system-packages '#:coalton '(#:coalton-prelude))
```

## Packages

A "package" is a Common Lisp namespace of symbols.

Unlike Common Lisp which has one monolithic package called `"COMMON-LISP"`, Coalton's standard library is organized as a large collection of packages. For example, string-related functions are in the `#:coalton/string` package. Refer to the [Coalton Reference](https://coalton-lang.github.io/reference) for a complete list of standard library packages.

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
```

We recommend using `.ct` files, however, if you're writing Coalton in a `.lisp` or `.cl` file, you should also be in the Coalton readtable:

```lisp
(named-readtables:in-readtable coalton:coalton)
```

This is done automatically with `.ct` files. The `named-readtables:in-readtable` form is optional but encouraged. Using Coalton's reader allows compiler errors to accurately refer to source code and provide correct line numbers.

The first primary entry points for Coalton code. Definitions and the like sit in a toplevel-form called `coalton-toplevel`.

```lisp
(coalton-toplevel
  ;; <Coalton definition forms>
  )
```

Currently, in SLIME/SLY, there's no way to `C-c-c` in any finer-grained way than a whole `coalton-toplevel` form. This may encourage you to define logical sections of functionality in separate `coalton-toplevel` forms.

The second primary entry point is calling Coalton from Lisp. In this case, one uses the `coalton` operator:

```lisp
;; Lisp code
;; ...
     (coalton #|coalton expression|#)
;; ...
```

Note that one _cannot_ make new definitions in a `coalton` form, only evaluate expressions.

Whereas `coalton-toplevel` expects one or more toplevel definitions or declarations, the `coalton` form evaluates one or more expressions relative to the current environment and returns the underlying Lisp value of the last expression. Multiple expressions are wrapped in an implicit `progn`. This can be useful for working with Coalton from a Lisp REPL.

> [!IMPORTANT]
> Remember that Coalton packages, including `#:coalton-user`, do *not* `:use` the `#:common-lisp`/`#:cl` package, so you must prepend Common Lisp symbols with `cl:` if you need them.


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

> [!IMPORTANT]
> It may be tempting to elide the `coalton` form and simply evaluate `(fst p)` directly in a Lisp REPL, but such behavior should not be relied upon!

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

### Top-Level Type Declarations

Coalton infers all types, but it's often better to express your intent by way of a type declaration. Top-level definitions can have their type declared with `declare`.

```lisp
(coalton-toplevel
  (declare add2 (Integer -> Integer))
  (define (add2 x)
    (+ 2 x))

  (declare three Integer)
  (define three (add2 1)))
```

We will discuss static typing in detail later in this document.

### Fixed-Arity Functions and Manual Currying

Coalton functions have fixed input and output arities. A type like `Integer * String -> Boolean`
describes a function that takes two arguments and returns one value. A type like
`Integer -> String -> Boolean` describes a function that takes one argument and returns another
function.

Consider this function:

```lisp
(coalton-toplevel
  (define (fma a b c)
    (+ c (* a b))))
```

`fma` really takes three arguments:

```lisp
(coalton-toplevel
  (define fma3 (fma 2 3 4))) ; evaluates to 10
```

Unlikes other ML-derived languages, Coalton does not "auto-curry" or "partially apply" functions. A function must take exactly the number of inputs it was defined to take. If you want currying or partial application, write it explicitly by returning a function using `fn`:

```lisp
(coalton-toplevel
  (define (make-fma a)
    (fn (b)
      (fn (c)
        (+ c (* a b)))))

  (define fma1 (make-fma 2))
  (define fma2 ((make-fma 2) 3))
  (define fma3 (((make-fma 2) 3) 4)))
```

Here is an example of using an explicitly constructed function to transform a list.

```lisp
(coalton-toplevel
  ;; Lists can be created with the make-list macro
  (define nums (make-list 2 3 4 5))

  (define add2
    (fn (x)
      (+ 2 x))))

(coalton
  (map add2 nums)) ;; 4 5 6 7
```

Functions may also return multiple values directly. This is distinct from returning a `Tuple`, which is still an ordinary single value.

```lisp
(coalton-toplevel
  (declare sum-and-product (Integer * Integer -> Integer * Integer))
  (define (sum-and-product x y)
    (values (+ x y) (* x y))))

(coalton
  (let (values s p) = (sum-and-product 3 4)
  (+ s p))) ;; 19
```


### Keyword Arguments

Coalton also supports keyword arguments. Keyword arguments come after all positional arguments and are introduced with `&key`:

```lisp
(coalton-toplevel
  (define (render-line x &key (width 80) (pad "."))
    ...))
```

Every actual keyword parameter must have an initial value. The parameter name determines the
surface keyword, so `width` is called with `:width` and `pad` is called with `:pad`.

Calls supply positional arguments first, followed by keyword/value pairs:

```lisp
(coalton-toplevel
  (render-line 5)
  (render-line 5 :width 120)
  (render-line 5 :pad "-" :width 120))
```

Keyword arguments are optional. If a keyword is omitted, its initial value is used. Once a
keyword argument appears, no later positional arguments are allowed.

We recommend using the `Optional` type to denote arguments which do not require a value.
However, an explicit `None` initializer is still required.

Anonymous functions use the same syntax:

```lisp
(coalton-toplevel
  (define add-offset
    (fn (x &key (offset 1))
      (+ x offset))))
```

Types write the keyword tail between the positional inputs and `->`:

```lisp
(declare render-line (Integer &key (:width Integer) (:pad String) -> String))
(declare make-default (&key (:x Integer) -> Integer))
```

A bare `&key` with no following keyword parameters is allowed in both definitions and types, but
it is treated exactly the same as omitting `&key` entirely:

```lisp
(define (identity-with-empty-key x &key) x)
(declare identity-with-empty-key (Integer &key -> Integer))
```

Explicit declarations are exact about which keywords exist. Inferred higher-order function types
may internally remain open to additional keywords for compatibility, but there is no separate
surface syntax for those open rows.


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

> [!NOTE]
> Since these are macros (indicated by their variadic arguments), they *cannot* be used as higher-order functions. Consider either an explicit `fn` wrapper or the `compose` function if you're thinking in that direction.

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

Variables prefixed with `_` like `_y` are still normal variables, and can be read. The following is valid Coalton:

```lisp
(define (f _x) _x)
```

One should treat underscore prefixed variables as ignored whenever possible, and use a name not prefixed with `_` if it may be used. Reading from underscore-prefixed variables is permitted so that generated code (e.g., using macros or read-conditionals) may avoid unused variable warnings for variables which will be used in some compilation contexts but not others.

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

There are several debugging tools which are useful when working with type aliases. Outside of a Coalton expression, `describe-type-of` prints the type of a symbol together with its aliases, and `describe-type-alias` prints a diagnostic representation of an alias together with the aliased type. Both are print-only helpers and return no values. For ordinary type display, such as `(coalton (type-of expr))`, Coalton can be configured to display only aliases, only types, or both. The preference can be set before compiling Coalton using `(setf (get ':coalton-config ':type-printing-mode) mode)` where `mode` is one of `:types`, `:aliases`, and `:types-and-aliases`. Thereafter, the mode can be changed among those three options using the function `set-type-printing-mode`. The default mode is `:types`.

```lisp
COALTON-USER> (coalton-toplevel
                (define-type-alias A Integer)
                (define x (the A 5)))
; No value

COALTON-USER> (set-type-printing-mode :aliases)
:ALIASES

COALTON-USER> (coalton (type-of x))
A

COALTON-USER> (set-type-printing-mode :types-and-aliases)
:TYPES-AND-ALIASES

COALTON-USER> (coalton (type-of x))
[A := INTEGER]

COALTON-USER> (set-type-printing-mode :types)
:TYPES

COALTON-USER> shifted-coordinate ;; from the example above
#.(TUPLE 1 0)

COALTON-USER> (coalton (type-of shifted-coordinate))
(TUPLE INTEGER INTEGER)

COALTON-USER> (describe-type-of 'shifted-coordinate)
[PAIR [COORDINATE := INTEGER] := TUPLE [COORDINATE := INTEGER] [COORDINATE := INTEGER]]

COALTON-USER> (describe-type-alias 'Pair)
; prints a diagnostic representation containing [PAIR :A := TUPLE :A :A]
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

## Local Variables: `let` and `let*`

The operators `let` and `let*` are expression forms
for local bindings. `let` binds its variables recursively and in parallel
(similar to Scheme's `letrec`), while `let*` binds them left-to-right so each
initializer can refer to the earlier bindings.

```lisp
(coalton
  (let ((x (+ y 1))
        (y 2))
    (+ x y)))
```

```lisp
(coalton
  (let* ((x 1)
         (y (+ x 1)))
    (+ x y)))
```

`let*` accepts the same binding syntax and local `declare` forms as `let`.

`let` can also be used to introduce local (possibly recursive)
functions.

```lisp
(let ((odd (fn (x)
             (if (zero? x)
                 False
                 (even (1- x)))))
      (even (fn (x)
              (if (zero? x)
                  True
                  (odd (1- x)))))))
  (odd 5))
```

> [!NOTE]
> Since `let` introduces recursive bindings, its generally
> not allowed for variables to be self-referential, as in
> `(let ((x (1+ x))) ...)`. Instead, use `let*`.

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

Coalton has a special built-in operator `rec` for doing "named-let"-style iteration. Its general syntax is:

```lisp
(rec <name> (<binding>*)
  <body>...)

;; <binding> := <declare-form>
;;            | (<symbol> <value>)
```

The `<name>` is in essence a local recursive function in the `<body>`, and the
binding list supplies the arguments for its initial call. Local `declare` forms
inside the binding list apply to the initial bindings, which in turn constrain
the recursive function's parameters. If you want to constrain the result type of
the whole `rec` expression, wrap it in `the`.

Recursive uses of `<name>` must be direct tail calls. This makes `rec` a good
fit for iteration and accumulator-passing style loops. If you want more
flexible local recursion, or you want to treat the recursive function as a
first-class value, use `let` together with `fn` instead.

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

We can make a monomorphic variant for `Integer` by declaring the init bindings
and wrapping the whole `rec` in `the`.

```lisp
(define (fib-monomorphic n)
  (the Integer
    (rec %
         ((declare i Integer)
          (declare a Integer)
          (declare b Integer)
          (i n)
          (a 0)
          (b 1))
      (cond
        ((== 0 i) a)
        ((== 1 i) b)
        (True (% (- i 1) b (+ a b)))))))
```

Now it works without any type declarations on use:

```
> (coalton (fib-monomorphic 10))
55
```


Coalton supports imperative looping, conditional looping, and pattern-based iteration.

### Conventional Looping: `for` and `for*`

The built-in imperative `for` form and its sequential variant `for*` have the
following shape:

```lisp
(for [label] (<binding-clause>*)
  [:returns expr]
  [(:while | :until | :repeat) expr]
  body...)

;; <binding-clause> := (declare var Type)
;;                   | (var init-expr step-expr)
```

The binding list is required, but it may be empty: `()`. Initializers are
evaluated once before the first iteration. In `for`, the initializer binding
group follows the same scoping rules as `let`, and step expressions are
evaluated simultaneously using the pre-step bindings from that iteration. In
`for*`, initializers follow `let*`-style left-to-right scoping, and step
expressions are performed from left to right so later steps can refer to
earlier updated variables. In that sense, `for*` plays the same role as Common
Lisp's `do*`. If no termination clause is present, the iteration is infinite
unless exited by `break`.

Each non-`declare` loop binding must include both an initializer and a step
expression. If a loop variable should stay constant across iterations, repeat it
as its own step, for example `(x 10 x)`.

You can iterate forever:

```lisp
(for ()
  (show "hi"))
```

You can also use `for` as a counted or state-threading loop:

```lisp
(coalton
 (for ((declare i UFix)
       (declare acc UFix)
       (i 0 (1+ i))
       (acc 0 (+ acc i)))
   :returns acc
   :repeat 10
   (when (even? i)
     (continue))
   (show i)))
```

When later loop bindings should build on earlier ones, use `for*`:

```lisp
(coalton
 (let ((xs (vector:make 10 20 30)))
   (for* ((declare i UFix)
          (declare x Integer)
          (i 0 (1+ i))
          (x (vector:index-unsafe i xs) (vector:index-unsafe i xs)))
     :returns (Tuple i x)
     :repeat 2
     Unit)))
```

Here `x` tracks the current vector element while `i` tracks the index. Because
this is `for*`, the step for `x` sees the updated value of `i`.

If a `:returns` clause is present, that expression is evaluated once when the `for` exits
normally or via `break`. If `:returns` is absent, the `for` produces no values.

You can use a `:while` clause when you only need a condition and a body:

```lisp
(coalton
 (let ((counter (cell:new 0))
       (limit 10))
   (for ()
     :while (< (cell:read counter) limit)
     (show "hi")
     (cell:increment! counter))))
```

You can also combine `for` with `match` and `break` when iteration depends on a
pattern match:

```lisp
(coalton
 (let ((xs (vector:make 4 3 2 1)))
   (for ()
     (match (vector:pop! xs)
       ((Some x) (show x))
       ((None) (break))))))
```

#### `break` and `continue`

The imperative `for` form supports `break` and `continue`.

The `break` form immediately terminates iteration.  The following
prints out `0`, `1`, and `2` and then terminates.

```lisp
(coalton
 (let counter = (cell:new 0))
 (for ()
   (when (== 3 (cell:read counter))
     (break))
   (show (cell:read counter))
   (cell:increment! counter)))
```

The `continue` form skips the remainder of the `for` body and starts
on its next iteration. The following prints out `1`, `3`, and `5`,
having skipped the even values.

```lisp
(coalton
 (let counter = (cell:new 0))
 (for ()
   (when (== 6 (cell:read counter))
     (break))
   (let n = (cell:read counter))
   (cell:increment! counter)
   (when (== 0 (mod n 2))
     (continue))
   (show n)))
```

For the imperative `for` form specifically, `continue` still performs the
step phase before the next iteration, while `break` skips the current iteration's
step phase.


#### For Labels

The `for` form takes an optional label keyword. These labels
can be used in conjunction with `break` and `continue` to achieve
complex control flow.

A label may immediately follow `for`:

```lisp 

(for :outer () (do-stuff))

(for :another-label ()
  :while (is-true?)
  (do-stuff))

```

In the following entirely artificial example, the outermost `for` is
labeled `:outer`. This label is passed to `break` from inside the
inner `for` to terminate iteration whenever the sum of `x` and `y`
exceeds 500. Without the `:outer` label, `break` would have only
broken out of the inner `for`.

```lisp
(coalton 
  (let ((acc (cell:new Nil)))
    (for :outer ((x 0 (+ x 10)))
      (for ((y 0 (1+ y)))
        :while (< y 10)
        (let total = (+ x y))
        (when (< 500 total)
          (break :outer))
        (when (== 0 (mod y 3))
          (continue))
        (cell:push! acc total)))
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

Coalton does have a division operator `/`, but division is split into a couple of related concepts. Division is tricky for two reasons:

1. Division can fail if we divide by something like zero,

2. Dividing two numbers doesn't necessarily result in the same type. (In fact, division may not even be possible!)

To address the first concern, division may result in a run-time error. We don't use `Optional` because it is quite cumbersome to use in mathematical contexts. (One may use `safe/` for a variant that does a division-by-zero check and produces an `Optional`.)

To address the second concern, Coalton separates same-type division from cross-type division.

The operator `/` is the method of the `Reciprocable` type class, so it performs division where the inputs and output all have the same type:

```
COALTON-USER> (describe-type-of '/)
∀ :A. RECIPROCABLE :A ⇒ :A * :A → :A
```

For cross-type division, Coalton provides the `Dividable` type class and its method `general/`. The type expression

```
(Dividable :s :t)
```

says that division of two items of type `:s` may result in an item of type `:t`.

Because of [Instance Defaulting](#instance-defaulting), division of `Integer` constants without any additional context defaults to `F64` division:

```
COALTON-USER> (coalton (/ 1 2))
0.5d0
```

We can inform Coalton that our literals are of another type by constraining them with `the` or relying on type inference. For example, in order to use `/` at a type other than the defaulted `F64`, constrain it to another `Reciprocable` type:

```
COALTON-USER> (coalton (the F32 (/ 4 2)))
2.0
COALTON-USER> (coalton (the Fraction (/ 4 2)))
2
```

An `Integer` result from division with `/` is not possible, because `Integer` does not implement `Reciprocable`:

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

Why shouldn't this just be `2`?! The unfortunate answer is that Coalton keeps integer division choices explicit. If you want exact rational division from integers, use `exact/`; if you want an inexact floating answer, use `inexact/`; and if you want integer division with a particular rounding rule, use `floor/`, `ceiling/`, or `round/`.

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

All of these cases are sufficiently common that we provide a few shorthands built on top of `general/`:

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

## Collection and Association Builder Syntax

When the Coalton reader is active, square brackets can be used as collection and
association builders.

```lisp
(coalton-toplevel
  (define xs [1 2 3])

  (define empty-xs
    (the (List Integer) []))

  (define pairs
    ["x" => 1
     "y" => 2])

  (define empty-pairs
    (the (Seq (Tuple String Integer))
         [=>])))
```

`[a b c]` is a collection builder. `[]` is an empty collection builder. `[=>]`
is an empty association builder. Any other bracket form containing `=>` is an
association builder.

Collection builders must be homogeneous: all elements must have the same type.
Association builders are homogeneous in both their keys and their values: all
keys must have the same type, and all values must have the same type.

When no concrete result type is specified, collection builders default to
`Seq`, and association builders default to
`Seq (Tuple :key :value)`.

In practice, `the` is often the clearest way to ascertain the intended result
type. This is especially useful for empty builders like `[]` and `[=>]`, since
those forms do not determine their element, key, or value types on their own.
It is also the way to request a specific target collection type.
This is of course unnecessary if the object is inferred to be the desired type.

```lisp
(coalton-toplevel
  (define empty-xs
    (the (List Integer) []))

  (define empty-pairs
    (the (Seq (Tuple String Integer)) [=>]))

  (define xs
    (the (List Integer) [1 2 3]))

  (define ys
    (the (Vector Integer) [1 2 3])))
```

Comprehensions on collections and associations are also supported.

```lisp
(coalton-toplevel
  (define evens
    [x :for x :below 10 :when (even? x)])

  (define scaled
    [y
     :for x :below 5
     :with y = (* x 10)
     :when (even? x)])

  (define first-ten
    [x
     :for x :below 10])

  (define squares
    [x => (* x x) :for x :below 5]))
```

The supported clauses for both are:

- `:for <var> :below <ufix>`: Have a variable range from 0 to below a maximum, incrementing by 1.
- `:for <var> :in <iter>`: Have a variable iterate through an iterator.
- `:with <var> = <expr>`: Lexically bind a variable over the remainder of the comprehension.
- `:when <expr>`: Guard against the truth of `<expr>`.

> [!NOTE]
> Collection and association comprehensions must be finite. They cannot produce infinite sequences.

Collection comprehensions default to `Seq`, and association comprehensions
default to `Seq (Tuple :key :value)` in the same way as itemized builders. As
with `[]` and `[=>]`, `the` can be used whenever you want to ascertain a more
specific result type.

### Extending Builder Syntax to User-Defined Types

Builder syntax is open to user-defined types through four type classes:

- `FromItemizedCollection` for `[a b c]`
- `FromItemizedAssociation` for `[k => v ...]`
- `FromCollectionComprehension` for `[expr :for ...]`
- `FromAssociationComprehension` for `[key => value :for ...]`

The itemized classes are for inputs whose elements or entries are explicitly and
finitely itemized in source. They receive the exact number of source items up
front, and the exact zero-based source index of each item as it is added. The
comprehension classes are stream-oriented: they receive an advisory initial size
hint, then each emitted element or entry, and finally convert their builder
state to the target collection.

Here is a simple collection-builder instance for a user-defined wrapper around
`List`. The builder state is just a temporary list, which is reversed once at
the end.

```lisp
(coalton-toplevel
  (define-type (Bag :a)
    (Bag (List :a)))

  (define-instance (FromItemizedCollection (Bag :a) :a (List :a))
    (define (begin-collection-builder _ _)
      Nil)
    (define (adjoin-to-collection-builder _ builder _ item)
      (Cons item builder))
    (define (finalize-collection-builder _ builder)
      (Bag (reverse builder))))

  (define bag
    (the (Bag Integer) [1 2 3])))
```

The first ignored argument is a `types:Proxy` for the target collection type,
and the third type parameter of `FromItemizedCollection` is the intermediate
builder state. In this example the final collection is `Bag :a`, but the
builder state is just `List :a`.

To support comprehensions as well, define a corresponding
`FromCollectionComprehension` instance. Association syntax works the same way,
using `FromItemizedAssociation` and `FromAssociationComprehension`.

### Immutable Sequences and Maps

Coalton allows mutability, but encourages writing efficient *immutable* code by offering built-in support for immutable sequence and map types. The two most important, general purpose types are `Seq` (from `coalton/seq`) and `HashMap` (from `coalton/hashmap`). These are backed by efficient data structures that allows logarithmic access and efficient concatenation.


## Static Typing

Coalton code is statically type checked. Types are inferred.

```lisp
(coalton-toplevel
  (define (fun x)
    (map (fn (y) (+ 2 y)) (str:parse-int x))))
```

The type of a variable or function can be checked with the Coalton operator `type-of`.

```
COALTON-USER>  (coalton (type-of fun))
(STRING -> (OPTIONAL INTEGER)
```

Type declarations can always be added manually.

```lisp
(coalton-toplevel
  (declare fun (String -> (Optional Integer)))
  (define (fun x)
    (map (fn (y) (+ 2 y)) (str:parse-int x))))
```

Type declarations can also be added in `let`, `let*`, `for`, and `for*`
expressions

```lisp
(coalton-toplevel
  (define (f a b)
    (let ((declare g (Integer * Integer -> Integer))
          (g +))
      (g a b))))
```

When you want the names of the quantified type variables to be part of the
declaration, you can write an explicit `forall`:

```lisp
(coalton-toplevel
  (declare keep-first (forall (:left :right) :left -> :right -> :left))
  (define (keep-first x _y)
    x))
```

The body of `forall` can be written either in the short form above or as a
single parenthesized type:

```lisp
(declare keep-first (forall (:left :right) (:left -> :right -> :left)))
```

Explicit `forall` also makes those type variables lexically available inside the
corresponding definition. Nested `declare`, `the`, and `lisp` annotations can
reuse them:

```lisp
(coalton-toplevel
  (declare scoped-id (forall (:item) :item -> :item))
  (define (scoped-id x)
    (let ((declare keep-item (Void -> :item))
          (keep-item (fn ()
                       (the :item x))))
      (keep-item))))
```

The same rule applies to local functions declared inside `let`. If a local
declaration has an explicit `forall`, those binders are in scope inside the
local function body, and the local body can also reuse outer scoped binders:

```lisp
(coalton-toplevel
  (declare scoped-local (forall (:item) :item -> :item))
  (define (scoped-local x)
    (let ((declare keep-outer
                  (forall (:tmp) :item * :tmp -> :item))
          (keep-outer (fn (y _z)
                        (the :item y))))
      (keep-outer x Unit))))
```

Here, `:item` comes from the outer declaration and `:tmp` comes from the local
`forall`. If an inner `forall` reuses a binder name, the inner binder shadows
the outer one.

Declarations without `forall` are still implicitly quantified, but their type
variable names do not become scoped names inside the body.

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

The `into` method is used only when a conversion can always be performed from one type to another. If not values of a type can be converted, then another type class `TryInto` with a method `tryInto` is used. The `tryinto` method returns an `Optional` type, yielding `Some` on success and `None` on failure.

**Note that `as` only works for conversions via `into`, i.e., conversions that are total.** There is no corresponding syntax for `tryInto`.

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

- OCaml manual, "Polymorphism and its limitations".
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

In this case, `really-expensive` will never get called due to short-circuiting. Also note that both `and` and `or` can take zero or more arguments.


## `COALTON:PROGN`

Coalton has a `coalton:progn` construct similar to lisp.

```lisp
(coalton-toplevel
  (declare f (Integer * Integer -> Integer * Integer))
  (define (f x y)
    (progn
      (+ x y)
      (* x y)
      (values x y))))
```

Outside flattened-body contexts, use the ordinary `let` and `let*` forms
described above. Inside `progn`, Coalton also has flattened `let` syntax.

```lisp
(coalton-toplevel
 (declare f (Integer * Integer -> String))
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
  (declare f (Integer * Integer -> String))
  (define (f x y)
    (let x_ = (into x))
    (let y_ = (into y))
    (<> x_ y_)))
```

## Dynamic Variables

Coalton also supports dynamically scoped variables, similar to Common Lisp
special variables.

- Dynamic variable names must begin and end with `*`, and must contain at
  least one non-`*` character, such as `*x*` or `*current-port*`.
- Ordinary lexical variables cannot use earmuff names.
- Dynamic variables are defined and declared with the usual `define` and
  `declare` forms.

```lisp
(coalton-toplevel
  (declare *base* Integer)
  (define *base* 10)

  (define (base-value)
    *base*))
```

Referencing a dynamic variable uses the variable name directly:

```lisp
(coalton
  *base*)
```

To rebind one or more dynamic variables, use `dynamic-bind`:

```lisp
(coalton
  (dynamic-bind ((*base* 20))
    (base-value))) ; returns 20
```

Inside `progn`, function bodies, and similar flattened-body contexts, there is
also shorthand syntax:

```lisp
(coalton-toplevel
  (define (f)
    (let *base* = 20)
    (base-value))) ; returns 20
```

Dynamic bindings are parallel and non-recursive. That means each initializer is
checked in the outer environment and can refer to the old dynamic value:

```lisp
(coalton-toplevel
  (declare *left* Integer)
  (define *left* 1)
  (declare *right* Integer)
  (define *right* 2)

  (define swapped
    (dynamic-bind ((*left* *right*)
                   (*right* *left*))
      (Tuple *left* *right*))))
```

The type of a dynamic rebinding must preserve the variable's existing type.
Polymorphic dynamic variables use the same value-restriction and variance checks
as ordinary inferred bindings, so expansive rebindings are only accepted when
their weak type variables can be generalized safely.

Dynamic variables may have any ordinary Coalton type, including function types.
The restriction is on rebinding, not on the declared type: each rebinding must
preserve the variable's existing type, and polymorphic rebindings remain subject
to the usual value restriction.

Because `dynamic-bind` establishes and later restores dynamic scope, wrapping a
recursive call in `dynamic-bind` breaks tail recursion.

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
    (== (:a * :a -> Boolean)))

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
        (_ False))))

  ;; Type declarations can have constraints
  (declare is-eql (Eq :a => (:a * :a -> String)))
  (define (is-eql a b)
    (if (== a b)
      "They are equal"
      "They are not equal"))

  ;; Multiple constraints must be wrapped in parentheses
  (declare double-is-eql ((Eq :a) (Eq :b) => (:a * :a * :b * :b -> String)))
  (define (double-is-eql a b c d)
    (if (and (== a b) (== c d))
      "Both pairs are equal"
      "The pairs are not both equal")))
```

Class methods can also use explicit `forall`. When they do, those binders are
in scope inside the corresponding `define-instance` method body, so local
annotations there may reuse the method's type variables.


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
  (let map = (the (coalton/hashmap:HashMap Point UFix)
                  coalton/hashmap:empty))
  (coalton/hashmap:insert map (Point 0 0) 1))
```

The instance that is generated will be the "obvious" one in each case. For example, equality will test that every sub-field of each case is equal. This may not be desired for every type, and hence sometimes custom instances are still needed.

### Builtin `derive` Type Classes

Instances of the following classes can be derived:

- `Eq`
- `Hash`
- `Default`
- `Show`

Currently these are the only derivable classes in the standard library, but more may be added in the future.

Writing custom derivers does not yet have an official API, but for the adventurous, it can be done relatively easily. For guidance, see [derivers.lisp](https://github.com/coalton-lang/coalton/blob/main/library/derivers.lisp).

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

## Nullary Functions

Coalton has true nullary functions. A type like `(Void -> Integer)` denotes a function that
takes zero arguments and returns one `Integer`.

Functions can be defined with `(fn () ...)`:

```lisp
(coalton-toplevel
  (declare five (Void -> Integer))
  (define five
    (fn () 5)))
```

`Unit` remains an ordinary type and value. A function of type `Unit -> Integer` still takes one
argument, and must be called as `(f Unit)`.

## Inline Lisp

Coalton can embed raw Common Lisp forms with `lisp`:

```lisp
(coalton-toplevel
  (declare random-int (Integer -> Integer))
  (define (random-int n)
    (lisp (-> Integer) (n)
      (cl:random n))))
```

The form is:

```lisp
(lisp (-> <output-type-spec>) (<coalton-variables>) <lisp-form>...)
```

This is unsafe; Coalton makes no attempt to analyze anything that is happening inside of a `lisp` form. That means it's possible (and easy) to create type errors, among other things.

`<output-type-spec>` may be `Void`, a single type, or a `*`-separated list of output types.
Single-output `lisp` forms consume only the primary Common Lisp value of their last form.
Multiple-output `lisp` forms return Common Lisp multiple values directly. Zero-output `lisp`
forms evaluate their body and then return no values.

The `<output-type-spec>` may also mention a type variable brought into scope by an
enclosing explicit `forall`:

```lisp
(coalton-toplevel
  (declare identity-through-lisp (forall (:item) :item -> :item))
  (define (identity-through-lisp x)
    (lisp (-> :item) (x)
      x)))
```

Embedded `(coalton ...)` forms inside the raw Lisp body are a separate Coalton
compilation context. They do not inherit those lexical type-variable bindings.

Multiple values may be returned from `lisp` just fine:

```lisp
(coalton-toplevel
  (declare quot-rem (Integer * Integer -> Integer * Integer))
  (define (quot-rem x y)
    (lisp (-> Integer * Integer) (x y)
      (cl:truncate x y))))
```

These values can be destructured with `let (values ...) = ...`:

```lisp
(coalton
  (let (values q r) = (quot-rem 17 5)
  (Tuple q r)))
```


## Inspecting the Coalton System

The `coalton` package defines several debugging functions.

The `type-of` Coalton expression and the `kind-of` Common Lisp helper can be used to inspect the types and kinds of definitions.

```
COALTON-USER> (coalton (type-of map))
∀ A B F. FUNCTOR F ⇒ (A → B) * F A → F B
COALTON-USER> (kind-of 'Result)
* → (* → *)
```

The following functions all take an optional package parameter.

* `print-type-db` - print every known type
* `print-value-db` - print the type of every toplevel value
* `print-class-db` - print every class and their methods
* `print-instance-db` - print the instances of every class

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

## Quirks and Differences from Common Lisp

* Coalton uses `True` (not `t`).  As such, `t` may be used as an ordinary variable name.
* For testing equality, Coalton uses double-equals, `==`.
* Lists in Coalton must be homogeneous.
* To denote anonymous functions, Coalton uses `fn` (*not* `lambda`).
* Numerical operators like `+` only take 2 arguments.
* Negation is done with `negate`. The operator `-` is a fixed-arity subtraction operator.

For more details, see the [glossary](https://github.com/coalton-lang/coalton/blob/main/docs/glossary.md).

## Incomplete Features

Coalton presently supports these features, but more work remains to be
done to improve upon them.

### Exception Handling

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

#### Defining, Throwing, and Catching Exceptions

If you want to catch any exception, including Common Lisp error conditions, you can use a wildcard pattern:

```lisp
(declare divide-by-random (Integer * Integer -> Integer))
(define (divide-by-random r m)
    "Divide `r` by a random integer between `0` and `m`. 
     If the divisor is `0`, then print the divide by zero error
     and then return `0.0`"
    (catch (lisp (-> Integer) (r m) (cl:/ r (cl:random m)))
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

#### Defining, Invoking, and Handling Resumptions 

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
        (skip  SkipEgg))
    (for ((i 0 (1+ i)))
      :repeat n
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


#### Caveats 

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


   
