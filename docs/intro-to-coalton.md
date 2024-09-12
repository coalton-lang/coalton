# Whirlwind Tour of Coalton

Coalton is a statically typed language that is embedded in, and compiles to, Common Lisp.

This document is aimed toward people who are already familiar with functional programming languages.

## Systems and Packages

Coalton uses ordinary Common Lisp packages (and ASDF systems) to organize code. If you're starting a new project, add `#:coalton` to your ASD's `:depends-on` list. For improved error messages, also depend on `#:named-readtables`.

Unlike Common Lisp, Coalton's standard library is organized as a large collection of packages. For example, string-related functions are in the `#:coalton-library/string` package. Refer to the [Coalton Reference](https://coalton-lang.github.io/reference) for a complete list of standard library packages.

When making a new project, you will want to define a new package and establish your standard library imports. *Do not `:use` the `#:cl` or `#:common-lisp` packages!* Instead, you'll want to, at minimum, `:use` both the `#:coalton` package (for core language features) and `#:coalton-prelude` (for extremely common standard library definitions):

```lisp
(defpackage #:my-package
  (:use 
   #:coalton 
   #:coalton-prelude))
```

The `#:coalton-prelude` package does not contain any functionality that isn't also found elsewhere in the standard library, and hence, it is a convenience.

In idiomatic usage, this minimal package definition would be far too limiting to be useful. For instance, if we wanted to use the standard library function `strip-prefix` (which strips a prefix off of a string), we would have to type `coalton-library/string:strip-prefix` in our program. Instead, we can create a local nickname for this package:

```lisp
(defpackage #:my-package
  (:use 
   #:coalton)
  (:local-nicknames 
   (#:str #:coalton-library/string)))
```

This way, we can now just type `str:strip-prefix`. Any time we make use of a new function, either from the Coalton standard library or from a third-party library, we might add nicknames that function's package to our `:local-nicknames` list. For example:

```lisp
(defpackage #:my-package
  (:use 
   #:coalton)
  (:local-nicknames 
   (#:str  #:coalton-library/string)
   (#:vec  #:coalton-library/vector)
   (#:math #:coalton-library/math)))
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
  ;; Functions in coalton are curried
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

```
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

Coalton supports infinite looping, conditional looping, and `for`-loop styled iteration. 

### `loop`, `while`, `while-let`, and `for`

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


### `break` and `continue`

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


### Loop Labels

Each of the above looping forms takes an optional loop label
keyword. These labels can be used in conjunction with `break` and
`continue` to acheive complex control flow.

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
labelled `:outer`. This label is passed to `break` from inside the
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

## Numbers

Coalton supports a few numeric types. The main ones are `Integer`, `Single-Float`, and `Double-Float`.

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

Numbers implement the `Num` typeclass, which has methods `+`, `-`, `*`, and `fromInt`.

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

To address the second concern, we need to introduce a new typeclass called `Dividable`. The type expression

```
(Dividable :s :t)
```

says that division of two items of type `:s` may result in an item of type `:t`. With all of this, we have the final type of `/`.

```
COALTON-USER> (type-of '/)
∀ :A :B. DIVIDABLE :A :B ⇒ (:A → :A → :B)
```

Because of the generic nature of division, if you're computing some values at the REPL, "raw division" simply does not work.

```
COALTON-USER> (coalton (/ 1 2))
Failed to reduce context for DIVIDABLE INTEGER :A
in COALTON
   [Condition of type COALTON-IMPL/TYPECHECKER::COALTON-TYPE-ERROR-CONTEXT]
```

You have to constrain the result type of the `Dividable` instance. You can do this with the `the` operator. There are lots of `Dividable` instances made for you.

```
COALTON-USER> (coalton (the Single-Float (/ 4 2)))
2.0
COALTON-USER> (coalton (the Fraction (/ 4 2)))
#.(COALTON-LIBRARY::%FRACTION 2 1)
```

But division of integers does not work.

```
COALTON-USER> (coalton (the Integer (/ 4 2)))
Failed to reduce context for DIVIDABLE INTEGER :A
in COALTON
   [Condition of type COALTON-IMPL/TYPECHECKER::COALTON-TYPE-ERROR-CONTEXT]
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
                (define wut (make-list 1 2 3.0)))

Failed to unify types SINGLE-FLOAT and INTEGER
in unification of types (INTEGER → (LIST SINGLE-FLOAT) → :A) and (:B → (LIST :B) → (LIST :B))
in definition of WUT
in COALTON-TOPLEVEL
   [Condition of type COALTON-IMPL/TYPECHECKER::COALTON-TYPE-ERROR-CONTEXT]
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

Coalton code is statically typechecked. Types are inferred.

```lisp
(coalton-toplevel
  (define (fun x)
    (map (+ 2) (parse-int x))))
```

The type of a variable or function can be checked with `coalton:type-of`.

```
COALTON-USER> (type-of 'fun)
(STRING -> (OPTIONAL INT)
```

Type declarations can always be added manually.

```lisp
(coalton-toplevel
  (declare fun (String -> (Optional Integer)))
  (define (fun x)
    (map (+ 2) (parse-int x))))
```

Type declarations can also be added in let expressions

```lisp
(coalton-toplevel
  (define (f a b)
    (let ((declare g (Integer -> Integer -> Integer))
          (g +))
      (g a b))))
```

### Type Casting, Coercing, and Conversion

Coalton manages type conversions similar to the Common Lisp function `cl:coerce` by way of a type class called `Into` (of the package `#:coalton-library/classes`) and its sole method `into`. However, the `into` method only takes a single argument. How should Coalton know which data type to convert to? It determines this either by type inference (i.e., by the surrounding context) as in this example, where `substring` expects a `String`:

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

The into method is used only when a conversion can always be performed from one type to another. If not values of a type can be converted, then another type class `TryInto` with a method `tryInto` is used. The `tryinto` method returns a `Result` type which indicates whether the conversion was successful or not. 

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


  ;; Literal values can also be matched on
  (define (is-5-or-7 x)
    (match x
      (5 True)
      (7 True)
      (_ False))))
```

Functions can pattern match on their arguments, but the patterns must be exhaustive.

```lisp
(coalton-toplevel
  (declare first (Tuple :a :b -> :a))
  (define (first (Tuple a _)) a)

  (declare second (Tuple :a :b -> :b))
  (define second (fn ((Tuple _ b)) b)))
```

The operator `coalton-library:if` can be used as a shorthand when matching on Booleans:

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

The Boolean operators `and` and `or` (of `coalton-library`) are actually variadic macros that short-circuit. Their functional counterparts are `boolean-and` and `boolean-or`.

```lisp
(coalton
  (or (cheap 5) True (really-expensive (expt 2 1000000))))
```

In this case, `really-expensive` will never get called due to short-circuiting. Also note that both `and` and `or` can take one or more arguments.


## `COALTON-LIBRARY:PROGN`

Coalton has a `coalton-library:progn` construct similar to lisp.

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

Flattened let expressions support pattern matching:

```lisp
(coalton-toplevel
  (declare f (Tuple Integer Integer -> Integer))
  (define (f t)
    (let (Tuple fst snd) = t)
    (+ fst snd)))

```

Flattened let expression are non recursive, and do not support let polymorphism. Thus the following is invalid:

```
(coalton
  (progn
    (let id = (fn (x) x))
    (id Unit)
    (id "hello")))
```

It does however work with standard let expressions:

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

The `coalton-library` package also includes `unless` and `when`, which work
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

## Typeclasses

Coalton supports typeclasses.

Currently, *all* member functions must be defined for each typeclass instance.

```lisp
(coalton-toplevel
  ;; Typeclasses are defined with the define-class keyword
  (define-class (Eq :a)
    (== (:a -> :a -> Boolean)))

  (define-type Color
    Red
    Green
    Blue)

  ;; Typeclass instances are defined with the define-instance keyword
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


## Builtin Typeclasses

The following are the main typeclasses defined in the standard library.

* `Eq` - defined on types that are comparable
* `Ord` - defined on types that are orderable
* `Num` - defined on types that are numeric

* `Semigroup` - defined on types which support an associative binary operation
* `Monoid` - defined on types that are semigroups and have an identity element


Each of the following typeclasses resembles the class of the same name in
Haskell, aside from meager differences.

* `Functor` - `fmap` is just `map` in Coalton
* `Applicative`
* `Monad` - monad does not have `return`, use `pure` from applicative instead
* `Alternative` - `<|>` is called `alt` in Coalton
* `Foldable`
* `Traversable`

These typeclasses are inspired by traits of the same name in Rust:

* `Into` - total conversions between one type and another
* `TryInto` - non-total conversions between one type and another

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

    ;; [6+3, 5+3, 5+3, 6+2, 5+2, 4+2, 6+1, 5+1, 4+1]
   (define xs (f (make-list 1 2 3) (make-list 4 5 6))))
```

## Inline Type Annotations

Inline type annotations can be added to resolve ambiguities when using typeclasses.

```lisp
(coalton-toplevel
  (define f (the U32 (+ (fromInt 5) (fromInt 7)))))
```

## Shorthand Function Syntax

Coalton does not have nullary functions. However, a function with the type signature `Unit -> *` can be called in Coalton without explicitly passing `Unit`. For instance, the Coalton form `(coalton-library/vector:new)` is a shorthand for `(coalton-library/vector:new Unit)`.

Functions can also be defined with an implicit parameter using `(fn () 5)`. This creates a function with a single implicit parameter of type `Unit`.

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

Coalton has a similar [type defaulting system](https://www.haskell.org/onlinereport/decls.html#sect4.3.4) as Haskell. Type defaulting is invoked on implicitly typed definitions and code compiled with the `coalton` macro. Defaulting is applied to a set of ambiguous predicates, with the goal to resolve an ambiguous type variable to a valid type. Coalton will only default if one or more of the predicates is a numeric type class (Num, Quantizable, Reciprocable, Complex, Remainder, Integral). Coalton will default an ambiguous variable to either Integer, Double-Float, or Single-Float; taking the first type that is valid for all predicates referencing that type variable. Coalton will not default when one or more of the predicates containing an ambiguous variable is a multi-parameter type class.


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

Specialization can only apply when the argument types at a callsite are known. Because specialization is not guaranteed, specialized functions must have the same behavior as their unspecialized variants. Specialization should only be used for performance. See the following example:

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
