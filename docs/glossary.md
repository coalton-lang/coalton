# Glossary: A Comparison of Lisp and Coalton Naming

*Note: This is a pedagogical document intended to help clarify the
naming of things. Comparisons between the two languages are almost
always approximate. This document is also not exhaustive.*

## Differences in Naming Conventions

In Lisp, almost everything is defined with lowercase
`kebab-case`. This includes variables, functions, classes, types, etc.

In Coalton, by convention, types are written in `CamelCase` and
values/functions are `kebab-case`.

Predicates (and sometimes boolean variables) in Common Lisp are
denoted with a trailing `p` or `-p`, while in Coalton they're
indicated by a trailing `?`.

Functions that mutate in Common Lisp are *sometimes* prefixed by
the letter `n` (e.g., `union` vs. `nunion`, but compare to the
mutating `sort`). In Coalton, such functions are indicated by a
trailing `!`, as in Scheme.

Broadly speaking, Coalton borrows a lot of naming conventions from
Haskell, since Coalton's type system is most similar to Haskell's.

## Differences in Naming of Basic Language Constructs

There is no direct equivalent to Lisp's `let` in Coalton. Coalton's
`let` is closer to Haskell's `let` or Scheme's `letrec`. All `let`
bindings are assumed mutually recursive.

Other differences are outlined in this table:

| Lisp               | Coalton    |
|:-------------------|:-----------|
| `defun`            | `define`   |
| `lambda`           | `fn`       |
| `handler-bind`     | `catch`    |
| `error`, `signal`  | `throw`    |
| condition, error   | exception  |
| restart            | resumption |

Coalton has a notion of records which it calls "structures". They are
unrelated to the Common Lisp `structure-class` or `defstruct` but play
a similar role.

## Differences in Naming of Basic Values

| Lisp               | Coalton |
|:-------------------|:--------|
| `t`                | `True`  |
| `nil` (boolean)    | `False` |
| `nil` (empty list) | `Nil`   |
| `nil` (absence)    | `None`  |

## Differences in Naming of Basic Functions

Equality in Coalton is done with `==` and is specific to each
type. The closest equivalent in Common Lisp would be the relatively
flexible `equalp`.

Conversion and casting is done in Coalton with `into` or
`tryinto`. The closest equivalent in Common Lisp is `coerce`.

Numerical functions are mostly similar. There are some notable
exceptions:

| Lisp    | Coalton          |
|:--------|:-----------------|
| `(- x)` | `(negate x)`     |
| `(/ x)` | `(reciprocal x)` |
| `ash`   | `lsh`, `rsh`     |
| `expt`  | `^`, `^^`, `pow` |

Some numerical predicates are named differently:

| Lisp     | Coalton     |
|:---------|:------------|
| `plusp`  | `positive?` |
| `zerop`  | `zero?`     |
| `minusp` | `negative?` |
| `oddp`   | `odd?`      |
| `evenp`  | `even?`     |

Coalton also has `nonpositive?`, `nonzero?`, and `nonnegative?`.

Other miscellaneous functions:

| Lisp         | Coalton              |
|:-------------|:---------------------|
| `constantly` | `const`              |
| `identity`   | `id`                 |
| `eq`         | `unsafe-pointer-eq?` |


## Differences in Naming of Basic Types

Common Lisp does not have a named recursive list type. `cl:list` is
equivalent to `(cl:or cl:null cl:cons)`. Coalton's `List` type is a
recursive, non-circular, homogeneous list.

Other differences are outlined in this table:

| Lisp           | Coalton |
|:---------------|:--------|
| `single-float` | `F32`   |
| `double-float` | `F64`   |
| `fixnum`       | `IFix`  |
| `character`    | `Char`  |

## "Type"

In Lisp, a "type" is a description of a set of objects that can be
checked with `typep`. The grammar for types is complicated and
expansive. It includes simple types (like `integer`), supertypes (like
`float`), compound types (like `(cons t1 t2)`), and many others.

In Coalton, a "type" is more limited and structured. Types in Coalton
are built out of simple types (like `Integer`), type constructors
(like `List` which takes one argument), type variables (like `List
:t`), and constraints (like `Num :t => List :t`).

In Lisp, types defined with `cl:deftype` are essentially just macros
or aliases over existing types. They don't manifest into existence a
new kind of object. In order to create new kinds of objects,
`defclass` or `defstruct` are needed.

In Coalton, types are defined with `define-type` or `define-struct`:

- `define-type` (almost always) defines an algebraic data type. The
closest equivalent in Lisp would be an abstract base class with a
finite number of subclasses.

- `define-struct` defines something like a record: a named type with
multiple fields. The closest equivalent in Lisp would be a simple
`defclass` or `defstruct` with virtually no options added.

## "Class"

In Lisp, a "class" is effectively a kind of data type, usually defined
by `defclass`. Classes in Lisp contain data and have a symbolic name.

In Coalton, a "class" usually refers to a "type class", defined by
`define-class`. A type class is sort of like an interface: types *are
an instance of*, *are members of*, *participate in*, *adhere to*,
etc. a type class. (A type class can also represent a constraint on a
type variable. The type `Eq :t => List :t` represents homogeneous
lists whose elements are of a type constrained by the `Eq` type class,
that is, they can be compared for equality with `==`.)

Lisp doesn't really have an equivalent of a type class. The closest
analog would be the idea of a "protocol", where we define a set of
generic functions that we opt a class into. For example, the type class

```
(define-class (Stringable :t)
  (from-string (String -> :t))
  (to-string (:t -> String)))
```

might be implemented by a single generic function protocol:

```
(defgeneric from-string (result-type str))
(defgeneric to-string (obj))
```

## "Instance"

In Lisp, an "instance" refers to an object of a class. We make new
objects usually with `make-instance`.

In Coalton, an "instance" is a declaration that a type participates in
a type class. That type is an "instance" of the type class.

Continuing the example above, we could make `Integer` an instance of `Stringable`:

```
(define-instance (Stringable Integer)
  (define (from-string s)
    (lisp Integer (s)
      (cl:parse-integer s)))
  (define (to-integer i)
    (lisp String (i)
      (cl:prin1-to-string i))))
```
