# The Coalton Standard Library Style Guide

Many parts of the Coalton Standard Library currently violate these guidelines. Please,
submit PRs bringing them into line!

## Naming

### Functions and other terms

Term-level variables, i.e. those bound by `define`, `let` and `fn`, should be named in
`kebab-case`. An exception is made for constructors, as bound by `define-type`.

Prefer longer, more expressive names. For example, `push-front` rather than just
`push`.

Use full words instead of abbreviations or acronyms. For example, `string-length`, not
`strlen`.

Exceptions are allowed for incredibly common and intuitive operators like `fn`.

#### Side-effecting operators

Operators which perform observable side effects, like altering the value contained in a
cell or writing to a stream, should be named with a trailing `!`, like `write!`.

#### Predicates

Predicates, i.e. non-side-effecting functions which return a boolean, should be named with
a trailing `?`, like `empty?` or `null?`.

#### Side-effecting predicates

Predicates with side effects are rare, and generally should be avoided, but are in some
cases necessary. For example, the `Iterator` iterface defines some operators which consume
an iterator and return a boolean depending on its contents.

These functions should be named with a trailing `!?`, like `any!?` or `some!?`. This makes
code sound very funny.

#### Conversion functions

Many Common Lisp libraries obey the convention that a function which converts a value of
type `foo` into `bar` should be named `foo-bar`, like `graph-list` or
`string-rope`. Others replace the hyphen `-` with an arrow `->`, like `graph->list` or
`string->rope`. Still others reverse the order and use a left arrow `<-`, like
`graph<-list` or `rope<-string`.

All of these conventions are symptoms of the fact that Common Lisp's facility for type
conversions, `coerce`, is not extensible to user-defined types. Coalton code has no need
to define conversion functions; it should instead define instances on `Into` or `TryInto`
as appropriate.

### Types, classes and constructors

Type names, class names, and constructor names, i.e. those defined by `define-type` and
`define-class`, should be `UpperCamelCase`. There is ongoing discussion as to whether
Coalton should be case-sensitive, but this is at least stylistically pleasant.

Single-constructor types whose members may be considered public should use the same name
for their type and their constructor.

Single-constructor types whose members may be considered private (i.e. those which should
not be directly constructed or destructured by user code) should name their constructor by
prefixing their type name with a `%`.

For example:

```
(define-type TwoVariants
  FirstVariant
  (SecondVariant Foo))

(define-type OneVariant
  (OneVariant Foo))

(define-type PrivateBody
  (%PrivateBody Lisp-Object))
```

### Type variables

Type variables should be `kebab-case` keywords. Whenever possible, a meaningful name
should be chosen based on the qualities or uses of the type, rather than using a
single-letter name. Unlike for term variables, reasonable abbreviations are permitted. For
example, the type parameter to `Iterator` is `:elt` (short for "element"), not `:a`.

## Parameter order

The order of parameters to functions should be considered in order to support currying.

Generally (although with some exceptions), parameters should be ordered:

1. Functions, such as the first argument to `map`, `liftA2` or `fold`.

2. Collections, such as the second argument to `map`, or the first argument to `index`.

3. Individual items, such as the new value in `cell-write!` or `insert`, or the needle in
   `contains?`.
