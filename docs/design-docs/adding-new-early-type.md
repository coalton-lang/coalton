# Adding a New Early Type

An "early type" (for the purposes of this document) is a Coalton type
that's built-in to the Coalton system before the standard library is
loaded. Typically these are core types that have a native Lisp
representation.

## Do I need a new early type?

Ask yourself the question, "Do I need an early type?" It's likely you
can get away with using a `Lisp-Object` in Coalton to store your Lisp
object and manipulate it. Early types are good when you need a very
specific object representation that would be too inefficient locked
inside of a `LISP-OBJECT` structure.


## Create a new symbol for your type

*File*: `package.lisp`

First, create a new symbol in the `COALTON` package for your
type. Search for the string "Early Types".


## Create Coalton type representations

*File*: `typechecker/types.lisp`

Add a Coalton type resentation. Search for "Early Types" and create a
tcon for your type.


## Add type to the default environment

*File*: `typechecker/environment.lisp`

Add these types to the default environment. Search for "Early Types"
and add an entry.


## Tell Lisp type system about it

*File*: `codegen/lisp-types.lisp`

Now we tell Coalton how to generate a type check for this Coalton
type. Search for "Early Types" and add it.


## Tell Lisp about its literal values

*File*: `ast/node.lisp`

Add the Lisp type that has literal values for your object to the
`literal-value` type.


## Tell Lisp what type the literal values are

*File*: `typechecker/derive-type.lisp`

In the function `derive-literal-type`, state what type the literal
should derive as. Currently there's no support for overlapping,
overloaded, or ambiguous literals.
