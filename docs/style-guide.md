# Coalton Style Guide

This is the recommended style guide for Coalton programs. It also establishes the standard by which the built-in library functionality must abide.

## Naming

**Rule N1**: Predicates (i.e., functions that answer a question or test a property of a value that return a `Boolean`) and flags should end in `?`.

*Example*: The standard functions `even?` and `odd?` check whether a given `Integer` is even or odd.

*Example*: The hypothetical function `bit-to-boolean` does not need to end in a `?` since it is not testing a property of a value.

*Rationale*: This is a convention that Scheme follows. It makes code more readable.

***

**Rule N2**: Ordinary variables naming functions and values should be written in "kebab-case", and the letters should be lowercase.

*Example*: `equivalence-classes`, `remove-duplicates`, `graph-number-count`

*Rationale*: This is standard in almost all Lisp-like languages, including Common Lisp.

*Corollary*: Functions should not be written in camel-case.

*Corollary*: Functions should not be written using underscores.

***

**Rule F3**: Functions converting from something like `A` to something like `B` should be written with an arrow `->` as in `A->B`.

**Example**: `single-float->integer`

*Rationale*: This is a convention that Scheme follows. It makes code more readable.

*Corollary*: The OCaml `B_of_A` is not acceptable.

***

**Rule F4**: The `/` symbol should not be used as a separator. It is reserved as a division-like operator.

***

**Rule F5**: Functions which mutate their input should end in `!`.

*Example*: `hashtable-set!`

*Rationale*: Mutation is a very dangerous thing in a typed language. This is also a convention that Scheme uses.

***

**Rule F6**: Functions specific to a data structure should have the data structure's name prepended to the function, assuming these functions are part of a larger package.

*Example*: `hashtable-get`, `vector-last`

*Rationale*: Even though multiple data structures might have a common function (like `length`), these common functions may not be generalized or part of a type class.

***

**Rule F7**: Avoid function names with overloaded or easily misconstrued meaning.

*Rationale*: In many functional programming languages, lists typically have the most "convenient" set of function names, and hence they're used more often. If we avoid using overly general operators for overly specific data structures, we can partially avoid this issue.

***

**Rule F8**: Avoid naming functions by symbols if a reasonable, spelled-out name exists.

*Example*: `compose`

***

**Rule F9**: Use a symbol if (1) it is very well understood by a large collection of programmers in multiple programming languages, (2) if the symbol is an established mathematical one, or (3) it massivly simplifies idiomatic Coalton code.

*Example*: `*`, `>>=`

***

**Rule F10**: Variables containing lists should typically be written plural, i.e., with an `s`. Lists of abstract elements can use a pluralized letter (e.g., `xs` or `as`).

*Rationale*: This is an idiom of many functional programming languages, including Haskell.

***

**Rule F11**: When naming a new data type, write it in the source code as camel case.

*Example*: `HashTable`

*Rationale*: Lisp, by default, is not case sensitive to symbols without escaping. Nonetheless, the practice is useful when writing code, because it (1) promotes shorter type names and (2) distinguishes type-looking things from function-looking things.

***

**Rule F12**: Constructors should be written with an uppercase letter.

*Example*: `Cons`, `Nil`

*See Also*: Rule F11.

***

**Rule F13**: Internal or unsafe variables, functions, constructors, or types that may inadvertently be visible to the user, but should not be used by the user, should (1) be prepended with a `%` and (2) not be exported. 

*Example*: `%reduce-fraction`, `%Fraction`

*Rationale*: We want to make very clear that certain functions are not intended for general use.

***
