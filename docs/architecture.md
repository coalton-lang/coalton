# Coalton Compiler Architecture

This document aims to describe the architecture of the Coalton compiler. It is split in to three main systems: the frontend, which is responsible for parsing Coalton forms; the typechecker, which is responsible for deriving the types of Coalton values and expressions; and the code generator, which is responsible for converting type-checked Coalton programs into the corresponding Common Lisp forms.

## Frontend

The frontend is responsible for parsing input Coalton forms and calling out to the [typechecker](#typechecker) and [codegen](#code-generator) systems.

The main entry point in to the Coalton compiler is through either the `coalton-toplevel` or `coalton` macros. `coalton-toplevel` handles most of the Coalton langugage and allows for `declare`, `define`, `define-type` (and associated `repr`), `define-class`, and `define-instance` forms. `coalton` acts as a quick way to evaluate Coalton expressions within Common Lisp. `coalton` does not allow for any of the top-level forms used in `coalton-toplevel`.

### `coalton-toplevel`

The work of `coalton-toplevel` can be summarized as follows:

1. Collect all top-level forms (`declare`, `define`, `define-type`, `define-class`, `define-instance`, and `repr`) and sort corresponding lists
2. Process these forms with the [typechecker](#typechecker)*
   a. Process type definitions
   b. Process class definitions
   c. Predeclare instance definitions
   d. Process value definitions
   e. Process instance definitions
3. Process the resulting values and perform [codegen](#code-generator)
   a. Produce [environment](#environment) diff and update code
   b. Call in to `codegen-program`
4. Update the global [environment](#environment)

*Note: This order ensures that forms are able to reference their dependencies. e.g. class definitions can reference types declared in the same `coalton-toplevel`. Within each of these steps, dependencies are resolved and forms are topologically sorted to ensure all referenced values are available at type check time.

### `coalton`

The `coalton` macro is quite a bit simpler because it does not allow the user to define values, types, classes, or instances, and instead simply generates the corresponding code for a Coalton expression.

The `coalton` macro does the following:
1. Parse the input form into the `node` ast representation
2. Type check the `node` ast, producing a `typed-node` ast representation
3. Produce the corresponding lisp code using `compile-expression`
4. Update the global [environment](#environment)

## Typechecker

TODO

## Code Generator

TODO

## Internals

This section describes some of the compiler internals which are useful to understanding the general flow of the compiler.

### Environment

The `environment` structure acts as the primary way the Coalton compiler keeps track of definitions within the current context. Inside the `environment` data structure there are sub-structures for the value, type, constructor, class, instance, function, and name environments. 
The code for this lives in the `src/typechecker/environment.lisp` file.

The `environment` structure is an immutable data structure, meaning that for any changes to the environment to take place, the current environment needs to be copied and replaced. This ensures that the environment is not modified by called subroutines throughout the compiler.

The current environment can be inspected using the `print-*-db` functions available in the `src/debug.lisp` source file.
