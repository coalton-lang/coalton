# Internals

This document gives some notes about the internals of the Coalton system.

## Where it all begins

There are a few places where Coalton "starts".

### Reader

File: `src/reader.lisp`

We define our own reader to allow for portable and good error messages. There are pros and cons to this approach which we won't get into here.

The named readtable `coalton:coalton` is defined and sets `#\(` to invoke `read-coalton-toplevel-open-paren`. This function basically itself calls `maybe-read-coalton`.

The function `maybe-read-coalton` looks at a form like `(op a1 a2 ...)`. It checks if `op` is one of the Coalton toplevel operators (`coalton`, `coalton-toplevel`, `coalton-codegen`, etc.) and immediately proceeds to perform that action.

**It is important to note that in the current design of Coalton, code is processed at read-time!**

The actions performed are broken into two steps: parsing and compilation.

### Parsing entry point

File: `src/parser/toplevel.lisp`

The main entry point is `read-program`. This takes a stream which is expected to have Coalton code, parses it out, and produces a `program` object. Parsing itself is a "read-parse-fill" action:

1. Read: Read a form from the stream.
2. Parse: Parse it out with `parse-toplevel-form`.
3. Fill: Add it to the mutable `program` object.

The secondary entry point is `read-expression` for reading individual expressions. This is used to implement the `coalton` operator.

The parser takes great care in trying to produce very good user error messages.

### Compilation entry points

File: `src/entry.lisp`

At this point we will have a `program` object.

The main entry points for transforming a parsed AST into Common Lisp code. This includes the `entry-point` function and the `expression-entry-point` function.

## Compiler phases overview

The compiler is structured into three large phases:

1. Parsing (`src/parser/`)
2. Type checking (`src/typechecker/`)
3. Code generation (`src/codegen/`)

Each of these has its own distinct AST representations. In fact, a lot of the code in the respective modules are "re-definitions" of previous data structures for expressing ASTs.

## Parsing phase
### Intermediate representation for expressions

File: `src/parser/expression.lisp`

The IR of this phase is an AST. Each node is a sub-structure of `node`.

Parsing of expressions also occurs in this file.

Some constructions are de-sugared directly in parsing. For example, there is no `node-rec`; it is desugared into a `node-let` immediately.

### Intermediate representation for types

File: `src/parser/types.lisp`

The IR for types is a relatively straightforward data structure, which are substructures of `ty`. This file also contains the parser for said data structure

### Transformations of the AST

File: `src/parser/renamer.lisp`

There are two functions, `rename-variables` and `rename-type-variables`, which are used by the compilation entry point.

## Type checking phase
### Intermediate representation for expressions

File: `src/typechecker/expression.lisp`

This mostly mirrors the parser AST but it includes type information.

### Intermediate representation for expressions

File: `src/typechecker/types.lisp`

This mostly mirrors the parser AST.

### Coalton-Lisp type correspondence

File: `src/typechecker/lisp-type.lisp`

The function `lisp-type` provides the Lisp type representation of a Coalton type. Special handling of `LispArray` and `Complex` happen here.

### AST traversal

File: `src/typechecker/traverse.lisp`

The type checker has its own AST traversal logic, which is different than the codegen one. The traversal logic is based on a `traverse-block` struct which contains a giant table of functions that say how every AST node should be transformed.

### Type checking

Type checking relies mainly on:

- substitution via `apply-substitution`, defined in `src/typechecker/substitutions.lisp` and `src/typechecker/expression.lisp`
- unification via `mgu` and `unify`, defined in `src/typechecker/unify.lisp`
- inference via `infer-expression-type`, defined in `src/typechecker/define.lisp`

There are other modules to handle more specific things, but the bulk of the "type calculus" is defined above.

### Environment

File: `src/typechecker/environment.lisp`

Information about all of the discovered types and the like are defined in this file. The `environment` structure contains everything.

## Code generation phase

Code generation is responsible for both optimization and code generation. It contains:

1. The monomorphizer (`src/codegen/monomorphize.lisp`)
2. The specializer (`src/codegen/specializer.lisp`)
3. The inliner (`src/codegen/inliner.lisp`)
4. The constant propagator (`src/codegen/constant-propagation.lisp`)

The entry point to optimization is `src/codegen/optimizer.lisp`.

Actual code generation is handled in the respective files: `codegen-*.lisp`. Of special note is `intrinsic-applications.lisp` which handles "special" functions like `inline`.
