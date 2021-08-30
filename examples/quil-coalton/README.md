# quil-coalton

This example shows a parser-combinator library as well as a parser for a subset of the [Quil Language](https://github.com/quil-lang/quil).

The currently supported Quil language features are:
- Simple gates
- Parametric gates
- Gate modifiers
- Measurement
- Reset
- Control Flow

## Usage

To run the parser, load `quil-coalton` and run

```lisp
(quil-coalton:run-quil-parser "<quil program>")
```

Example usage:

```lisp
QUIL-COALTON> (quil-coalton:run-quil-parser "H 0; CNOT 0 1")
#.(OK #.(QUILPROGRAM #.(CONS #.(QUILGATE #.(SIMPLE #.(SIMPLEGATE #.(NAME "H") #.(CONS #.(QUBIT 0) #.NIL)))) #.(CONS #.(QUILGATE #.(SIMPLE #.(SIMPLEGATE #.(NAME "CNOT") #.(CONS #.(QUBIT 0) #.(CONS #.(QUBIT 1) #.NIL))))) #.NIL))))
```
