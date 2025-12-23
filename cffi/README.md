# Coalton CFFI

A library for interfacing with C foreign functions in Coalton.

## Summary

This directory contains the package definitions that comprise the
Coalton CFFI. The essential functionality is provided by a single
package, `#:coalton-cffi`, which includes the infrastructure to
utilize foreign structs, enums, and functions and leverage Coalton's
static type analysis.

## Overview

### Aliases

Type aliases are provided for mapping foreign types to the primitive
Coalton types.

New type aliases for integer types can be defined as in the following
example, where `:int` is a type known to the Common Lisp CFFI.

```lisp
(define-foreign-integer-alias Int :int :signed)
```

### Types (`ForeignRepr`)

Every type that has a foreign representation can define an instance of
`ForeignRepr`. This is a type class for types that are known to the
Common Lisp CFFI and have a known size. New instances of this class
can be conveniently defined using a macro, as in the following
example.

```lisp
(define-foreign-repr-instance Int :int)
```

### Pointers

A `Pointer` type is provided, parametrized on an element type. New
pointers can be allocated using the following generic functions.

```lisp
;; Allocate memory by specifying a byte count.
(foreign-alloc-raw count)

;; Allocate memory by specifying a number of elements and an initial element.
(foreign-alloc n x)

;; Allocate memory by specifiying a number of elements only.
(foreign-alloc-uninitialized n)
```

Pointers can be freed using `foreign-free`, shifted using
`shift-pointer` and `mem-aptr`, and reinterpreted using `unsafe-reinterpret`.

The generic functions, `mem-cpy-raw`, `mem-cpy`, and `mem-acpy`, can
be used to copy bytes from one pointer to another.

### Types (`SimpleForeignRepr`)

For simple types, pointers of which can be directly dereferenced,
there is a type class called `SimpleForeignRepr`. This class provides
methods for getting and setting bytes from a pointer offset by a
number of bytes or by an element index. These are `mem-ref`,
`mem-set!`, `mem-aref`, and `mem-aset!`. Complex numbers are also
supported with instances of `SimpleForeignRepr`.

### Boxes

For encapsulating pointers (and other data) in heap-allocated
structures, the `Box` and `BoxedPointer` types are
provided. Encapsulating simple data in heap-allocated structures
allows for reliable integration with the garbage collector using
finalizers.

### Foreign Libraries

Foreign libraries are represented using `ForeignLibrary` and defined
using the macro, `define-foreign-library`, which is a wrapper for
`cffi:define-foreign-library`. These libraries can be loaded and
unloaded using `load-foreign-library` and `unload-foreign-library`,
respectively.

### Foreign Structs

The interface to foreign structs uses a special transparent type for
representing the slots of a struct. The type contains information
about the type of the slot and about the type of the struct to which
it belongs. This allows for all struct access and modification to be
done using generic functions.

When a slot contains one element, use `slot-pointer`, `slot-ref`, and
`slot-set!`, and when a slot contains multiple elements, use
`slot-aptr`, `slot-aref`, and `slot-aset!`.

Structs are defined using a macro, as in the following example.

```lisp
;; Define a struct called `S` with a "conc" name, "s.".
(define-foreign-struct (S "s.")
  "A struct called \"S\"."
  (x 10 Int))

;; Instantiate the struct and populate the third element of the x slot.
(coalton
  (define s
    (let ((s (foreign-alloc-uninitialized 1))
          (value 15))
      (slot-aset! s s.x 2 value)
      s)))

;; Read the third element of the x slot.
(coalton (slot-aref s s.x 2))
```

### Foreign Functions

Foreign functions are defined using a macro similar to `cffi:defcfun`,
except types are provided as Coalton types.

```lisp
(define-foreign-function (memcpy "memcpy") (Pointer Void)
  "Copy `n` bytes from `src` into `dest` and return `dest`."
  (dest (Pointer Void))
  (src  (Pointer Void))
  (n    Size))
```

### Foreign Enums

Foreign enums are defined using a macro as in the following example.

```lisp
(define-foreign-enum T
  A
  B)
```

Foreign enums are defined as Coalton ADTs, so the enum members can be
used as patterns in the branches of `match` expressions.

The base type of the enum can be specified, as well as the indices of
the members.

```lisp
(define-foreign-enum (T Int)
  (A 3)
  (B 10))
```
