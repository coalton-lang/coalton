# Porting Coalton to a new Lisp environment

We consider what is required to port Coalton to a new Lisp
environment, that is, two necessary requirements, two desired
requirements, and a requirement for the library.

These are lessons learnt while working on the [compatibility layer
fork](https://github.com/c-kloukinas/coalton-compat/tree/compat-in-small-pieces),
which contains the final versions of what is mentioned below (these
are sometimes initial commits that have been modified later). Code
quality can most probably be improved substantially.

## The two *necessary* requirements for porting Coalton

### Hash type representation and hash functions

File `library/classes.lisp` defines the native representation of type
`Hash`, essentially as an `unsigned-byte` that can fit in a `fixnum`.

Then, file `library/hash.lisp` defines `lisp-combine-hashes` in an
environment-specific manner.

Finally, closely related to hashing is file
`library/math/num-defining-macros.lisp` that defines `+fixnum-bits+`,
which one could use to define the native representation of type
`Hash`.

An attempt at developing [portable definitions for hash-related
requirements is available
here](https://github.com/coalton-lang/coalton/commit/71542b2e0b9d7b43345f8fc75d89d82c91c2bf21)
- see functions `coalton-compatibility:get-fixnum-bits`
`coalton-compatibility:hash-combine`, and macro
`coalton-compatibility:get-hash-type`. Function
`coalton-compatibility:hash-combine` provides a portable
implementation for 32 and 64-bit fixnums, following the logic in the
code of the Boost container_hash C++ library.

### Unsetting float traps

File `library/set-float-traps.lisp` attempts to unset all
floating-point traps in the current Lisp environment (defined for sbcl
& ccl but *not* for allegro).

An [*initial* attempt at unsetting floating traps for sbcl, ccl, abcl,
ecl, and clasp is available
here](https://github.com/coalton-lang/coalton/commit/7b401de613b029a05554fabe641dacf829f32a21)
- see macro `coalton-compatibility:unset-all-float-traps`.

## The two *desired* requirements for porting Coalton

### Number of bytes consed

File `library/system.lisp` defines `monotonic-bytes-consed` that
attempts to compute the number of bytes consed since some unspecified
(but constant) time point. It's currently implemented only for sbcl,
returning `None` in the other environments.

An [implementation for abcl is defined
here](https://github.com/coalton-lang/coalton/commit/96252069c8755f083037d2ac65247d4d87fa23ea)
- ccl, ecl, and clasp don't seem to be able to support it, so in these
`None` is returned.

### Un/Locking packages

The code in Coalton makes heavy use of un/locking packages under sbcl,
even in the Lisp code that it produces when compiling Coalton source
code. This is done in almost all files.

An [initial implementation of un/locking packages for sbcl and ecl is
available
here](https://github.com/coalton-lang/coalton/commit/c9b30844d3257f0249fe318f02e07150faefd4b6). [Later
it was extended for
clasp](https://github.com/c-kloukinas/coalton-compat/tree/compat-in-small-pieces)
as well - no respective functionality was discovered for abcl or ccl.

Whether one wants to use this more portable package un/locking
functionality is up for debate - the code will work without it in
other environments right now, even when these support package
un/locking. Including it may help harden the code, though it'll
require changes to all files (and can cause crashes if the more
portable version is used in some files but the sbcl-specific version
is mistakenly used in others).

## Library TCE requirement

In order for the Coalton library to run correctly, the Lisp
environment needs to support tail-call elimination (TCE).
This is not a requirement for the core part of the Coalton language
but the library uses recursive definitions throughout.

Of the Lisp environments mentioned so far, only abcl does not offer
TCE. As such, abcl cannot use the Coalton library and a number of
tests will always fail in it. For example, ["inliner-tests-1" fails on
abcl](https://github.com/coalton-lang/coalton/commit/e4ff0d0605f780c5fd1e5ce7a8d35a5e245133bc)
and should probably be excluded in that environment.

## Extras

Some Lisp environments are more strict than others on how they
interpret the standard. For example, ecl insists that [a reader macro
returns zero or one
values](https://www.lispworks.com/documentation/HyperSpec/Body/02_b.htm),
and therefore requires that function
`coalton-impl/reader:read-coalton-toplevel-open-paren` in file
`src/reader.lisp` is wrapped in a `values`, since the functions it
calls can return two values. None of sbcl, ccl, allegro, abcl had an
issue with this.

- Coalton developers: We are unsure why the reader macro was returning
  two values - was the second value needed somewhere when compiling
  Coalton source code? If so, a work-around needs to be designed for
  ecl.


Abcl has an issue with how Fiasco defines test packages and [requires
a
patch](https://github.com/coalton-lang/coalton/commit/e980d703c05178d4e0e375bdfd4cf1b7cda62efc). This
patch had been submitted to the Fiasco maintainer but they haven't
merged it yet (unsure if the project is still maintained).

### Currently ongoing issues

While self-testing FSet, it turned out that it fails on abcl & clasp.

For abcl, the issue is one for which a solution is already in abcl's
repository but has not yet been included in the latest official
release (1.9.2 as of this writing), so [a patch for it has been
provided](https://github.com/coalton-lang/coalton/commit/d683d8037588d3fffb46e1bc1cb9c3fec7f6bb19).

For clasp, [the issue seems to be a clasp
bug](https://github.com/clasp-developers/clasp/issues/1731).

Whether any of these is important for Coalton to work correctly on
abcl/clasp has not been researched further - should be kept in mind
though if trying to complete the porting to one of these environments.
