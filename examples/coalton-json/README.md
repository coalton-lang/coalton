# coalton-json

This library is an example of a real-world binding to a Common Lisp (non-Coalton) library. It's intended to be simple but non-trivial. We bind to the [`JSON-STREAMS`)(https://github.com/rotatef/json-streams) library.

The way we do this is as follows:

1. We define a Coalton data structure to represent JSON, called `Json`. The `Json` type is a simple algebraic data type representing JSON in a type-safe manner.
2. We write a Common Lisp function that uses the `JSON-STREAMS` library to parse JSON strings, building up a data structure out of `Json` constructors. Note that we are calling these constructors as ordinary Common Lisp functions!
3. We create a Coalton entry point called `coalton-json:parse-json` of type `(String -> Json)` which calls the aforementioned Common Lisp function.

Note that this approach guarantees a type-safe _interface_ to `JSON-STREAMS`, but it's still entirely possible that type errors exist within the Common Lisp function. With that said, in ordinary `safety` and `debug` compiler policies, Coalton inserts Common Lisp type assertions in the constructors, so the programmer can still enjoy accurate run-time type errors.


Example usage:

```lisp
CL-USER> (in-package #:coalton-json)
COALTON-JSON> (coalton (parse-json "[]"))
#.(JSON-ARRAY #.NIL)
COALTON-JSON> (coalton (parse-json "[[]]"))
#.(JSON-ARRAY #.(CONS #.(JSON-ARRAY #.NIL) #.NIL))
COALTON-JSON> (coalton (parse-json "5"))
#.(JSON-NUMBER 5.0d0)
COALTON-JSON> (coalton (parse-json "\"x\""))
#.(JSON-STRING "x")
COALTON-JSON> (coalton (parse-json "[true, 1.0, [[]]]"))
#.(JSON-ARRAY #.(CONS #.(JSON-BOOLEAN #.TRUE) #.(CONS #.(JSON-NUMBER 1.0d0) #.(CONS #.(JSON-ARRAY #.(CONS #.(JSON-ARRAY #.NIL) #.NIL)) #.NIL))))
COALTON-JSON> (coalton (parse-json "{\"a\": {}, \"b\": null}"))
#.(JSON-OBJECT #.(SM-ALIST #.(CONS #.(TUPLE "b" #.JSON-NULL) #.(CONS #.(TUPLE "a" #.(JSON-OBJECT #.(SM-ALIST #.NIL))) #.NIL))))
```

Note that there are *no* opaque `Lisp-Object`s; everything consists of completely native Coalton objects.
