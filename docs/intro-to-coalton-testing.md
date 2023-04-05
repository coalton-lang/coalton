# Testing Your Coalton Code

By now, hopefully, you've read the [Whirlwind Tour of Coalton](intro-to-coalton.md) and
written some non-trivial Coalton code. Now the question is, "how do I know it works?"

The fact that your code type-checks and compiles should assure you that it's free from a
large class of bugs, but that doesn't necessarily mean that it does what you expect. If
you're worried about whether you've got the logic right, it's time to write a test suite!

## Adding a Test System to Your System Definition

For a project `foo`, you should already have a system definition file `foo.asd` located in
the project's root directory, which looks something like:

```
(defsystem "foo"
  :depends-on ("coalton" "named-readtables" ...other dependencies?)
  :components ((:file "main")
               ...other components?))
```

Don't worry if your system looks a little different. It might have `:serial t`, and it
might specify an `:author` or a `:license`, it might have a `:pathname`, and if you're
really cool it might have `:class :package-inferred-system`.

To that `defsystem` form, add the following line:

```
  :in-order-to ((test-op (test-op "foo/test")))
```

(Replace `foo` with your actual system name, obviously.)

This tells ASDF, "when you want to run the `test-op` operation on this system, run
`test-op` on this other system `"foo/test"`."

That's great, but as yet the `"foo/test"` system doesn't exist. Add to the `.asd` file a
new `defsystem` form like:

```
(defsystem "foo/test"
  :depends-on ("foo" "coalton/testing")
  :pathname "test/"
  :serial t
  :components ((:file "test"))
  :perform (test-op (o s)
             (symbol-call '#:foo/test '#:run-tests)))
```

TODO: do we want to tell users to explicitly depend on `fiasco`, the same way we tell them
      to explicitly depend on `named-readtables`?

(Again, replace `foo` with your actual system name.)

Note that the name of your testing subsystem must start with your full system's name, with
some suffix after a `/`. This tells ASDF that it's a subsystem, and so to search for its
definition in `foo.asd`, rather than trying to find a file `foo/test.asd`.

The `:pathname "test/"` says that the components of the subsystem will be in the `test/`
subdirectory, which we'll create in a moment.

`:components ((:file "test"))` says that this subsystem contains a single file,
`test.lisp`. We'll create it in a moment.

`:perform (test-op (o s) (symbol-call '#:foo/test '#:run-tests))` tells ASDF, "when you
want to run the `test-op` operation on this subsystem, look up a symbol named `RUN-TESTS`
in the package `FOO/TEST`, and call that function." This means that we'll need to define a
package named `FOO/TEST`, and it will need to export a symbol `FOO/TEST:RUN-TESTS` which
names a function that runs our test suite.

The `:perform` clause actually defines a function to handle the operation, and `o` and `s`
are arguments bound to the operation (in this case an instance of `asdf:test-op`) and the
system (in this case, an instance of `asdf:system` which will be equal to
`(asdf:find-system "foo/test")`.) We don't actually need these arguments, so we'll just
leave them unused.

## Making Files

Create a subdirectory `test` within your project's root directory. Inside it, create a
file `test.lisp` to hold your tests. (If you used a different `:pathname` or `:file` in
your system definition, use those names instead.)

In `test.lisp`, put the following forms:

```
(defpackage #:foo/test
  (:use #:coalton #:coalton-prelude #:coalton-testing)
  (:export #:run-tests))
(in-package #:foo/test)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:foo/fiasco-test-package)

(coalton-fiasco-init #:foo/fiasco-test-package)

(cl:defun run-tests ()
  (fiasco:run-package-tests
   :packages '(#:foo/fiasco-test-package)
   :interactive cl:t))
```

The `defpackage`, `in-package` and `named-readtables:in-readtable` forms should all be
familiar to you. Add any clauses to your `defpackage` that you need to reference your
system, like `(:local-nicknames (#:f #:foo))` or `(:use #:foo)`. Also add any
`:local-nicknames` for `coalton-library` packages you want.

We've added one new package to our `:use` list, `#:coalton-testing`. It defines the
utilities you need for defining tests and making assertions within them.

Coalton uses [Fiasco](https://github.com/joaotavora/fiasco) for its testing
framework. Fiasco groups tests into packages, so we need to define a package for our
tests, but we don't actually want to write our code in that package: it's intended for
Common Lisp, so it `:use`s `#:cl` and `#:fiasco`, but we'd much prefer to write our tests
in Coalton. So we define a Fiasco test package named `FOO/FIASCO-TEST-PACKAGE`, and use
`coalton-fiasco-init` to arrange for defining Coalton tests which Fiasco can find in that
package.

We told ASDF to run our tests by calling the `run-tests` function, so we define
`run-tests`. It just tells Fiasco to run our test suite.

## Defining Tests

Now that all that's done, we can finally get to the good part. Define a test with
`define-test`, and inside it, make some assertions with `is` or `matches`.

### `define-test`

`define-test` is a macro that was generated by the `coalton-fiasco-init` form you wrote
earlier. (This is an implementation detail; try not to think too much about it.) For your
very first `define-test`, try writing:

```
(define-test my-first-test ()
  (is True))
```

`my-first-test` is the name of your test. The empty list `()` is its argument list. I
(Phoebe) am not really sure why Fiasco tests take arguments. `coalton-testing` doesn't
support doing anything useful with them, so leave the arglist empty. `(is True)` is an
assertion that always succeeds.

### Boolean assertions with `is`

The `is` macro asserts that its first argument evaluates to true. `(is True)` will always
succeed, and `(is False)` will always fail.

If you have a function `always-returns-zero` in your package `foo`, and you want to make
sure it returns `0`, you might write:

```
(define-test test-always-returns-zero ()
  (is (== 0 (foo:always-returns-zero))))
```

When calling boolean predicates in an `is` form, like `==`, prefer writing the expected
value as the first operand, and the computed value as the second.

TODO: `coalton-testing` doesn't actually privilege that argument order the way FiveAM and
      (I assume) Fiasco do. Should we recommend it anyway? I still think it's a nice
      style, personally.

`is` optionally takes a second argument, a string used to describe the assertion if it
fails. For example, you might write:

```
(is (== 0 (foo:always-returns-zero))
    "ALWAYS-RETURNS-ZERO returned a non-zero value!")
```

### Pattern assertions with `matches`

The `matches` macro takes two required arguments, a pattern and an expression, and asserts
that the result of the expression matches the pattern. `(matches _ ANYTHING)` will always
succeed (assuming `anything` compiles and executes without error).

If you have a function `one-element-list` which wraps its argument in a one-element list
in your package `foo`, you might write:

```
(define-test test-one-element-list ()
  (matches (Cons _ (Nil))
           (foo:one-element-list 0)))
```

Note that this doesn't assert anything about the contents of the list returned my
`(foo:one-element-list 0)`, only its structure. If `(foo:one-element-list 0)` returns the
list `(Cons 0 (Nil))`, the test will pass just the same as if it returned `(Cons "zero"
(Nil))`.

Like `is`, `matches` takes an optional string argument used to describe the assertion if
it fails. You might write:

```
(matches (Cons _ (Nil))
         (foo:one-element-list 0)
         "ONE-ELEMENT-LIST returned a list with a length other than 1!")
```

### Uses for `(is False)`

Often, it's useful to invoke a function, destructure its return with `match`, and to run
additional assertions in some branches while treating the other branches as failures. In
this case, it's convenient to write an `(is False)` assertion with a message describing
the branch incorrectly taken in each of the failed branches. For example, if our
`one-element-list` function from above is supposed to wrap specifically its argument in a
list, we'll want to add an `==` assertion. We might write:

```
(define-test test-one-element-list ()
  (match (one-element-list 0)
    ((Nil) (is False "ONE-ELEMENT-LIST returned an empty list!"))
    ((Cons elt (Nil)) (is (== 0 elt)))
    ((Cons _ _) (is False "ONE-ELEMENT-LIST returned a list with more than 1 element!"))))
```

## Running your tests

Now that you've defined a test system and some tests to go in it, you can run your tests
in the REPL using `(asdf:test-system "foo")`.
