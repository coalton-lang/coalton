# Implementing tests in Coalton

Letting Coalton run loose in a Fiasco test-suite can cause a bit of havoc, but, with a little finesse, Coalton and Fiasco can interact safely and smoothly.

## An Example Coalton Package:
```
(defpackage #:foobar
  (:use #:coalton
        #:coalton-prelude)
  (:export #:add-twenty
           #:baz-it))

(in-package #:foobar)

(coalton-toplevel
  (define (add-twenty x)
    (+ x 20))
    
  (define (baz-it string)
    (coalton-library/string:concat "baz-" string)))
```
## Writing a test suite

When writing a test suite for a Coalton project, you'll actually make two separate packages: a test package written using native Coalton, and a Common Lisp/Fiasco test package which will render your Coalton tests Fiasco-compatible. The Fiasco test package also accommodates tests of its own, allowing the test suite to have tests in both Coalton and Common Lisp.

### Step 1: Define a Fiasco test package
```
(fiasco:define-test-package #:foobar-tests
  (:use #:cl))

```
### Step 2: Define a Coalton test package

Defining a Coalton test package requires `#:coalton-testing`, a package which facilitates Coalton-Fiasco compatibility.

In order to use `#:coalton-testing`, either run `(ql:quickload :coalton/testing)` or add `#:coalton/testing` to your project's .asd file.
```
(defpackage #:foobar-native-tests
  (:use #:coalton-testing #:foobar))
```
### Step 3: Initialize Fiasco

Within the Coalton test package, initialize Fiasco compatibiltiy by calling `#'coalton-fiasco-init` with your Fiasco test package.
```
(in-package #:foobar-native-tests)

(coalton-fiasco-init #:foobar-tests)
```

### Step 4: Write a test suite

```
(in-package #:foobar-native-tests)

(define-test foobarbaz-tests ()
  (is (== (add-twenty 1) 21))
  (is (== (foobar::baz-it "squatch") "baz-squatch")))
```
### Step 5: Run the test suite

As with standard Fiasco test suites, tests can be easily run from the REPL.

While in the Fiasco test package for the project (in this case `#:foobar-tests`), run `(run-package-tests)` to run all compiled tests.

```
FOOBAR> (in-package #:foobar-tests)
#<PACKAGE "FOOBAR-TESTS">
FOOBAR-TESTS> (run-package-tests)
FOOBAR-TESTS (Suite)
  FOOBARBAZ-TESTS.........................................................[ OK ]

T
(#<test-run of FOOBAR-TESTS: 2 tests, 2 assertions, 0 failures in 8.3e-5 sec>)
```