# Test Case Input Format

The files in this directory contain test cases in a structured text
format.

A test consists of a header section, a program section, and an
optional error section, separated by rows of '=' and '-'
characters.

Section separators contain any non-zero number of ='s or -'s starting
at a newline.

The header may contain a test number (the first integer to appear
anywhere in the header).

The header may also contain a set of flags, expressed as a readable
list of Lisp keywords.

Supported flags are:

- :disable Disable a test case

## Examples

### Example 1

The structure of a test.

```
============================================================
{NUMBER} HEADER (:FLAG :FLAG2)
============================================================

PROGRAM BODY

------------------------------------------------------------

ERROR MESSAGE
```

### Example 2

A test that checks that a program parses and compiles.

```
================================================================================
Simple Test Case
================================================================================

(package my-test-package
  (import coalton-prelude))

(declare fib (Integer -> Integer))
(define (fib n)
  "Compute the nth Fibonacci number"
  (match n
    (0 0)
    (1 1)
    (_ (+ (fib (1- n))
          (fib (- n 2))))))
```

### Example 3

A numbered, disabled test that checks that an invalid return
statement is detected.

```
================================================================================
101 return, trailing junk       (:disable)
================================================================================

(package test-package)

(define x (return y z))

--------------------------------------------------------------------------------

error: Malformed return expression
  --> test:3:20
   |
 3 |  (define x (return y z))
   |                      ^ unexpected trailing form
```
