# To run Coalton benchmarks:

- `(ql:quickload :coalton/benchmarks)` or `(asdf:load-system :coalton/benchmarks)`

- `(in-package #:coalton-benchmarks)`

- `(run-benchmarks)` runs all the benchmarks

- `(run-benchmark :BENCHMARK-PACKAGE)` runs the named benchmark package.

# To add a new benchmark

## Add `define-coalton-benchmark` form in `package.lisp`

```
(define-coalton-benchmark mybenchmark
   (<extra-package-clause> ...)
  <native-package-clause>
  ...)
```

This expands into two package definitions as follows.

```
(defpackage :coalton-benchmark-mybenchmark/native
  <native-package-clause> ...)

(define-benchmark-package :coalton-benchmark-mybenchmark
  (:local-nicknames (#:native :coalton-benchmark-mybenchmark/native)
  <extra-package-clause> ...)
```

The idea is to put coalton code into the `/native` package, and
benchmarking code that calls the coalton code (`define-benchmark`
form etc.) in the package without `/native`.

## Add benchmark source files

```
(cl:in-package :coalton-benchmark-mybenchmark)

(define-benchmark ...)

;; here, you can refer to Coalton procedures with `native:` prefix


(cl:in-package :coalton-benchmark-mybenchmark/native

(coalton-toplevel
  ...
  )

```

## Add the benchmark source files in `coalton.asd`
