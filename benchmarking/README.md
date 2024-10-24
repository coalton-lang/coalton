# Run the Coalton benchmark suite:

`(ql:quickload :coalton/benchmarks)` or `(asdf:load-system :coalton/benchmarks)`

`(in-package #:coalton-benchmarks)`

`(run-coalton-benchmarks)`

# Coalton benchmark development

Benchmarks can be written in any Coalton project, as long as the package imports or nicknames `#:coalton-benchmarking`. 

Benchmarks are attached to the package they are defined in, though they can be reexported to other packages.

This allows them to be embedded amongst the relevant code, in a standalone suite, or both!

## Benchmark Settings

### Verbose
Coalton benchmarking prints to the repl by default. 

This setting can be turned off with:

```
(cl:setf *coalton-verbose-benchmarking* cl:nil)
```

### Printing width
Coalton benchmarks print to the repl at 90 characters wide by default.

This can be changed using:

```
(cl:setf *coalton-benchmark-width* 90) 
```

###  Print time in cientific notation
By default, times are printed using scientific notation. This can be turned off using:

```
(cl:setf *coalton-benchmark-sci-notation* cl:nil) 
```

## Defining benchmarks:

Benchmarks can be defined in any Coalton package (that imports or nicknames `#:coalton-benchmarking`):

```
;; Defining a Coalton benchmark
(define-benchmark stak 1000 ; iterations
  (fn ()
    (stak 18 12 6)
    Unit))

;; Defining a Lisp Benchmark
(define-benchmark lisp-stak 1000 ; iterations
  (fn ()
    (lisp Unit ()
      (lisp-stak 18 12 6)
      Unit)))
```

## Running individual benchmarks

Individual benchmarks can be run with `#'run-benchmark`, as long as the benchmark is defined.

`#'run-benchmark` returns a `BenchmarkResults` object.

```
COALTON-BENCHMARKS> (coalton (run-benchmark "tak"))
┌─────────────────────────────────────────────────────────────────────────────────────────┐
│                                      Benchmark tak                                      │
├─────────────────────────────────────────────────────────────────────────────────────────┤
│                          System: ARM64 OS-MACOSX SBCL2.2.4-WIP                          │
├─────────────────────────────────────────────────────────────────────────────────────────┤
│                   Coalton development mode without heuristic inlining                   │
├───────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┤
│        Benchmark      │    Time Elapsed     │    Bytes consed     │    # Iterations     │
├───────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┤
│           TAK         │     9.4079e-1 s     │          0          │        1000         │
└───────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┘

#.(BENCHMARKRESULTS "TAK" 1000 940788 #.(SOME 0))
```

## Running package benchmarks

Package benchmarks can be run with #'run-package-benchmarks, from any package that imports coalton-benchmarking.

`#'run-package-benchmarks` returns a `PackageBenchmarkResults` object.

```
COALTON-BENCHMARKS> (coalton (run-package-benchmarks "coalton-benchmarks/gabriel/tak"))
┌─────────────────────────────────────────────────────────────────────────────────────────┐
│                        Package 'coalton-benchmarks/gabriel/tak'                         │
├─────────────────────────────────────────────────────────────────────────────────────────┤
│                          System: ARM64 OS-MACOSX SBCL2.2.4-WIP                          │
├─────────────────────────────────────────────────────────────────────────────────────────┤
│                   Coalton development mode without heuristic inlining                   │
├─────────────────┬─────────────────┬─────────────────┬─────────────────┬─────────────────┤
│    Benchmark    │    Run time     │    Real time    │  Bytes consed   │  # Iterations   │
├─────────────────┼─────────────────┼─────────────────┼─────────────────┼─────────────────┤
│       TAK       │   1.043406 s    │   1.044723 s    │      65520      │      1000       │
├─────────────────┼─────────────────┼─────────────────┼─────────────────┼─────────────────┤
│    LISP-TAK     │   0.082777 s    │   0.082867 s    │      65520      │      1000       │
└─────────────────┴─────────────────┴─────────────────┴─────────────────┴─────────────────┘

#.(PACKAGEBENCHMARKRESULTS "coalton-benchmarks/gabriel/tak" #.(COALTON-BENCHMARKING/BENCHMARKING::BENCHMARKSYSTEM "ARM64" "OS-MACOSX" "SBCL" "2.2.4-WIP" COMMON-LISP:NIL COMMON-LISP:NIL) #(#.(BENCHMARKRESULTS "TAK" 1000 1040557 1041583 95888)
                                                                                                                                                                                            #.(BENCHMARKRESULTS "LISP-TAK" 1000 83104 83040 65520)))
```

## Reexporting package benchmarks
Package benchmarks can be reexported to other packages:

```
(reexport-benchmarks
   "coalton-benchmarks/fibonacci"
   "coalton-benchmarks/big-float"
   "coalton-benchmarks/gabriel")
```

This is useful for package-per-file projects.
