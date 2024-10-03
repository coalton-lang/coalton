# Run the Coalton benchmark suite:

`(ql:quickload :coalton/benchmarks)` or `(asdf:load-system :coalton/benchmarks)`

`(in-package #:coalton-benchmarks)`

`(run-coalton-benchmarks)`

# Coalton benchmark development

Benchmarks can be written in any Coalton project, as long as the package imports or nicknames `#:coalton-benchmarking`. 

## Benchmark Settings

### Verbose
Coalton benchmarking prints to the repl by default. 

This setting can be turned off with:

```
(cell:write! *coalton-verbose-benchmarking* False)
```

### Printing width
Coalton benchmarks print to the repl at 90 characters wide by default.

This can be changed using:

```
(cell:write! *benchmark-width* <UFix>)
```

###  Print time in cientific notation
By default, times are printed using scientific notation. This can be turned off using:

```
(ell:write! *coalton-benchmark-sci-notation* False)
```

## Defining benchmarks:

Benchmarks can be defined in any Coalton package (that imports or nicknames `#:coalton-benchmarking`):

```
;; Defining a Coalton benchmark
(define-benchmark stak 1000 ; samples
  (fn ()
    (stak 18 12 6)
    Unit))

;; Defining a Lisp Benchmark
(define-benchmark lisp-stak 1000
  (fn ()
    (lisp Unit ()
      (lisp-stak 18 12 6)
      Unit)))
```

Parameterized benchmarks allow for multiple inputs to the same function:

```
(define-parameterized-benchmark rec-fib 1000
  (fn (x)
    (fib x)
    Unit)
  (seq:make 10 15 20 25))

```
Using `:detect-convergence?` will add standard deviation information to your run, as well as dynamic sample scaling.

### Dynamic sample scaling


With `:detect-convergence?` selected, benchmarks will run one sample at a time, collecting live standard deviation information for timings:

``` 
(define-parameterized-benchmark rec-fib-generic 1000
  (fn (x)
    (fib-generic-wrapped x)
    Unit)
  (seq:make 10 15 20 25)
  :detect-convergence? cl:t)

```

When running this mode, the `samples` input is interpreted as the minimum samples, and the benchmark will continue running until the standard deviation of time converges. This ensures the minimum amount of samples before a reliable average.

This mode is most effective on benchmarks with significant individual sample times.

## Running individual benchmarks

Individual benchmarks can be run with `#'run-benchmark`, as long as the benchmark is defined in the current package.

`#'run-benchmark` returns a `BenchmarkResults` object.

```
COALTON-BENCHMARKS/FIBONACCI> (coalton (run-benchmark "rec-fib0"))
┌─────────────────────────────────────────────────────────────────────────────────────────┐
│                                   Benchmark rec-fib0                                    │
├─────────────────────────────────────────────────────────────────────────────────────────┤
│                          System: ARM64 OS-MACOSX SBCL2.2.4-WIP                          │
├─────────────────────────────────────────────────────────────────────────────────────────┤
│                   Coalton development mode without heuristic inlining                   │
├──────────────┬──────────────┬──────────────┬──────────────┬──────────────┬──────────────┤
│  Benchmark   │  Time (ms)   │Avg Time (ms) │ Time std dev │  Space (B)   │  # Samples   │
├──────────────┼──────────────┼──────────────┼──────────────┼──────────────┼──────────────┤
│ REC-FIB0     │            2 │            0 │          n/a │            0 │         1000 │
└──────────────┴──────────────┴──────────────┴──────────────┴──────────────┴──────────────┘

#.(BENCHMARKRESULTS REC-FIB0 1000 2371 2 #.NONE #.(SOME 0))
```

## Running package benchmarks

Package benchmarks can be run with `#'run-package-benchmarks`.

`#'run-package-benchmarks` returns a `PackageBenchmarkResults` object.

Local packages can be run with `#'run-benchmarks`.

```
COALTON-BENCHMARKS/FIBONACCI> (coalton (run-benchmarks))
┌─────────────────────────────────────────────────────────────────────────────────────────┐
│                         Package 'COALTON-BENCHMARKS/FIBONACCI'                          │
├─────────────────────────────────────────────────────────────────────────────────────────┤
│                          System: ARM64 OS-MACOSX SBCL2.2.4-WIP                          │
├─────────────────────────────────────────────────────────────────────────────────────────┤
│                   Coalton development mode without heuristic inlining                   │
├──────────────┬──────────────┬──────────────┬──────────────┬──────────────┬──────────────┤
│  Benchmark   │  Time (ms)   │Avg Time (ms) │ Time std dev │  Space (B)   │  # Samples   │
├──────────────┼──────────────┼──────────────┼──────────────┼──────────────┼──────────────┤
│ REC-FIB0     │            2 │            0 │          n/a │            0 │         1000 │
├──────────────┼──────────────┼──────────────┼──────────────┼──────────────┼──────────────┤
│ REC-FIB1     │           20 │            0 │          n/a │            0 │         1000 │
├──────────────┼──────────────┼──────────────┼──────────────┼──────────────┼──────────────┤
│ REC-FIB2     │          216 │            0 │          n/a │            0 │         1000 │
├──────────────┼──────────────┼──────────────┼──────────────┼──────────────┼──────────────┤
│ REC-FIB3     │         2370 │            2 │          n/a │            0 │         1000 │
├──────────────┼──────────────┼──────────────┼──────────────┼──────────────┼──────────────┤
│ REC-FIB-GENE │           17 │            0 │ 0.0055017565 │            0 │         1002 │
├──────────────┼──────────────┼──────────────┼──────────────┼──────────────┼──────────────┤
│ REC-FIB-GENE │           95 │            0 │ 0.0053594956 │            0 │         1002 │
├──────────────┼──────────────┼──────────────┼──────────────┼──────────────┼──────────────┤
│ REC-FIB-GENE │          912 │            1 │ 0.0226823009 │            0 │         1002 │
├──────────────┼──────────────┼──────────────┼──────────────┼──────────────┼──────────────┤
│ REC-FIB-GENE │        10270 │           10 │ 0.3188679543 │            0 │         1014 │
├──────────────┼──────────────┼──────────────┼──────────────┼──────────────┼──────────────┤
│ REC-FIB-LISP │          105 │            0 │          n/a │            0 │         1000 │
├──────────────┼──────────────┼──────────────┼──────────────┼──────────────┼──────────────┤
│ REC-FIB-MONO │          306 │            0 │          n/a │            0 │         1000 │
└───────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┘
#.(PACKAGEBENCHMARKRESULTS "COALTON-BENCHMARKS/FIBONACCI" #.(BENCHMARKSYSTEM "ARM64" "OS-MACOSX" "SBCL" "2.2.4-WIP" COMMON-LISP:NIL COMMON-LISP:NIL) #(#.(BENCHMARKRESULTS REC-FIB0 1000 2476 2 #.NONE #.(SOME 0))
                                                                                                                                                       #.(BENCHMARKRESULTS REC-FIB1 1000 20065 20 #.NONE #.(SOME 0))
                                                                                                                                                       #.(BENCHMARKRESULTS REC-FIB2 1000 215897 216 #.NONE #.(SOME 0))
                                                                                                                                                       #.(BENCHMARKRESULTS REC-FIB3 1000 2370219 2370 #.NONE #.(SOME 0))
                                                                                                                                                       #.(BENCHMARKRESULTS REC-FIB-GENERIC0 1002 16914 17 #.(SOME 5.501756508182156d0) #.(SOME 0))
                                                                                                                                                       #.(BENCHMARKRESULTS REC-FIB-GENERIC1 1002 94676 94 #.(SOME 5.359495667149465d0) #.(SOME 0))
                                                                                                                                                       #.(BENCHMARKRESULTS REC-FIB-GENERIC2 1002 911697 910 #.(SOME 22.68230092439423d0) #.(SOME 0))
                                                                                                                                                       #.(BENCHMARKRESULTS REC-FIB-GENERIC3 1014 10270171 10128 #.(SOME 318.8679543261109d0) #.(SOME 0))
                                                                                                                                                       #.(BENCHMARKRESULTS REC-FIB-LISP 1000 104998 105 #.NONE #.(SOME 0))
                                                                                                                                                       #.(BENCHMARKRESULTS REC-FIB-MONO 1000 306261 306 #.NONE #.(SOME 0))))
```

`#:run-benchmarks` runs the current package's benchmarks.

## Reexporting package benchmarks
Package benchmarks can be manually run from other packages simply by defining a helper function, as in `#:coalton-benchmarks/gabriel`.

```
(coalton-toplevel

  (define (run-gabriel-benchmarks)
    (run-package-benchmarks "coalton-benchmarks/gabriel/tak")
    (run-package-benchmarks "coalton-benchmarks/gabriel/takr")
    (run-package-benchmarks "coalton-benchmarks/gabriel/stak")
    (run-package-benchmarks "coalton-benchmarks/gabriel/takl")))
```

This is useful for package-per-file projects.
