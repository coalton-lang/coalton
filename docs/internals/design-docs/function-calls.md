# Function Application and Currying in Coalton

Coalton functions are automatically curried like functions in Haskell and Elm. Coalton uses the same optimization as Elm to detect fully applied function calls at runtime and make a direct call to the uncurried function.

Coalton functions are wrapped in `function-entry` structs which contain a reference to the original function as well as the functions arity and a curried version. The functions `F2...FN` produce a `function-entry` for a function of N arguments.
Function calls are then done indirectly through the applicator functions `A2...AN`. When applying arguments to a function the number of arguments are checked against the function's arity, if they are equal then a direct uncurried call can be made.
Otherwise the arguments are applied one at a time to the curried version of the function. If a function-entry is partially applied then it will return a bare function instead of another function entry.
The applicator functions will apply arguments one at a time to a bare function.


When an fully applied call is made on `(optimize (speed 3) (safety 0))` the effective cost is only two branches more than a bare function call.
In the benchmark below `BENCHMARK-ADD4` is the bare function call and `BENCHMARK-FADD4` is the fully applied call on a function entry.


The benchmark code is available at [./function-calls-benchmarks.lisp](./function-calls-benchmarks.lisp).

## Benchmark Results

With `(optimize (speed 3) (safety 1))`:

```
Results for benchmark BENCHMARK-ADD4
                 SAMPLES  TOTAL           MINIMUM     MAXIMUM      MEDIAN      AVERAGE             DEVIATION
REAL-TIME        10000    0.630617        6.0e-5      5.74e-4      6.0e-5      6.30617e-5          2.2454797e-5
RUN-TIME         10000    0.616667        6.0e-5      1.61e-4      6.0e-5      6.16667e-5          5.714299e-6
USER-RUN-TIME    10000    0.602598        5.9e-5      1.43e-4      5.9e-5      6.02598e-5          4.757216e-6
SYSTEM-RUN-TIME  10000    0.014713        1.0e-6      5.9e-5       1.0e-6      1.4713e-6           1.8238356e-6
PAGE-FAULTS      10000    0               0           0            0           0                   0.0
GC-RUN-TIME      10000    0               0           0            0           0                   0.0
BYTES-CONSED     10000    16              0           16           0           0.0016              0.159992
EVAL-CALLS       10000    0               0           0            0           0                   0.0

Results for benchmark BENCHMARK-FADD4
                 SAMPLES  TOTAL          MINIMUM    MAXIMUM     MEDIAN     AVERAGE            DEVIATION
REAL-TIME        10000    0.787874       7.5e-5     5.74e-4     7.5e-5     7.87874e-5         2.6699501e-5
RUN-TIME         10000    0.769355       7.5e-5     1.9e-4      7.5e-5     7.69355e-5         6.4279498e-6
USER-RUN-TIME    10000    0.755012       7.4e-5     1.9e-4      7.4e-5     7.55012e-5         5.3811523e-6
SYSTEM-RUN-TIME  10000    0.015598       1.0e-6     5.3e-5      1.0e-6     1.5598e-6          2.041084e-6
PAGE-FAULTS      10000    0              0          0           0          0                  0.0
GC-RUN-TIME      10000    0              0          0           0          0                  0.0
BYTES-CONSED     10000    0              0          0           0          0                  0.0
EVAL-CALLS       10000    0              0          0           0          0                  0.0
```

With `(optimize (speed 3) (safety 0))`:

```
Results for benchmark BENCHMARK-ADD4
                 SAMPLES  TOTAL           MINIMUM     MAXIMUM      MEDIAN      AVERAGE             DEVIATION
REAL-TIME        10000    0.633633        6.0e-5      5.74e-4      6.0e-5      6.33633e-5          2.479171e-5
RUN-TIME         10000    0.618514        6.0e-5      1.57e-4      6.0e-5      6.18514e-5          6.9335215e-6
USER-RUN-TIME    10000    0.604786        5.9e-5      1.48e-4      5.9e-5      6.04786e-5          6.226487e-6
SYSTEM-RUN-TIME  10000    0.014387        1.0e-6      3.5e-5       1.0e-6      1.4387e-6           1.7320053e-6
PAGE-FAULTS      10000    0               0           0            0           0                   0.0
GC-RUN-TIME      10000    0               0           0            0           0                   0.0
BYTES-CONSED     10000    0               0           0            0           0                   0.0
EVAL-CALLS       10000    0               0           0            0           0                   0.0

Results for benchmark BENCHMARK-FADD4
                 SAMPLES  TOTAL           MINIMUM     MAXIMUM      MEDIAN      AVERAGE             DEVIATION
REAL-TIME        10000    0.634038        6.0e-5      5.44e-4      6.0e-5      6.34038e-5          2.4509389e-5
RUN-TIME         10000    0.619228        6.0e-5      2.02e-4      6.0e-5      6.19228e-5          6.972951e-6
USER-RUN-TIME    10000    0.605571        5.9e-5      1.69e-4      5.9e-5      6.05571e-5          5.95165e-6
SYSTEM-RUN-TIME  10000    0.014658        1.0e-6      6.7e-5       1.0e-6      1.4658e-6           1.9362412e-6
PAGE-FAULTS      10000    0               0           0            0           0                   0.0
GC-RUN-TIME      10000    0               0           0            0           0                   0.0
BYTES-CONSED     10000    0               0           0            0           0                   0.0
EVAL-CALLS       10000    0               0           0            0           0                   0.0
```
