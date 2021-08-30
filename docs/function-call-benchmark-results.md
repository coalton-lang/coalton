With `(optimize (speed 3) (safety 1))`:

```
Results for benchmark BENCHMARK-ADD4
-                SAMPLES  TOTAL           MINIMUM     MAXIMUM      MEDIAN      AVERAGE             DEVIATION
REAL-TIME        10000    0.630617        6.0e-5      5.74e-4      6.0e-5      6.30617e-5          2.2454797e-5
RUN-TIME         10000    0.616667        6.0e-5      1.61e-4      6.0e-5      6.16667e-5          5.714299e-6
USER-RUN-TIME    10000    0.602598        5.9e-5      1.43e-4      5.9e-5      6.02598e-5          4.757216e-6
SYSTEM-RUN-TIME  10000    0.014713        1.0e-6      5.9e-5       1.0e-6      1.4713e-6           1.8238356e-6
PAGE-FAULTS      10000    0               0           0            0           0                   0.0
GC-RUN-TIME      10000    0               0           0            0           0                   0.0
BYTES-CONSED     10000    16              0           16           0           0.0016              0.159992
EVAL-CALLS       10000    0               0           0            0           0                   0.0

Results for benchmark BENCHMARK-FADD4
-                SAMPLES  TOTAL          MINIMUM    MAXIMUM     MEDIAN     AVERAGE            DEVIATION
REAL-TIME        10000    0.787874       7.5e-5     5.74e-4     7.5e-5     7.87874e-5         2.6699501e-5
RUN-TIME         10000    0.769355       7.5e-5     1.9e-4      7.5e-5     7.69355e-5         6.4279498e-6
USER-RUN-TIME    10000    0.755012       7.4e-5     1.9e-4      7.4e-5     7.55012e-5         5.3811523e-6
SYSTEM-RUN-TIME  10000    0.015598       1.0e-6     5.3e-5      1.0e-6     1.5598e-6          2.041084e-6
PAGE-FAULTS      10000    0              0          0           0          0                  0.0
GC-RUN-TIME      10000    0              0          0           0          0                  0.0
BYTES-CONSED     10000    0              0          0           0          0                  0.0
EVAL-CALLS       10000    0              0          0           0          0                  0.0

Results for benchmark BENCHMARK-ADD4-A4
-                SAMPLES  TOTAL           MINIMUM    MAXIMUM      MEDIAN       AVERAGE             DEVIATION
REAL-TIME        10000    3.087174        1.55e-4    0.006325     1.93e-4      3.087174e-4         7.3235313e-4
RUN-TIME         10000    3.048988        1.55e-4    0.006162     1.93e-4      3.048988e-4         7.398838e-4
USER-RUN-TIME    10000    2.925584        1.55e-4    0.00545      1.91e-4      2.925584e-4         6.761848e-4
SYSTEM-RUN-TIME  10000    0.128467        1.0e-6     7.46e-4      2.0e-6       1.28467e-5          6.500388e-5
PAGE-FAULTS      10000    0               0          0            0            0                   0.0
GC-RUN-TIME      10000    992.255         0          5.461        0            0.0992255           0.6795755
BYTES-CONSED     10000    11200191264     1106512    1244880      1113856      1120019.1           12872.723
EVAL-CALLS       10000    0               0          0            0            0                   0.0

Results for benchmark BENCHMARK-UNOPTIMIZED-ADD4
-                SAMPLES  TOTAL          MINIMUM     MAXIMUM      MEDIAN      AVERAGE            DEVIATION
REAL-TIME        10000    0.68191        5.8e-5      6.37e-4      6.3e-5      6.8191e-5          2.6281201e-5
RUN-TIME         10000    0.664578       5.7e-5      2.17e-4      6.3e-5      6.64578e-5         7.93234e-6
USER-RUN-TIME    10000    0.649788       5.7e-5      1.95e-4      6.2e-5      6.49788e-5         6.812955e-6
SYSTEM-RUN-TIME  10000    0.015833       1.0e-6      7.9e-5       1.0e-6      1.5833e-6          2.277512e-6
PAGE-FAULTS      10000    0              0           0            0           0                  0.0
GC-RUN-TIME      10000    0              0           0            0           0                  0.0
BYTES-CONSED     10000    0              0           0            0           0                  0.0
EVAL-CALLS       10000    0              0           0            0           0                  0.0

Results for benchmark BENCHMARK-UNOPTIMIZED-FADD4
-                SAMPLES  TOTAL           MINIMUM     MAXIMUM     MEDIAN      AVERAGE             DEVIATION
REAL-TIME        10000    0.822019        7.0e-5      5.24e-4     7.9e-5      8.22019e-5          2.866028e-5
RUN-TIME         10000    0.801283        7.0e-5      2.04e-4     7.9e-5      8.01283e-5          7.814669e-6
USER-RUN-TIME    10000    0.78696         6.9e-5      2.02e-4     7.8e-5      7.8696e-5           6.923134e-6
SYSTEM-RUN-TIME  10000    0.015355        1.0e-6      5.0e-5      1.0e-6      1.5355e-6           1.9933739e-6
PAGE-FAULTS      10000    0               0           0           0           0                   0.0
GC-RUN-TIME      10000    0               0           0           0           0                   0.0
BYTES-CONSED     10000    32              0           16          0           0.0032              0.22625154
EVAL-CALLS       10000    0               0           0           0           0                   0.0

Results for benchmark BENCHMARK-FADD4-A2-A2
-                SAMPLES  TOTAL            MINIMUM      MAXIMUM       MEDIAN       AVERAGE              DEVIATION
REAL-TIME        10000    3.822954         1.98e-4      0.007004      2.47e-4      3.822954e-4          7.940273e-4
RUN-TIME         10000    3.771901         1.98e-4      0.007178      2.47e-4      3.771901e-4          8.0236146e-4
USER-RUN-TIME    10000    3.625574         1.97e-4      0.006121      2.45e-4      3.625574e-4          7.3154015e-4
SYSTEM-RUN-TIME  10000    0.152141         1.0e-6       0.00106       2.0e-6       1.52141e-5           7.253416e-5
PAGE-FAULTS      10000    0                0            0             0            0                    0.0
GC-RUN-TIME      10000    1147.673         0            6.424         0            0.1147673            0.73563826
BYTES-CONSED     10000    12800130080      1250960      1376224       1277952      1280013.0            8020.3496
EVAL-CALLS       10000    0                0            0             0            0                    0.0

Results for benchmark BENCHMARK-NOOP
-                SAMPLES  TOTAL          MINIMUM    MAXIMUM      MEDIAN     AVERAGE            DEVIATION
REAL-TIME        10000    0.09791        7.0e-6     4.79e-4      9.0e-6     9.791e-6           8.922047e-6
RUN-TIME         10000    0.095797       7.0e-6     9.2e-5       9.0e-6     9.5797e-6          2.5794666e-6
USER-RUN-TIME    10000    0.082385       6.0e-6     4.8e-5       8.0e-6     8.2385e-6          2.0299306e-6
SYSTEM-RUN-TIME  10000    0.013642       1.0e-6     8.2e-5       1.0e-6     1.3642e-6          1.2945881e-6
PAGE-FAULTS      10000    0              0          0            0          0                  0.0
GC-RUN-TIME      10000    0              0          0            0          0                  0.0
BYTES-CONSED     10000    0              0          0            0          0                  0.0
EVAL-CALLS       10000    0              0          0            0          0                  0.0
```

With `(optimize (speed 3) (safety 0))`:

```
Results for benchmark BENCHMARK-ADD4
-                SAMPLES  TOTAL           MINIMUM     MAXIMUM      MEDIAN      AVERAGE             DEVIATION
REAL-TIME        10000    0.633633        6.0e-5      5.74e-4      6.0e-5      6.33633e-5          2.479171e-5
RUN-TIME         10000    0.618514        6.0e-5      1.57e-4      6.0e-5      6.18514e-5          6.9335215e-6
USER-RUN-TIME    10000    0.604786        5.9e-5      1.48e-4      5.9e-5      6.04786e-5          6.226487e-6
SYSTEM-RUN-TIME  10000    0.014387        1.0e-6      3.5e-5       1.0e-6      1.4387e-6           1.7320053e-6
PAGE-FAULTS      10000    0               0           0            0           0                   0.0
GC-RUN-TIME      10000    0               0           0            0           0                   0.0
BYTES-CONSED     10000    0               0           0            0           0                   0.0
EVAL-CALLS       10000    0               0           0            0           0                   0.0

Results for benchmark BENCHMARK-FADD4
-                SAMPLES  TOTAL           MINIMUM     MAXIMUM      MEDIAN      AVERAGE             DEVIATION
REAL-TIME        10000    0.634038        6.0e-5      5.44e-4      6.0e-5      6.34038e-5          2.4509389e-5
RUN-TIME         10000    0.619228        6.0e-5      2.02e-4      6.0e-5      6.19228e-5          6.972951e-6
USER-RUN-TIME    10000    0.605571        5.9e-5      1.69e-4      5.9e-5      6.05571e-5          5.95165e-6
SYSTEM-RUN-TIME  10000    0.014658        1.0e-6      6.7e-5       1.0e-6      1.4658e-6           1.9362412e-6
PAGE-FAULTS      10000    0               0           0            0           0                   0.0
GC-RUN-TIME      10000    0               0           0            0           0                   0.0
BYTES-CONSED     10000    0               0           0            0           0                   0.0
EVAL-CALLS       10000    0               0           0            0           0                   0.0

Results for benchmark BENCHMARK-ADD4-A4
-                SAMPLES  TOTAL            MINIMUM      MAXIMUM        MEDIAN       AVERAGE              DEVIATION
REAL-TIME        10000    3.213704         1.53e-4      0.01768        1.9e-4       3.213704e-4          8.331843e-4
RUN-TIME         10000    3.16845          1.53e-4      0.016815       1.91e-4      3.16845e-4           8.35662e-4
USER-RUN-TIME    10000    3.039483         1.52e-4      0.012879       1.89e-4      3.039483e-4          7.6812576e-4
SYSTEM-RUN-TIME  10000    0.134056         1.0e-6       0.003939       2.0e-6       1.34056e-5           7.4842945e-5
PAGE-FAULTS      10000    0                0            0              0            0                    0.0
GC-RUN-TIME      10000    1118.764         0            15.968         0            0.1118764            0.77347744
BYTES-CONSED     10000    11200244320      1113712      1244880        1113856      1120024.4            12874.732
EVAL-CALLS       10000    0                0            0              0            0                    0.0

Results for benchmark BENCHMARK-UNOPTIMIZED-ADD4
-                SAMPLES  TOTAL           MINIMUM     MAXIMUM     MEDIAN      AVERAGE             DEVIATION
REAL-TIME        10000    0.647191        6.0e-5      5.45e-4     6.2e-5      6.47191e-5          2.2859904e-5
RUN-TIME         10000    0.633177        6.0e-5      1.95e-4     6.2e-5      6.33177e-5          5.879164e-6
USER-RUN-TIME    10000    0.619443        5.9e-5      1.46e-4     6.1e-5      6.19443e-5          5.0151766e-6
SYSTEM-RUN-TIME  10000    0.014708        1.0e-6      5.6e-5      1.0e-6      1.4708e-6           1.7659976e-6
PAGE-FAULTS      10000    0               0           0           0           0                   0.0
GC-RUN-TIME      10000    0               0           0           0           0                   0.0
BYTES-CONSED     10000    0               0           0           0           0                   0.0
EVAL-CALLS       10000    0               0           0           0           0                   0.0

Results for benchmark BENCHMARK-UNOPTIMIZED-FADD4
-                SAMPLES  TOTAL           MINIMUM     MAXIMUM      MEDIAN     AVERAGE             DEVIATION
REAL-TIME        10000    0.796283        7.0e-5      5.81e-4      7.5e-5     7.96283e-5          2.598808e-5
RUN-TIME         10000    0.779437        7.0e-5      1.83e-4      7.5e-5     7.79437e-5          9.685036e-6
USER-RUN-TIME    10000    0.764865        6.9e-5      1.63e-4      7.4e-5     7.64865e-5          8.914798e-6
SYSTEM-RUN-TIME  10000    0.015641        1.0e-6      6.4e-5       1.0e-6     1.5641e-6           2.0806467e-6
PAGE-FAULTS      10000    0               0           0            0          0                   0.0
GC-RUN-TIME      10000    0               0           0            0          0                   0.0
BYTES-CONSED     10000    0               0           0            0          0                   0.0
EVAL-CALLS       10000    0               0           0            0          0                   0.0

Results for benchmark BENCHMARK-FADD4-A2-A2
-                SAMPLES  TOTAL            MINIMUM      MAXIMUM        MEDIAN     AVERAGE              DEVIATION
REAL-TIME        10000    3.985363         1.97e-4      0.015414       2.5e-4     3.985363e-4          8.8045304e-4
RUN-TIME         10000    3.926046         1.98e-4      0.015622       2.5e-4     3.926046e-4          8.8454236e-4
USER-RUN-TIME    10000    3.780663         1.98e-4      0.011563       2.48e-4    3.780663e-4          8.13534e-4
SYSTEM-RUN-TIME  10000    0.150743         1.0e-6       0.004061       2.0e-6     1.50743e-5           7.916986e-5
PAGE-FAULTS      10000    0                0            0              0          0                    0.0
GC-RUN-TIME      10000    1269.006         0            14.654         0          0.1269006            0.8184251
BYTES-CONSED     10000    12800181952      1263328      1376224        1277952    1280018.3            8022.7134
EVAL-CALLS       10000    0                0            0              0          0                    0.0

Results for benchmark BENCHMARK-NOOP
-                SAMPLES  TOTAL         MINIMUM    MAXIMUM     MEDIAN     AVERAGE           DEVIATION
REAL-TIME        10000    0.114374      1.0e-5     4.4e-4      1.0e-5     1.14374e-5        1.0062698e-5
RUN-TIME         10000    0.111698      1.0e-5     7.3e-5      1.0e-5     1.11698e-5        2.228804e-6
USER-RUN-TIME    10000    0.099802      9.0e-6     4.9e-5      9.0e-6     9.9802e-6         1.7028823e-6
SYSTEM-RUN-TIME  10000    0.012242      1.0e-6     2.9e-5      1.0e-6     1.2242e-6         8.94055e-7
PAGE-FAULTS      10000    0             0          0           0          0                 0.0
GC-RUN-TIME      10000    0             0          0           0          0                 0.0
BYTES-CONSED     10000    0             0          0           0          0                 0.0
EVAL-CALLS       10000    0             0          0           0          0                 0.0
```
