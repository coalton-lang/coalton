# Parallel numerical simulations

Three physics simulations and a path tracer, written in Coalton and
parallelized with [`coalton/threads`](../../threads/):

- [`fdtd.ct`](fdtd.ct): electromagnetic waves, by the finite-difference
  time-domain (FDTD) method. A plane pulse is focused by a dielectric
  cylinder.
- [`nbody.ct`](nbody.ct): N bodies under their mutual gravitation, by
  direct summation and the leapfrog method.
- [`ising.ct`](ising.ct): the two-dimensional Ising model of a
  ferromagnet, by Metropolis Monte Carlo.
- [`raytrace.ct`](raytrace.ct): Monte Carlo path tracing of a scene of
  spheres, with the renderer of [`raytrace`](../raytrace/).

Each example's sequential and parallel versions run the same code.
[`run.lisp`](run.lisp) runs both from the same initial state, checks that
they end in exactly the same state, and reports the times. Like
`coalton/threads`, these examples run on SBCL only.

## Running

Make this checkout discoverable through ASDF, and start a Lisp with
`COALTON_ENV=release` for the best performance. Then:

```lisp
(ql:quickload :coalton-parallel-numerics)
(coalton-parallel-numerics:run-examples :image-directory #p"/tmp/")
```

This runs every example with its default parameters and writes images
of the final electric field (`fdtd.pgm`) and spins (`ising.pgm`), and the
rendered picture (`raytrace.ppm`). Each example can also run alone, with
other parameters:

```lisp
(coalton-parallel-numerics:run-fdtd :size 2048 :steps 2000 :image #p"/tmp/fdtd.pgm")
(coalton-parallel-numerics:run-nbody :bodies 16384 :steps 5)
(coalton-parallel-numerics:run-ising :size 4096 :sweeps 100 :temperature 2.269d0)
(coalton-parallel-numerics:run-raytrace :width 1280 :height 720 :samples 64
                                        :image #p"/tmp/raytrace.ppm")
```

`(asdf:test-system :coalton-parallel-numerics)` runs small versions of
the examples and checks that their sequential and parallel runs agree.

## Results

On a 16-core, 32-thread AMD Ryzen AI Max+ PRO 395 with SBCL 2.6.9, in
release mode, with a 4 GB heap (`--dynamic-space-size 4096`) and 32
workers:

| Example | Sequential | Parallel | Speedup |
|---------|-----------:|---------:|--------:|
| FDTD, 1024×1024 grid, 1000 steps | 2.22 s | 0.42 s | 5.2x |
| N-body, 8192 bodies, 10 steps | 1.83 s | 0.13 s | 13.9x |
| Ising, 2048×2048 lattice, 200 sweeps | 4.99 s | 0.31 s | 16.0x |
| Path tracing, 640×360 pixels, 16 samples per pixel | 2.33 s | 0.76 s | 3.1x |

The N-body and Ising simulations do a lot of arithmetic for each value
they read from memory, and their speedups approach the number of
cores. The FDTD simulation does little arithmetic per value: each time
step streams all the fields through memory twice, so it is limited by
memory bandwidth. A single core can already use a good part of the
machine's memory bandwidth, so more cores help less. Storing the fields
as `F32` rather than `F64`, as FDTD codes commonly do, halves the
traffic.

The path tracer does plenty of arithmetic, but it also allocates: its
renderer computes with ordinary Coalton records, as
[`raytrace`](../raytrace/) does on purpose to benchmark the compiler,
and allocates about 12 GB of short-lived vectors, rays and hits per
frame. SBCL's garbage collector stops every thread and collects on one,
so with 32 workers allocating, collections take about 40% of the
parallel run. A larger heap, for which SBCL uses a proportionally larger
nursery, makes them rarer: with an 8 GB heap the speedup is 4.0x. One
worker per core, with `COALTON_NUM_THREADS=16`, gives 4.4x.

## How the parallelism is expressed

The kernels of each simulation are ordinary sequential loops over a
range of rows, or of bodies. For example, this advances the magnetic
field in rows `lo` to `hi` of the grid:

```lisp
(declare update-h! (Grid * UFix * UFix -> Void))
(define (update-h! g lo hi)
  ...)
```

[`common.ct`](common.ct) defines `over-range`, which calls such a
kernel on the whole range in the current thread, or on chunks of it in
parallel with `par:for-chunks!`. A time step of the FDTD simulation is
then:

```lisp
(common:over-range parallel? 0 rows (fn (lo hi) (update-h! g lo hi)))
(common:over-range parallel? 1 (- rows 1) (fn (lo hi) (update-e! g lo hi)))
(add-source! g n)
```

Each loop returns once all its chunks are done, so the electric field is
only updated once the magnetic field is complete. Within a loop, the
chunks are independent: the magnetic field is computed from the electric
field alone and vice versa. Likewise, a body's acceleration depends
only on the positions, an Ising spin's update only on its neighbors,
which have the other color of a checkerboard, and a row of the picture
only on the scene.

Because every value is computed by the same operations in the same order
however the work is divided, the parallel results are identical to the
sequential ones, not just close. The Ising simulation achieves this
with its random numbers too: each update draws a number from a generator
keyed by the sweep and the site (SplitMix64), rather than from a stream
shared between threads. Likewise, each row of the path tracer draws its
random numbers from a stream of its own, seeded from the frame's seed
and the row's number. The N-body potential energy is a parallel sum,
which `par:map-reduce-chunks` computes reproducibly when given a fixed
`:grain`.
