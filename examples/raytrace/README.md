# Ray tracer allocation benchmark

This is a small, deterministic Monte Carlo path tracer for measuring changes to
Coalton's allocation and generated code. All rendering code is in `raytrace.ct`.
`benchmark.lisp` handles measurement and image output; `tests.ct` checks correctness.
Only Coalton and its existing dependencies are required.

The scene contains diffuse, rough metal, and glass spheres, lit by a sky gradient.
A pinhole camera emits jittered primary rays. Paths reflect, refract, or scatter
until they escape or reach the bounce limit. A median-split bounding-volume
hierarchy accelerates intersections. The scene and sample streams use separate,
fixed seeds and a local xorshift32 generator, so they do not depend on Lisp's
global random state.

Vectors, rays, hits, scattering results, and bounding intervals are ordinary
Coalton records. Vector arithmetic constructs and returns vectors. There are no
object pools, manual stack-allocation declarations, or conversions of records
into multiple return values. Small vector helpers have ordinary `inline`
annotations; normal compiler optimizations remain enabled. The persistent scene
tree and short-lived intermediate records provide different lifetime patterns
for future escape analysis. Only the output framebuffer is a packed F64 array.

## Running

Make this checkout discoverable through ASDF (for example, symlink it into
Quicklisp's `local-projects`, or add its root recursively to `CL_SOURCE_REGISTRY`).
Start a **fresh Lisp with `COALTON_ENV=release`**. Use a clean or separate ASDF
cache when changing compiler settings or switching development/release modes.

```lisp
(ql:quickload :coalton-raytrace)
(asdf:test-system :coalton-raytrace)

(coalton-raytrace/benchmark:run-benchmark :output #p"raytrace.ppm")
```

The default workload is 320 by 180 pixels, 16 samples per pixel, a maximum path
depth of 8, seed 12345, and three measured trials after one full warm-up render.
It can allocate many gigabytes cumulatively without requiring that much live
memory. For a quicker development check:

```lisp
(coalton-raytrace/benchmark:run-benchmark
  :width 80 :height 45 :samples 4 :depth 6 :trials 3)
```

For a less noisy picture, increase the sample count. PPM output is optional and
uses square-root display gamma. The returned framebuffer contains linear RGB
in row order from top to bottom. `render-frame` accepts the same rendering
settings and returns an image without benchmarking:

```lisp
(let ((pixels (coalton-raytrace/benchmark:render-frame
               :width 640 :height 360 :samples 64)))
  (coalton-raytrace/benchmark:write-ppm #p"picture.ppm" pixels 640 360))
```

## What is measured

Each trial renders the complete frame into a preallocated framebuffer. Scene
construction, BVH construction, camera setup, output-buffer allocation, warm-up,
image comparisons, checksums, printing, and file writing are outside the timed
region. A full GC precedes each trial on SBCL and CCL; GC remains enabled during
rendering. The frame-local random state is reset for each render, and its small
allocation is included.

The runner reports elapsed time for each trial and the median. On SBCL it also
reports total bytes allocated, bytes per primary sample, and GC time. Allocation
counters measure cumulative allocation, **not peak live memory**. Their granularity
can cause small differences between otherwise identical trials. A primary sample
includes all its secondary bounces; this is not a bytes-per-ray measurement.
On other Lisps, unavailable counters are returned as `nil`.

The two return values are a report plist (including individual trials) and the
framebuffer. The report records the Lisp version, renderer build settings,
workload settings, and an image checksum. Every trial must reproduce the warm-up
image exactly. The checksum uses quantized linear RGB; use an elementwise
floating-point tolerance when comparing different Lisp implementations or
arithmetic transformations. The tests include analytic intersection/scattering
cases, BVH versus brute-force traversal, and deterministic small renders.
These small tests run in CI; the measured benchmark is opt-in.

## Comparing compiler changes

Keep the renderer source, Lisp version, machine, compiler settings, workload,
and seed fixed. Record the Coalton commit and any Lisp compiler-policy overrides
alongside the returned report. Rebuild the compiler, library, and benchmark in
a fresh process/cache for each compiler version. Compare median elapsed time,
allocated bytes, and GC time together, and check the pixels. An allocation
reduction is useful only if it preserves the output and improves the workload.

Use separate profiling runs to identify allocation sites; do not compare a
profiled run's elapsed time to an unprofiled baseline. On SBCL, for example:

```lisp
(require :sb-sprof)
(sb-sprof:with-profiling (:mode :alloc :max-samples 10000 :reset t :report :flat)
  (coalton-raytrace/benchmark:run-benchmark
    :width 160 :height 90 :samples 8 :trials 1))
```

That profile also includes setup, warm-up, and reporting. For a render-only
profile, construct `make-scene`, `make-camera`, and the framebuffer first, then
profile repeated calls to the Coalton `render!` function. No performance
thresholds are enforced by the tests. The renderer is single-threaded and has
no textures, explicit light sampling, or production image pipeline; it is one
representative workload, not a comprehensive graphics benchmark.
