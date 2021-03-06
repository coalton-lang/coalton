# Coalton `Big-Float`

`Big-Float` is an arbitrary precision floating point number. The numeric results
will  *not* be consistent across implementations.

By default SBCL will use the `sb-mpfr` library.

This depends on the MPFR library being installed, and visible within your
dynamic linker path. Otherwise, you will get an error while compiling.

Before coalton is compiled set the environment
variable`COALTON_PORTABLE_BIGFLOAT=1` or add the feature
`:coalton-portable-bigfloat` to proceed anyways.
All other implementations will use a `Big-Float` written in Coalton. This comes
at a significant slow down and increased memory usage.
