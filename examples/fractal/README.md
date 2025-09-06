# Fractal

This is a crude implementation of an infinite-zoom viewer of the Mandelbrot set. This uses the `Num` type class, Coalton's `Big-Float` type, and Coalton-Lisp interop.

Try it with:

```
make run
```

After some compilation output, a black window will pop up and some dots `....` will be printed to the terminal. After 50 dots are printed, the window should get populated with an image of the Mandelbrot set.

Click-and-drag a rectangle to zoom. (Note: The UI doesn't display the rectangle live.) Again, the terminal will print some progress output while it renders.

Press the `Esc` key to quit.

## Instructions for macOS on Apple Silicon

To run this on macOS, you will need to follow a few extra steps. Here's what works as of August 2025.

1. `brew install sdl2`
2. Follow [these instructions](https://github.com/lispgames/cl-sdl2/issues/154#issuecomment-1280030566) to get a working Lisp SDL library.
3. Run: `sbcl --eval '(ql:quickload "sdl2")' --quit`
4. `cd path/to/coalton/examples/fractal/`
5. `make run`
