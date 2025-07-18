# Fractal

This is a crude implementation of an infinite-zoom viewer of the Mandelbrot set. This uses the `Num` type class, Coalton's `Big-Float` type, and Coalton-Lisp interop.

Try it with:

```
make run
```

and click-and-drag a rectangle to zoom. (Note: The UI doesn't display the rectangle live.) The console will show progress in rendering.

Instructions to get SDL2 loaded on macOS as of 2025 are [here](https://github.com/lispgames/cl-sdl2/issues/154#issuecomment-1280030566).
