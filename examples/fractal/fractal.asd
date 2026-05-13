(defsystem "fractal"
  :description "Mandelbrot viewer with infinite zoom"
  :author "Robert Smith <robert@stylewarning.com>"
  :license "MIT"
  :depends-on ("sdl2" "coalton" "coalton/xmath")
  :serial t
  :components ((:file "fractal")))
