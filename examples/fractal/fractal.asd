(defsystem "fractal"
  :description "Mandelbrot viewer with infinite zoom"
  :author "Robert Smith <robert@stylewarning.com>"
  :license "MIT"
  :depends-on ("sdl2" "coalton" "coalton/library/big-float")
  :serial t
  :components ((:file "fractal")))
