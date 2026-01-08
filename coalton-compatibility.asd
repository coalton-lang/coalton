;;; A compatibility system for Coalton, modelled after coalton-compiler.

(asdf:defsystem "coalton-compatibility"
  :description "A Coalton compatibility system."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :pathname "compat/"
  :components ((:file "compatibility-layer")))
