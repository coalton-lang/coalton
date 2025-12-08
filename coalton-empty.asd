;;; A compatibility system for Coalton, modelled after coalton-compiler.

(asdf:defsystem "coalton-empty"
  :description "The Coalton compatibility system. With a system-name (or file-name?) that ASDF likes..."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :pathname "compat/"
  :components ((:file "compatibility-layer")))
