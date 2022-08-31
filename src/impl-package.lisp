(defpackage #:coalton-impl
  (:documentation "Implementation and runtime for COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use
   #:cl)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:error #:coalton-impl/error)
   (#:ast #:coalton-impl/ast)
   (#:rt #:coalton-impl/runtime)
   (#:tc #:coalton-impl/typechecker))

  (:import-from
   #:coalton-impl/settings
   #:coalton-release-p
   #:*coalton-optimize-library*)
  (:export
   #:coalton-release-p
   #:*coalton-optimize-library*))
