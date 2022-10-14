(in-package #:cl-user)

(defpackage #:coalton-doc
  (:documentation "Implementation of documentation generation for COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:settings #:coalton-impl/settings)
   (#:algo #:coalton-impl/algorithm)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry))
  (:export
   #:write-stdlib-documentation-to-file
   #:write-documentation-to-file))
