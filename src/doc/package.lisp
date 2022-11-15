(in-package #:cl-user)

(uiop:define-package #:coalton-doc
  (:documentation "Implementation of documentation generation for COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl
        #:coalton-impl/util
        #:coalton-impl/settings
        #:coalton-impl/algorithm
        #:coalton-impl/ast
        #:coalton-impl/typechecker
        #:coalton-impl/codegen)
  (:export
   #:write-stdlib-documentation-to-file
   #:write-documentation-to-file))
