;;;; package.lisp

(fiasco:define-test-package #:coalton-tests
  (:documentation "Tests for the COALTON system.")
  (:use #:cl #:coalton-impl/typechecker)
  (:shadowing-import-from #:coalton
                          #:fn
                          #:->
                          #:=>)
  (:import-from #:coalton-impl/codegen
                #:direct-application)
  (:import-from #:coalton-impl/ast
                #:pattern-var
                #:pattern-wildcard
                #:pattern-literal
                #:pattern-constructor)
  (:shadowing-import-from #:coalton
                          #:String
                          #:Integer
                          #:Char
                          #:Unit)
  (:export
   #:run-coalton-tests))

(uiop:define-package #:coalton-test-user
  (:documentation "A copy-cat package to COALTON-USER for testing.")
  (:use #:coalton #:coalton-library))
