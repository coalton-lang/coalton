;;;; package.lisp

(fiasco:define-test-package #:coalton-tests
  (:documentation "Tests for the COALTON system.")
  (:use #:cl #:coalton-impl/typechecker)
  (:shadowing-import-from #:coalton
                          #:fn
                          #:->
                          #:=>)
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
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:ast #:coalton-impl/ast)
   (#:tc #:coalton-impl/typechecker))

  (:export
   #:run-coalton-tests))

(defpackage #:coalton-native-tests
  (:documentation "Tests for the COALTON system, written in Coalton.")
  (:use #:coalton-testing)
  (:local-nicknames
   (#:types #:coalton-library/types)
   (#:math #:coalton-library/math)
   (#:big-float #:coalton-library/big-float)
   (#:string #:coalton-library/string)
   (#:vector #:coalton-library/vector)
   (#:slice #:coalton-library/slice)
   (#:hashtable #:coalton-library/hashtable)
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:red-black/tree #:coalton-library/ord-tree)
   (#:red-black/map #:coalton-library/ord-map)
   (#:result #:coalton-library/result)))

(in-package #:coalton-native-tests)

(coalton-fiasco-init #:coalton-tests)


