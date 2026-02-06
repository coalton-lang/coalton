;;;; package.lisp

(fiasco:define-test-package #:coalton-tests
  (:documentation "Tests for the COALTON system.")
  (:use
   #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker)
   (#:ast #:coalton-impl/codegen/ast)
   (#:traverse #:coalton-impl/codegen/traverse)
   (#:analysis #:coalton-impl/analysis)
   (#:entry #:coalton-impl/entry))
  (:export
   #:run-coalton-tests
   #:run-test-file
   #:run-test))

(defpackage #:coalton-native-tests
  (:documentation "Tests for the COALTON system, written in Coalton.")
  (:use #:coalton-testing)
  (:local-nicknames
   (#:types #:coalton/types)
   (#:math #:coalton/math)
   (#:hyperdual #:coalton/math/hyperdual)
   (#:big-float #:coalton/big-float)
   (#:bits #:coalton/bits)
   (#:char #:coalton/char)
   (#:string #:coalton/string)
   (#:vector #:coalton/vector)
   (#:slice #:coalton/slice)
   (#:hashtable #:coalton/hashtable)
   (#:hashmap #:coalton/hashmap)
   (#:cell #:coalton/cell)
   (#:iter #:coalton/iterator)
   (#:list #:coalton/list)
   (#:array #:coalton/lisparray)
   (#:ordtree #:coalton/ordtree)
   (#:ordmap #:coalton/ordmap)
   (#:result #:coalton/result)
   (#:seq #:coalton/seq)
   (#:file #:coalton/file)
   (#:experimental #:coalton/experimental)
   (#:st #:coalton/monad/state)
   (#:m-id #:coalton/monad/identity)
   (#:m-opt #:coalton/monad/optionalt)
   (#:m-res #:coalton/monad/resultt)
   (#:m-env #:coalton/monad/environment)
   (#:m-stt #:coalton/monad/statet)
   (#:fft #:coalton/algorithms/fft)))

(in-package #:coalton-native-tests)

(coalton-fiasco-init #:coalton-tests)
