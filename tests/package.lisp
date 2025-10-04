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
   (#:types #:coalton-library/types)
   (#:math #:coalton-library/math)
   (#:hyperdual #:coalton-library/math/hyperdual)
   (#:big-float #:coalton-library/big-float)
   (#:bits #:coalton-library/bits)
   (#:char #:coalton-library/char)
   (#:string #:coalton-library/string)
   (#:vector #:coalton-library/vector)
   (#:slice #:coalton-library/slice)
   (#:hashtable #:coalton-library/hashtable)
   (#:hashmap #:coalton-library/hashmap)
   (#:cell #:coalton-library/cell)
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:array #:coalton-library/lisparray)
   (#:ordtree #:coalton-library/ordtree)
   (#:ordmap #:coalton-library/ordmap)
   (#:result #:coalton-library/result)
   (#:seq #:coalton-library/seq)
   (#:file #:coalton-library/file)
   (#:experimental #:coalton-library/experimental)
   (#:st #:coalton-library/monad/state)
   (#:m-id #:coalton-library/monad/identity)
   (#:m-opt #:coalton-library/monad/optionalt)
   (#:m-res #:coalton-library/monad/resultt)
   (#:m-env #:coalton-library/monad/environment)
   (#:m-stt #:coalton-library/monad/statet)
   (#:fft #:coalton-library/algorithms/fft)))

(in-package #:coalton-native-tests)

(coalton-fiasco-init #:coalton-tests)
