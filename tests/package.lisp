;;;; package.lisp

(fiasco:define-test-package #:coalton-tests
  (:documentation "Tests for the COALTON system.")
  (:use
   #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)
   (#:analysis #:coalton-impl/analysis)
   (#:entry #:coalton-impl/entry))
  (:export
   #:run-coalton-tests))

(defpackage #:coalton-native-tests
  (:documentation "Tests for the COALTON system, written in Coalton.")
  (:use #:coalton-testing)
  (:local-nicknames
   (#:types #:coalton-library/types)
   (#:math #:coalton-library/math)
   (#:big-float #:coalton-library/big-float)
   (#:char #:coalton-library/char)
   (#:string #:coalton-library/string)
   (#:vector #:coalton-library/vector)
   (#:slice #:coalton-library/slice)
   (#:hashtable #:coalton-library/hashtable)
   (#:cell #:coalton-library/cell)
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:red-black/tree #:coalton-library/ord-tree)
   (#:red-black/map #:coalton-library/ord-map)
   (#:result #:coalton-library/result)
   (#:seq #:coalton-library/seq)))

(in-package #:coalton-native-tests)

(coalton-fiasco-init #:coalton-tests)


