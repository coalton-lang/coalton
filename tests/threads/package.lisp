;;;; package.lisp
;;;;
;;;; Packages for the tests of coalton/threads. Their tests belong to
;;;; the suite of the package COALTON-TESTS.

(fiasco:define-test-package (#:coalton-tests/threads :in fiasco-suites::coalton-tests)
  (:documentation "Tests of the runtime underlying coalton/threads.")
  (:local-nicknames
   (#:backend #:coalton/threads/backend)
   (#:rt #:coalton/threads/runtime)))

(defpackage #:coalton-native-tests/threads
  (:documentation "Tests of the Coalton interface of coalton/threads.")
  (:use #:coalton-testing)
  (:local-nicknames
   (#:array #:coalton/lisparray)
   (#:cell #:coalton/cell)
   (#:exception #:coalton/exception)
   (#:iter #:coalton/iterator)
   (#:result #:coalton/result)
   (#:seq #:coalton/seq)
   (#:vector #:coalton/vector)
   (#:par #:coalton/threads/parallel)
   (#:thread #:coalton/threads/thread)
   (#:mutex #:coalton/threads/mutex)
   (#:cv #:coalton/threads/condition-variable)
   (#:semaphore #:coalton/threads/semaphore)
   (#:atomic #:coalton/threads/atomic)
   (#:channel #:coalton/threads/channel)))

(in-package #:coalton-native-tests/threads)

(coalton-fiasco-init #:coalton-tests)
