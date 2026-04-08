;;; This is coalton.asd, the toplevel coalton system definition.
;;;
;;; While it would be more convenient to put all of Coalton's
;;; dependencies into a single file, the need to define an ASDF
;;; extension for Coalton source files prevents that. Specifically,
;;; coalton/library's system definition contains a
;;; :defsystem-depends-on clause, and though it would be nice to to be
;;; able call that dependency 'coalton/asdf', ASDF signals a
;;; circular dependency error, claiming that it depends on 'coalton'.
;;;
;;; The asdf extension in turn requires access to the compiler. so
;;; coalton-asdf and coalton-compiler live in their own .asd files.

(asdf:defsystem "coalton"
  :description "An efficient, statically typed functional programming language that supercharges Common Lisp. "
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :in-order-to ((asdf:test-op (asdf:test-op #:coalton/tests)))
  :depends-on ("coalton-compiler"
               "coalton/library"))

(asdf:defsystem "coalton/library"
  :description "The Coalton standard library."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t)
                          #+sbcl (sb-ext:*block-compile-default* :specified)
                          ;; The lisp-toplevel form is currently
                          ;; restricted to standard library
                          ;; implementation by checking for the
                          ;; presence of this feature.
                          (*features* (cons ':coalton-lisp-toplevel *features*)))
                      (funcall compile)))
  :defsystem-depends-on ("coalton-asdf")
  :depends-on ("coalton-compiler"
               "coalton/hashtable-shim"
               "trivial-garbage"
               "alexandria")
  :pathname "library/"
  :serial t
  :components ((:file "set-float-traps")
               (:ct-file "utils")
               (:ct-file "types")
               (:ct-file "primitive-types")
               (:ct-file "classes")
               (:ct-file "builtin")
               (:ct-file "show")
               (:module "internal"
                :serial t
                :components ((:module "rbit"
                              :serial t
                              :components
                              ((:file "package")
                               (:file "portable"
                                :if-feature (:not (:and :sbcl :arm64)))
                               (:file "sbcl-arm64"
                                :if-feature (:and :sbcl :arm64))))))
               (:file "hash-defining-macros")
               (:ct-file "hash")
               (:file "derivers")
               (:ct-file "functions")
               (:ct-file "boolean")
               (:ct-file "bits")
               (:ct-file "symbol")
               (:module "math"
                :serial t
               :components ((:ct-file "arith")
                             (:file "num-defining-macros")
                             (:ct-file "num")
                             (:ct-file "bounded")
                             (:ct-file "conversions")
                             (:ct-file "fraction")
                             (:ct-file "integral")
                             (:ct-file "real")
                             (:ct-file "complex")
                             (:ct-file "elementary")
                             (:file "package")))
               (:ct-file "randomaccess")
               (:ct-file "cell")
               (:ct-file "tuple")
               (:ct-file "optional")
               (:ct-file "iterator")
               (:ct-file "result")
               (:ct-file "lisparray")
               (:ct-file "list")
               (:module "experimental"
                :serial t
                :components ((:ct-file "loops")
                             (:file "package")))
               (:ct-file "vector")
               (:ct-file "char")
               (:ct-file "string")
               (:ct-file "slice")
               (:ct-file "hashtable")
               (:ct-file "iterator-hashtable")
               (:ct-file "hashmap")
               (:ct-file "queue")
               (:module "monad"
                :serial t
                :components ((:ct-file "classes")
                             (:ct-file "identity")
                             (:ct-file "state")
                             (:ct-file "statet")
                             (:ct-file "environment")
                             (:ct-file "resultt")
                             (:ct-file "optionalt")
                             (:ct-file "free")
                             (:ct-file "freet")))
               ;; Unfortunately this had to be split from the rest of the
               ;; experimental files because of intermediate dependencies.
               (:module "experimental-do-control"
                :pathname "experimental/"
                :serial t
                :components ((:ct-file "do-control-core")
                             (:ct-file "do-control-loops")
                             (:ct-file "do-control-loops-adv")))
               (:ct-file "ordtree")
               (:ct-file "ordmap")
               (:ct-file "seq")
               (:ct-file "system")
               (:ct-file "file")

               (:file "prelude")))

(cl:when (cl:member (uiop:getenv "COALTON_PORTABLE_BIGFLOAT") '("1" "true" "t") :test #'cl:equalp)
  (cl:pushnew ':coalton-portable-bigfloat cl:*features*))

(asdf:defsystem "coalton/xmath"
  :description "Extended mathematics library for Coalton."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t)
                          #+sbcl (sb-ext:*block-compile-default* :specified))
                      (funcall compile)))
  :defsystem-depends-on ("coalton-asdf")
  :depends-on ("coalton"
               "coalton/library"
               "computable-reals"
               (:feature (:and (:not :coalton-portable-bigfloat) :sbcl) "sb-mpfr")
               (:feature (:and (:not :coalton-portable-bigfloat) :sbcl) "sb-gmp"))
  :pathname "xmath/"
  :serial t
  :components ((:ct-file "dyadic")
               (:ct-file "dual")
               (:ct-file "hyperdual")
               (:ct-file "fft")
               (:module "big-float"
                :serial t
                :components ((:file "package")
                             (:ct-file "impl-sbcl"
                              :if-feature (:and (:not :coalton-portable-bigfloat) :sbcl))
                             (:ct-file "impl-default"
                              :if-feature (:or :coalton-portable-bigfloat (:not :sbcl)))))
               (:module "computable-reals"
                :serial t
                :components ((:ct-file "computable-reals")))
               (:ct-file "realalgebraic")))

(asdf:defsystem "coalton/library/big-float"
  :description "Deprecated. Use coalton/xmath."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :depends-on ("coalton/xmath"))

(asdf:defsystem "coalton/library/computable-reals"
  :description "Deprecated. Use coalton/xmath."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :depends-on ("coalton/xmath"))

(asdf:defsystem "coalton/library/algorithms"
  :description "Deprecated. Use coalton/xmath."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :depends-on ("coalton/xmath"))

(asdf:defsystem "coalton/testing"
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :depends-on ("coalton"
               "fiasco")
  :pathname "src/testing/"
  :serial t
  :components ((:file "package")
               (:file "coalton-native-test-utils")))

;;; we need to inspect the sbcl version in order to decide which version of the hashtable shim to load,
;;; because 2.1.12 includes (or will include) a bugfix that allows a cleaner, more maintainable
;;; implementation.

#+sbcl
(cl:handler-case
    (cl:progn
      (sb-ext:assert-version->= 2 2 2)
      (cl:pushnew ':sbcl-post-2-2-2 cl:*features*))
  (cl:error (c)
    (declare (ignore c))
    (cl:pushnew ':sbcl-pre-2-2-2 cl:*features*)))

(asdf:defsystem "coalton/hashtable-shim"
  :description "Shim over Common Lisp hash tables with custom hash functions, for use by the Coalton standard library."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :pathname "src/hashtable-shim"
  :serial t
  :components ((:file "defs")
               (:file "impl-sbcl" :if-feature :sbcl)
               (:file "hash-table" :if-feature (:not :sbcl))
               (:file "impl-custom" :if-feature (:not :sbcl))))

(asdf:defsystem "coalton/doc"
  :description "Documentation generator for Coalton"
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :depends-on ("coalton"
               "coalton/xmath"
               "html-entities"
               "spinneret"
               "yason"
               "uiop")
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/doc"
  :serial t
  :components ((:file "base")
               (:file "environment")
               (:file "model")
               (:file "string")
               (:file "markdown")
               (:file "html")
               (:file "hugo")
               (:file "main")))

(asdf:defsystem "coalton/tests"
  :description "Tests for COALTON."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :defsystem-depends-on ("coalton-asdf")
  :depends-on ("coalton"
               "coalton/library/big-float"
               "coalton/library/algorithms"
               "coalton/doc"
               "coalton/xmath"
               "coalton/testing"
               "fiasco"
               "quil-coalton/tests"
               "thih-coalton/tests")
  :perform (asdf:test-op (o s)
                         (unless (symbol-call :coalton-tests :run-coalton-tests)
                           (error "Tests failed")))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "loader")
               (:file "utilities")
               (:file "source-tests")
               (:file "preserve-case-tests")
               (:file "tarjan-scc-tests")
               (:file "avl-tree-tests")
               (:file "reader-tests")
               (:file "error-tests")
               (:module "parser"
                :serial t
                :components ((:file "cursor-tests")))
               (:file "entry-tests")
               (:file "codegen-pattern-tests")
               (:file "toplevel-tests")
               (:file "doc-tests")
               (:file "type-inference-tests")
               (:file "fundep-tests")
               (:file "fundep-fib-test")
               (:ct-file "runtime-tests")
               (:module "typechecker"
                :serial t
                :components ((:file "lisp-type-tests")
                             (:file "dictionary-resolution-tests")))
               (:file "environment-persist-tests")
               (:file "coalton-tests")
               (:file "shortcut-tailcall-tests")
               (:file "slice-tests")
               (:ct-file "float-tests")
               (:ct-file "dual-tests")
               (:ct-file "hyperdual-tests")
               (:ct-file "quantize-tests")
               (:ct-file "realalgebraic-tests")
               (:file "hashtable-tests")
               (:file "hashmap-tests")
               (:file "iterator-tests")
               (:file "call-coalton-from-lisp")
               (:file "bits-tests")
               (:file "vector-tests")
               (:file "queue-tests")
               (:file "string-tests")
               (:file "optional-tests")
               (:file "ordtree-tests")
               (:file "ordmap-tests")
               (:file "recursive-let-tests")
               (:file "multiple-values-tests")
               (:file "class-tests")
               (:file "struct-tests")
               (:file "type-alias-tests")
               (:file "list-tests")
               (:file "lisparray-tests")
               (:file "seq-tests")
               (:file "pattern-matching-tests")
               (:file "looping-native-tests")
               (:ct-file "monomorphizer-tests")
               (:ct-file "inliner-tests")
               (:file "inliner-tests-1") ; must come after inliner-tests
               (:ct-file "deriver-tests")
               (:ct-file "file-tests")
               (:ct-file "experimental-tests")
               (:file "exceptions")
               (:module "monad"
                :serial t
                :components ((:ct-file "optionalt")
                             (:ct-file "resultt")
                             (:ct-file "environment")
                             (:ct-file "statet")))
               (:module "algorithms-tests"
                :serial t
                :components ((:file "fft-tests")))
               (:module "redef-detection"
                :serial t
                :components ((:file "interactive-tests")))))
