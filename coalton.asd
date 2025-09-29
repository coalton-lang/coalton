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
               (:file "utils")
               (:file "types")
               (:file "primitive-types")
               (:file "classes")
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
               (:file "hash")
               (:file "derivers")
               (:file "builtin")
               (:file "functions")
               (:file "boolean")
               (:file "bits")
               (:file "symbol")
               (:module "math"
                :serial t
                :components ((:file "arith")
                             (:file "num-defining-macros")
                             (:file "num")
                             (:file "bounded")
                             (:file "conversions")
                             (:file "fraction")
                             (:file "integral")
                             (:file "real")
                             (:file "complex")
                             (:file "elementary")
                             (:file "dyadic")
                             (:file "dual")
                             (:file "hyperdual")
                             (:file "package")))
               (:module "experimental"
                :serial t
                :components ((:file "loops")
                             (:file "package")))
               (:file "randomaccess")
               (:file "cell")
               (:file "tuple")
               (:file "iterator")
               (:file "optional")
               (:file "result")
               (:file "lisparray")
               (:file "list")
               (:file "vector")
               (:file "char")
               (:file "string")
               (:file "slice")
               (:file "hashtable")
               (:file "hashmap")
               (:file "queue")
               (:module "monad"
                :serial t
                :components ((:file "identity")
                             (:file "state")
                             (:file "environment")
                             (:file "resultt")
                             (:file "optionalt")
                             (:file "free")
                             (:file "freet")))
               (:file "ordtree")
               (:file "ordmap")
               (:file "seq")
               (:file "system")
               (:file "file")
               (:file "show")
               (:file "prelude")))

(cl:when (cl:member (uiop:getenv "COALTON_PORTABLE_BIGFLOAT") '("1" "true" "t") :test #'cl:equalp)
  (cl:pushnew ':coalton-portable-bigfloat cl:*features*))

(asdf:defsystem "coalton/library/big-float"
  :description "An arbitrary precision floating point library."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t)
                          #+sbcl (sb-ext:*block-compile-default* :specified))
                      (funcall compile)))
  :depends-on ("coalton"
               "coalton/library"
               (:feature (:and (:not :coalton-portable-bigfloat) :sbcl) "sb-mpfr")
               (:feature (:and (:not :coalton-portable-bigfloat) :sbcl) "sb-gmp"))
  :pathname "library/big-float/"
  :serial t
  :components ((:file "package")
               (:file "impl-sbcl"
                :if-feature (:and (:not :coalton-portable-bigfloat) :sbcl))
               (:file "impl-default"
                :if-feature (:or :coalton-portable-bigfloat (:not :sbcl)))))

(asdf:defsystem "coalton/library/computable-reals"
  :description "A Coalton interface for computable-reals (https://github.com/stylewarning/computable-reals)"
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :pathname "library/computable-reals"
  :depends-on ("coalton"
               "computable-reals")
  :serial t
  :components ((:file "computable-reals")))

(asdf:defsystem "coalton/library/algorithms"
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t)
                          #+sbcl (sb-ext:*block-compile-default* ':specified))
                      (funcall compile)))
  :depends-on ("coalton"
               "coalton/library")
  :pathname "library/algorithms"
  :serial t
  :components ((:file "fft")))

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

(asdf:defsystem "coalton/benchmarks"
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t)
                          #+sbcl (sb-ext:*block-compile-default* :specified))
                      (funcall compile)))

  :depends-on ("coalton"
               "coalton/library/big-float"
               "trivial-benchmark"
               "yason")
  :pathname "benchmarks"
  :serial t
  :components ((:file "package")
               (:file "fibonacci")
               (:file "big-float")
               (:file "pi")
               (:file "seq")
               (:file "matrix")
               (:file "mapping")
               (:module "gabriel-benchmarks"
                :serial t
                :components ((:file "tak")
                             (:file "stak")
                             (:file "takl")
                             (:file "takr")))))

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
               "coalton/library/big-float"
               "coalton/library/computable-reals"
               "coalton/library/algorithms"
               "html-entities"
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
               (:file "hugo")
               (:file "main")))

(asdf:defsystem "coalton/tests"
  :description "Tests for COALTON."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :depends-on ("coalton"
               "coalton/library/big-float"
               "coalton/library/algorithms"
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
               (:file "tarjan-scc-tests")
               (:file "reader-tests")
               (:file "error-tests")
               (:module "parser"
                :serial t
                :components ((:file "cursor-tests")))
               (:file "entry-tests")
               (:file "toplevel-tests")
               (:file "type-inference-tests")
               (:file "fundep-tests")
               (:file "fundep-fib-test")
               (:file "runtime-tests")
               (:module "typechecker"
                :serial t
                :components ((:file "lisp-type-tests")))
               (:file "environment-persist-tests")
               (:file "coalton-tests")
               (:file "shortcut-tailcall-tests")
               (:file "slice-tests")
               (:file "float-tests")
               (:file "dual-tests")
               (:file "hyperdual-tests")
               (:file "quantize-tests")
               (:file "hashtable-tests")
               (:file "hashmap-tests")
               (:file "iterator-tests")
               (:file "call-coalton-from-lisp")
               (:file "bits-tests")
               (:file "vector-tests")
               (:file "string-tests")
               (:file "optional-tests")
               (:file "recursive-let-tests")
               (:file "class-tests")
               (:file "struct-tests")
               (:file "type-alias-tests")
               (:file "list-tests")
               (:file "lisparray-tests")
               (:file "red-black-tests")
               (:file "seq-tests")
               (:file "pattern-matching-tests")
               (:file "looping-native-tests")
               (:file "monomorphizer-tests")
               (:file "inliner-tests")
               (:file "inliner-tests-1") ; must come after inliner-tests
               (:file "deriver-tests")
               (:file "file-tests")
               (:file "experimental-tests")
               (:file "exceptions")
               (:module "monad"
                :serial t
                :components ((:file "optionalt")
                             (:file "resultt")
                             (:file "environment")))
               (:module "algorithms-tests"
                :serial t
                :components ((:file "fft-tests")))))

