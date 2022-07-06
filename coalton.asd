;;;; coalton.asd

(asdf:defsystem #:coalton
  :description "An efficient, statically typed functional programming language that supercharges Common Lisp. "
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :in-order-to ((asdf:test-op (asdf:test-op #:coalton/tests)))
  :depends-on (#:coalton/compiler
               #:coalton/library))

(asdf:defsystem #:coalton/compiler
  :description "The Coalton compiler."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")

  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t)
                          #+sbcl (sb-ext:*block-compile-default* :specified))
                      (funcall compile)))
  :depends-on (#:alexandria
               #:trivia
               #:fset
               #:float-features
               #:split-sequence
               #:uiop)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "settings")
               (:file "utilities")
               (:file "global-lexical")
               (:module "algorithm"
                :serial t
                :components ((:file "tarjan-scc")
                             (:file "immutable-map")
                             (:file "immutable-listmap")))
               (:module "ast"
                :serial t
                :components ((:file "pattern")
                             (:file "node")
                             (:file "parse-error")
                             (:file "parse-form")
                             (:file "free-variables")))
               (:module "typechecker"
                :serial t
                :components ((:file "kinds")
                             (:file "type-errors")
                             (:file "types")
                             (:file "pretty-printing")
                             (:file "substitutions")
                             (:file "predicate")
                             (:file "scheme")
                             (:file "typed-node")
                             (:file "unify")
                             (:file "equality")
                             (:file "environment")
                             (:file "context-reduction")
                             (:file "type-parse-error")
                             (:file "parse-type")
                             (:file "parse-type-definition")
                             (:file "parse-class-definition")
                             (:file "parse-instance-definition")
                             (:file "derive-type")
                             (:file "debug")
                             (:file "translation-unit")))
               (:module "codegen"
                :serial t
                :components ((:file "ast")
                             (:file "ast-subsitutions")
                             (:file "resolve-instance")
                             (:file "typecheck-node")
                             (:file "hoister")
                             (:file "transformations")
                             (:file "compile-expression")
                             (:file "compile-instance")
                             (:file "function-entry")
                             (:file "lisp-type")
                             (:file "struct-or-class")
                             (:file "codegen-pattern")
                             (:file "codegen-type-definition")
                             (:file "codegen-expression")
                             (:file "codegen-class")
                             (:file "monomorphize")
                             (:file "optimizer")
                             (:file "program")
                             (:file "package")))
               (:file "toplevel-define-type")
               (:file "toplevel-declare")
               (:file "toplevel-define")
               (:file "toplevel-define-instance")
               (:file "toplevel-specializations")
               (:file "unlock-package" :if-feature :sb-package-locks)
               (:file "coalton")
               (:file "debug")
               (:file "faux-macros")
               (:file "language-macros")
               (:file "lock-package" :if-feature :sb-package-locks)))

(asdf:defsystem #:coalton/library
  :description "The Coalton standard library."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t)
                          #+sbcl (sb-ext:*block-compile-default* :specified))
                      (funcall compile)))
  :depends-on (#:coalton/compiler
               #:coalton/hashtable-shim
               #:trivial-garbage
               #:alexandria)
  :pathname "library/"
  :serial t
  :components ((:file "utils")
               (:file "fixed-size-numbers")
               (:file "classes")
               (:file "builtin")
               (:file "functions")
               (:file "boolean")
               (:file "bits")
               (:module "math"
                :serial t
                :components ((:file "arith")
                             (:file "num")
                             (:file "bounded") 
                             (:file "conversions")
                             (:file "fraction")
                             (:file "integral")
                             (:file "real")
                             (:file "complex")
                             (:file "elementary")))
               (:file "char")
               (:file "string")
               (:file "tuple")
               (:file "optional")
               (:file "list")
               (:file "result")
               (:file "addressable")
               (:file "stage-1")
               (:file "cell")
               (:file "vector")
               (:file "slice")
               (:file "hashtable")
               (:file "monad/state")
               (:file "iterator")
               (:file "system")
               (:file "prelude")))

(asdf:defsystem #:coalton/library/big-float
  :description "An arbitrary precision floating point library that uses SB-MPFR."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t)
                          #+sbcl (sb-ext:*block-compile-default* :specified))
                      (funcall compile)))
  :depends-on (#:coalton
               #:coalton/library
               #:sb-gmp
               #:sb-mpfr)
  :pathname "library/big-float/"
  :serial t
  :components ((:file "big-float")))

(asdf:defsystem #:coalton/testing
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :depends-on (#:coalton
               #:fiasco)
  :pathname "src/testing/"
  :serial t
  :components ((:file "package")
               (:file "coalton-native-test-utils")))

(asdf:defsystem #:coalton/benchmarks
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t)
                          #+sbcl (sb-ext:*block-compile-default* :specified))
                      (funcall compile)))

  :depends-on (#:coalton
               #:trivial-benchmark
               #:yason)
  :pathname "benchmarks"
  :serial t
  :components ((:file "package")))

;;; we need to inspect the sbcl version in order to decide which version of the hashtable shim to load,
;;; because 2.1.12 includes (or will include) a bugfix that allows a cleaner, more maintainable
;;; implementation.

(cl:if (uiop:featurep :sbcl)
       (cl:pushnew
        (cl:if (uiop:version< (cl:lisp-implementation-version)
                              "2.2.2")
               :sbcl-pre-2-2-2
               :sbcl-post-2-2-2)
        cl:*features*))

(asdf:defsystem #:coalton/hashtable-shim
  :description "Shim over Common Lisp hash tables with custom hash functions, for use by the Coalton standard library."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :pathname "src/hashtable-shim"
  :serial t
  :components ((:file "defs")
               (:file "impl-sbcl-pre-2-2-2" :if-feature :sbcl-pre-2-2-2)
               (:file "impl-sbcl-post-2-2-2" :if-feature :sbcl-post-2-2-2)
               (:file "impl-fail" :if-feature (:not :sbcl))))

(asdf:defsystem #:coalton/doc
  :description "Documentation generator for Coalton"
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :depends-on (#:coalton
               (:feature :sbcl #:coalton/library/big-float)
               #:html-entities
               #:yason
               #:uiop)
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/doc"
  :serial t
  :components ((:file "package")
               (:file "generate-documentation")
               (:file "markdown")
               (:file "hugo")))

(asdf:defsystem #:coalton/tests
  :description "Tests for COALTON."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :depends-on (#:coalton
               (:feature :sbcl #:coalton/library/big-float)
               #:coalton/testing
               #:fiasco
               #:coalton-json/tests
               #:quil-coalton/tests
               #:thih-coalton/tests)
  :perform (asdf:test-op (o s)
                         (unless (symbol-call :coalton-tests :run-coalton-tests)
                           (error "Tests failed")))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "toplevel-walker-tests")
               (:file "tarjan-scc-tests")
               (:file "type-inference-tests")
               (:file "runtime-tests")
               (:file "environment-persist-tests")
               (:file "slice-tests")
               (:file "float-tests")
               (:file "quantize-tests")
               (:file "hashtable-tests")
               (:file "iterator-tests")
               (:file "addressable-tests")
               (:file "call-coalton-from-lisp")
               (:file "vector-tests")
               (:file "recursive-let-tests")))
