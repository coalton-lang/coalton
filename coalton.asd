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
               #:fset
               #:float-features
               #:eclector
               #:concrete-syntax-tree
               #:eclector-concrete-syntax-tree
               #:named-readtables)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "settings")
               (:file "utilities")
               (:file "global-lexical")
               (:file "constants")
               (:module "algorithm"
                :serial t
                :components ((:file "tarjan-scc")
                             (:file "immutable-map")
                             (:file "immutable-listmap")
                             (:file "package")))
               (:file "error")
               (:module "parser"
                :serial t
                :components ((:file "base")
                             (:file "reader")
                             (:file "types")
                             (:file "pattern")
                             (:file "macro")
                             (:file "expression")
                             (:file "toplevel")
                             (:file "collect")
                             (:file "renamer")
                             (:file "binding")
                             (:file "type-definition")
                             (:file "package")))
               (:module "runtime"
                :serial t
                :components ((:file "function-entry")
                             (:file "package")))
               (:module "typechecker"
                :serial t
                :components ((:file "kinds")
                             (:file "types")
                             (:file "substitutions")
                             (:file "predicate")
                             (:file "scheme")
                             (:file "type-errors")
                             (:file "unify")
                             (:file "fundeps")
                             (:file "environment")
                             (:file "lisp-type")
                             (:file "context-reduction")
                             (:file "stage-1")
                             (:file "base")
                             (:file "pattern")
                             (:file "expression")
                             (:file "traverse")
                             (:file "toplevel")
                             (:file "binding")
                             (:file "accessor")
                             (:file "partial-type-env")
                             (:file "parse-type")
                             (:file "define-type")
                             (:file "define-class")
                             (:file "tc-env")
                             (:file "define")
                             (:file "define-instance")
                             (:file "specialize")
                             (:file "translation-unit")
                             (:file "package")))
               (:module "analysis"
                :serial t
                :components ((:file "pattern-exhaustiveness")
                             (:file "unused-variables")
                             (:file "underapplied-values")
                             (:file "analysis")
                             (:file "package")))
               (:module "codegen"
                :serial t
                :components ((:file "pattern")
                             (:file "ast")
                             (:file "ast-subsitutions")
                             (:file "resolve-instance")
                             (:file "typecheck-node")
                             (:file "hoister")
                             (:file "transformations")
                             (:file "translate-expression")
                             (:file "translate-instance")
                             (:file "struct-or-class")
                             (:file "codegen-pattern")
                             (:file "codegen-type-definition")
                             (:file "codegen-expression")
                             (:file "codegen-class")
                             (:file "monomorphize")
                             (:file "optimizer")
                             (:file "program")
                             (:file "package")))
               (:file "unlock-package" :if-feature :sb-package-locks)
               (:file "entry")
               (:file "reader")
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
  :components ((:file "set-float-traps")
               (:file "utils")
               (:file "types")
               (:file "primitive-types")
               (:file "classes")
               (:file "hash")
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
                             (:file "elementary")
                             (:file "dyadic")
                             (:file "dual")))
               (:file "randomaccess")
               (:file "cell")
               (:file "iterator")
               (:file "optional")
               (:file "result")
               (:file "tuple")
               (:file "lisparray")
               (:file "list")
               (:file "vector")
               (:file "char")
               (:file "string")
               (:file "slice")
               (:file "hashtable")
               (:file "queue")
               (:file "monad/state")
               (:file "ord-tree")
               (:file "ord-map")
               (:file "monad/free")
               (:file "seq")
               (:file "system")
               (:file "prelude")))

(when (member (getenv "COALTON_PORTABLE_BIGFLOAT") '("1" "true" "t") :test 'equalp)
  (pushnew :coalton-portable-bigfloat *features*))

(asdf:defsystem #:coalton/library/big-float
  :description "An arbitrary precision floating point library."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t)
                          #+sbcl (sb-ext:*block-compile-default* :specified))
                      (funcall compile)))
  :depends-on (#:coalton
               #:coalton/library
               (:feature (:and (:not :coalton-portable-bigfloat) :sbcl) #:sb-mpfr)
               (:feature (:and (:not :coalton-portable-bigfloat) :sbcl) #:sb-gmp))
  :pathname "library/big-float/"
  :serial t
  :components ((:file "package")
               (:file "impl-sbcl"
                :if-feature (:and (:not :coalton-portable-bigfloat) :sbcl))
               (:file "impl-default"
                :if-feature (:or :coalton-portable-bigfloat (:not :sbcl)))))

(asdf:defsystem #:coalton/library/computable-reals
  :description "A Coalton interface for computable-reals (https://github.com/stylewarning/computable-reals)"
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :pathname "library/computable-reals"
  :depends-on (#:coalton
               #:computable-reals)
  :serial t
  :components ((:file "computable-reals")))

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
               #:coalton/library/big-float
               #:trivial-benchmark
               #:yason)
  :pathname "benchmarks"
  :serial t
  :components ((:file "package")
               (:file "fibonacci")
               (:file "big-float")
               (:module "gabriel-benchmarks"
                        :serial t
                        :components ((:file "tak")
                                     (:file "stak")
                                     (:file "takl")
                                     (:file "takr")))))

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
               (:file "impl-sbcl" :if-feature :sbcl)
               (:file "hash-table" :if-feature (:not :sbcl))
               (:file "impl-custom" :if-feature (:not :sbcl))))

(asdf:defsystem #:coalton/doc
  :description "Documentation generator for Coalton"
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :depends-on (#:coalton
               #:coalton/library/big-float
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
               #:coalton/library/big-float
               #:coalton/testing
               #:fiasco
               #:quil-coalton/tests
               #:thih-coalton/tests)
  :perform (asdf:test-op (o s)
                         (unless (symbol-call :coalton-tests :run-coalton-tests)
                           (error "Tests failed")))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "tarjan-scc-tests")
               (:file "reader-tests")
               (:file "error-tests")
               (:file "parser-tests")
               (:file "type-inference-tests")
               (:file "fundep-tests")
               (:file "fundep-fib-test")
               (:file "runtime-tests")
               (:file "environment-persist-tests")
               (:file "slice-tests")
               (:file "float-tests")
               (:file "dual-tests")
               (:file "quantize-tests")
               (:file "hashtable-tests")
               (:file "iterator-tests")
               (:file "call-coalton-from-lisp")
               (:file "vector-tests")
               (:file "string-tests")
               (:file "recursive-let-tests")
               (:file "class-tests")
               (:file "list-tests")
               (:file "red-black-tests")
               (:file "seq-tests")
               (:file "unused-variables")
               (:file "pattern-matching-tests")
               (:file "looping-native-tests")))
