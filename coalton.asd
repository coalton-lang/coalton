;;;; coalton.asd

(asdf:defsystem #:coalton
  :description "An efficient, statically typed functional programming language that supercharges Common Lisp. "
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :depends-on (#:alexandria
               #:global-vars
               #:trivia
               #:serapeum
               #:fset
               #:float-features)
  :in-order-to ((asdf:test-op (asdf:test-op #:coalton/tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "global-lexical")
               (:file "settings")
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
                             (:file "types")
                             (:file "pretty-printing")
                             (:file "substitutions")
                             (:file "predicate")
                             (:file "scheme")
                             (:file "typed-node")
                             (:file "type-errors")
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
                             (:file "check-types")
                             (:file "debug")))
               (:module "codegen"
                :serial t
                :components ((:file "utilities")
                             (:file "lisp-types")
                             (:file "function-entry")
                             (:file "optimizer")
                             (:file "direct-application")
                             (:file "match-constructor-lift")
                             (:file "pointfree-transform")
                             (:file "compile-pattern")
                             (:file "compile-typeclass-dicts")
                             (:file "compile-expression")
                             (:file "compile-type-definition")
                             (:file "program")))
               (:file "toplevel-define-type")
               (:file "toplevel-declare")
               (:file "toplevel-define")
               (:file "toplevel-define-instance")
               (:file "coalton")
               (:file "debug")
               (:module "doc"
                :serial t
                :components ((:file "generate-documentation")))
               (:file "faux-macros")
               (:module "library"
                :serial t
                :components ((:file "macros")
                             (:file "types")
                             (:file "classes")
                             (:file "boolean")
                             (:file "builtin")
                             (:file "fraction")
                             (:file "arith")
                             (:file "char")
                             (:file "string")
                             (:file "optional")
                             (:file "list")
                             (:file "tuple")
                             (:file "result")
                             (:file "functions")
                             (:file "cell")
                             (:file "vector")
                             (:file "slice")
                             (:file "hashtable")
                             (:file "graph")))
               (:file "toplevel-environment")))

(asdf:defsystem #:coalton/tests
  :description "Tests for COALTON."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :depends-on (#:coalton
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
               (:file "free-variables-tests")
               (:file "tarjan-scc-tests")
               (:file "type-inference-tests")
               (:file "environment-persist-tests")
               (:file "graph-tests")))
