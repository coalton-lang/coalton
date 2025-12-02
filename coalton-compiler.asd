;;; The Coalton compiler. See coalton.asd for the toplevel Coalton package.

(asdf:defsystem "coalton-compiler"
  :description "The Coalton compiler."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")

  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t)
                          #+sbcl (sb-ext:*block-compile-default* :specified))
                      (funcall compile)))
  :depends-on (
                                        ; This causes a circular def
                                        ; for some reason.
               ;; "coalton/compatibility-layer"
                                        ; But this one works - go
                                        ; figure...
                                        ;
                                        ; Strangely enough, it doesn't
                                        ; seem to appear anywhere in
                                        ; the loading chain (maybe
                                        ; that one is showing the
                                        ; packages instead of the
                                        ; systems).
               "coalton-compatibility-layer"
               "alexandria"
               "concrete-syntax-tree"
               "eclector"
               "eclector-concrete-syntax-tree"
               "float-features"
               "fset"
               "named-readtables"
               "source-error"
               "trivial-gray-streams")
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "settings")
               (:file "utilities")
               (:file "source")
               (:file "global-lexical")
               (:file "constants")
               (:module "algorithm"
                :serial t
                :components ((:file "tarjan-scc")
                             (:file "immutable-map")
                             (:file "immutable-listmap")
                             (:file "package")))
               (:module "parser"
                :serial t
                :components ((:file "base")
                             (:file "cursor")
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
                             (:file "optional")
                             (:file "package")))
               (:module "typechecker"
                :serial t
                :components ((:file "base")
                             (:file "kinds")
                             (:file "types")
                             (:file "substitutions")
                             (:file "predicate")
                             (:file "scheme")
                             (:file "type-errors")
                             (:file "unify")
                             (:file "fundeps")
                             (:file "environment")

                             (:module "redef-detection"
                              :pathname "../redef-detection/"
                              :serial t
                              :components ((:file "dependencies")
                                           (:file "compatibility")
                                           (:file "conditions")
                                           (:file "package")))

                             (:file "lisp-type")
                             (:file "context-reduction")
                             (:file "stage-1")
                             (:file "pattern")
                             (:file "expression")
                             (:file "traverse")
                             (:file "toplevel")
                             (:file "binding")
                             (:file "accessor")
                             (:file "partial-type-env")
                             (:file "parse-type")
                             (:file "derive")
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
                             (:file "traverse")
                             (:file "transformations")
                             (:file "ast-substitutions")
                             (:file "resolve-instance")
                             (:file "typecheck-node")
                             (:file "hoister")
                             (:file "translate-expression")
                             (:file "translate-instance")
                             (:file "struct-or-class")
                             (:file "codegen-exception")
                             (:file "codegen-pattern")
                             (:file "codegen-type-definition")
                             (:file "codegen-match")
                             (:file "codegen-expression")
                             (:file "codegen-class")
                             (:file "intrinsic-applications")

                             ;; Optimizations
                             (:file "monomorphize")
                             (:file "constant-propagation")
                             (:file "canonicalizer")
                             (:file "inliner")
                             (:file "specializer")
                             (:file "optimizer")

                             ;; Entry points
                             (:file "program")
                             (:file "package")))
               (:file "unlock-package" :if-feature :sb-package-locks)
               (:file "entry")
               (:file "reader")
               (:file "debug")
               (:file "faux-macros")
               (:file "intrinsic-applications")
               (:file "define-coalton-macro")
               (:file "language-macros")
               (:file "lock-package" :if-feature :sb-package-locks)))

;; Two systems for the same package (aka Dr Jekyll & Mr Hyde).
;;
;; Because we can.
;;
;; And we want to because ASDF doesn't like the original
;; (coalton/compatibility-layer) system - it's
;; ``circular''. (huh?!?!?)
(asdf:defsystem "coalton-compatibility-layer"
  :description "The Coalton compatibility layer."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :pathname "compat/"
  :components ((:file "compatibility-layer")))
