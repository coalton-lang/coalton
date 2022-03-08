(in-package #:cl-user)

;;;
;;; Internal packages
;;;

(uiop:define-package #:coalton-global-symbols
  (:documentation "A place that global value stores are stashed. We don't use uninterned symbols so that they can be reified through the compilation process.")
  (:use))

(uiop:define-package #:coalton-impl/util
  (:use #:cl)
  (:export
   #:required
   #:unreachable
   #:coalton-bug
   #:sexp-fmt
   #:include-if
   #:define-symbol-property
   #:debug-log))

(uiop:define-package #:coalton-impl/algorithm
  (:documentation "Implementation of generic algorithms used by COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl #:coalton-impl/util)
  (:export
   #:tarjan-scc                         ; FUNCTION
   #:immutable-map                      ; STRUCT
   #:immutable-map-data                 ; ACCESSOR
   #:immutable-map-parent               ; ACCESSOR
   #:make-immutable-map                 ; FUNCTION
   #:immutable-map-set                  ; FUNCTION
   #:immutable-map-set-multiple         ; FUNCTION
   #:immutable-map-lookup               ; FUNCTION
   #:immutable-map-diff                 ; FUNCTION
   #:immutable-map-remove               ; FUNCTION
   #:immutable-listmap                  ; STRUCT
   #:make-immutable-listmap             ; CONSTRUCTOR
   #:immutable-listmap-data             ; ACCESSOR
   #:immutable-listmap-lookup           ; FUNCTION
   #:immutable-listmap-push             ; FUNCTION
   #:immutable-listmap-replace          ; FUNCTION
   #:immutable-listmap-diff             ; FUNCTION
   ))

(uiop:define-package #:coalton-impl/ast
    (:documentation "Implementation of the abstract syntax tree used by COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl
        #:coalton-impl/util
        #:coalton-impl/algorithm)
  (:export
   #:literal-value                      ; TYPE
   #:node                               ; STRUCT
   #:node-list                          ; TYPE
   #:node-unparsed                      ; ACCESSOR
   #:symbol-list                        ; TYPE
   #:binding-list                       ; TYPE
   #:node-literal                       ; STRUCT
   #:node-literal-value                 ; ACCESSOR
   #:node-variable                      ; STRUCT
   #:node-variable-name                 ; ACCESSOR
   #:node-application                   ; STRUCT
   #:node-application-rator             ; ACCESSOR
   #:node-application-rands             ; ACCESSOR
   #:node-abstraction                   ; STRUCT
   #:node-abstraction-vars              ; ACCESSOR
   #:node-abstraction-subexpr           ; ACCESSOR
   #:node-abstraction-name-map          ; ACCESSOR
   #:node-let                           ; STRUCT
   #:node-let-bindings                  ; ACCESSOR
   #:node-let-declared-types            ; ACCESSOR
   #:node-let-subexpr                   ; ACCESSOR
   #:node-let-name-map                  ; ACCESSOR
   #:node-lisp                          ; STRUCT
   #:node-lisp-type                     ; ACCESSOR
   #:node-lisp-form                     ; ACCESSOR
   #:node-lisp-variables                ; ACCESSOR
   #:node-match                         ; STRUCT
   #:node-match-expr                    ; ACCESSOR
   #:node-match-branches                ; ACCESSOR
   #:node-branch                        ; STRUCT
   #:node-branch-pattern                ; ACCESSOR
   #:node-branch-subexpr                ; ACCESSOR
   #:node-seq                           ; STRUCT
   #:node-seq-subnodes                  ; ACCESSOR
   #:node-the                           ; STRUCT
   #:node-the-type                      ; ACCESSOR
   #:node-the-subnode                   ; ACCESSOR
   #:match-branch-pattern               ; ACCESSOR
   #:match-branch-subexpr               ; ACCESSOR
   #:match-branch-unparsed              ; ACCESOR
   #:match-branch-name-map              ; ACCESSOR
   #:pattern                            ; STRUCT
   #:pattern-var                        ; STRUCT
   #:pattern-var-id                     ; ACCESSOR
   #:pattern-wildcard                   ; STRUCT
   #:pattern-literal                    ; STRUCT
   #:pattern-literal-value              ; ACCESSOR
   #:pattern-constructor                ; STRUCT
   #:pattern-constructor-name           ; ACCESSOR
   #:pattern-constructor-patterns       ; ACCESSOR
   #:pattern-variables                  ; FUNCTION
   )
  (:export
   #:parse-form                         ; FUNCTION
   #:error-parsing                      ; FUNCTION
   #:error-inherited-symbol             ; FUNCTION
   #:coalton-parse-error                ; CONDITION
   #:coalton-parse-error-form           ; READER
   #:coalton-parse-error-reason-control ; READER
   #:coalton-parse-error-reason-args    ; READER
   #:with-parsing-context               ; MACRO
   )
  (:export
   #:bindings-to-dag                    ; FUNCTION
   ))

(uiop:define-package #:coalton-impl/typechecker
    (:documentation "Implementation of types and the typechecker for COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl
        #:coalton-impl/util
        #:coalton-impl/algorithm
        #:coalton-impl/ast)
  (:export
   #:ty                                 ; STRUCT
   #:ty-scheme                          ; STRUCT
   #:qualified-ty                       ; STRUCT
   #:function-type-p                    ; FUNCTION
   #:function-type-from                 ; FUNCTION
   #:function-type-to                   ; FUNCTION
   #:scheme-predicates                  ; FUNCTION
   )
  (:export
   #:typed-node                               ; STRUCT
   #:typed-binding-list                       ; TYPE
   #:typed-node-type                          ; ACCESSOR
   #:typed-node-unparsed                      ; ACCESSOR
   #:typed-node-list                          ; TYPE
   #:typed-node-literal                       ; STRUCT
   #:typed-node-literal-value                 ; ACCESSOR
   #:typed-node-variable                      ; STRUCT
   #:typed-node-variable-name                 ; ACCESSOR
   #:typed-node-application                   ; STRUCT
   #:typed-node-application-rator             ; ACCESSOR
   #:typed-node-application-rands             ; ACCESSOR
   #:typed-node-direct-application            ; STRUCT
   #:typed-node-direct-application-rator-type ; ACCESSOR
   #:typed-node-direct-application-rator      ; ACCESSOR
   #:typed-node-direct-application-rands      ; ACCESSOR
   #:typed-node-abstraction                   ; STRUCT
   #:typed-node-abstraction-vars              ; ACCESSOR
   #:typed-node-abstraction-subexpr           ; ACCESSOR
   #:typed-node-abstraction-name-map          ; ACCESSOR
   #:typed-node-bound-abstraction             ; STRUCT
   #:typed-node-bound-abstraction-vars        ; ACCESSOR
   #:typed-node-bound-abstraction-subexpr     ; ACCESSSOR
   #:typed-node-let                           ; STRUCT
   #:typed-node-let-bindings                  ; ACCESSOR
   #:typed-node-let-subexpr                   ; ACCESSOR
   #:typed-node-let-sorted-bindings           ; ACCESSOR
   #:typed-node-let-dynamic-extent-bindings   ; ACCESSOR
   #:typed-node-let-name-map                  ; ACCESSOR
   #:typed-node-lisp                          ; STRUCT
   #:typed-node-lisp-type                     ; ACCESSOR
   #:typed-node-lisp-form                     ; ACCESSOR
   #:typed-node-lisp-variables                ; ACCESSOR
   #:typed-node-match                         ; STRUCT
   #:typed-node-match-expr                    ; ACCESSOR
   #:typed-node-match-branches                ; ACCESSOR
   #:typed-match-branch                       ; STRUCT
   #:typed-match-branch-pattern               ; ACCESSOR
   #:typed-match-branch-subexpr               ; ACCESSOR
   #:typed-match-branch-bindings              ; ACCESSOR
   #:typed-match-branch-unparsed              ; ACCESSOR
   #:typed-match-branch-name-map              ; ACCESSOR
   #:typed-node-seq                           ; STRUCT
   #:typed-node-seq-subnodes                  ; ACCESSOR
   )
  (:export
   #:environment                        ; STRUCT
   #:make-default-environment           ; FUNCTION
   #:environment-diff                   ; FUNCTION
   #:environment-shadow                 ; FUNCTION
   #:generate-environment-update        ; FUNCTION
   #:set-value-type                     ; FUNCTION
   #:set-type                           ; FUNCTION
   #:set-function                       ; FUNCTION
   #:unset-function                     ; FUNCTION
   #:set-constructor                    ; FUNCTION
   #:set-name                           ; FUNCTION
   #:lookup-value-type                  ; FUNCTION
   #:lookup-type                        ; FUNCTION
   #:lookup-class                       ; FUNCTION
   #:lookup-constructor                 ; FUNCTION
   #:lookup-function                    ; FUNCTION
   #:lookup-class-instances             ; FUNCTION
   #:lookup-class-instance              ; FUNCTION
   #:lookup-name                        ; FUNCTION
   #:type-entry
   #:type-entry-name                    ; STRUCT
   #:type-entry-runtime-type            ; ACCESSOR
   #:type-entry-type                    ; ACCESSOR
   #:type-entry-enum-repr               ; ACCESSOR
   #:type-entry-newtype                 ; ACCESSOR
   #:type-entry-docstring               ; ACCESSOR
   #:constructor-entry                  ; STRUCT
   #:constructor-entry-name             ; ACCESSOR
   #:constructor-entry-arity            ; ACCESSOR
   #:constructor-entry-constructs       ; ACCESSOR
   #:constructor-entry-classname        ; ACCESSOR
   #:constructor-entry-compressed-repr  ; ACCESSOR
   #:make-constructor-entry             ; CONSTRUCTOR
   #:constructor-entry-list             ; TYPE
   #:function-env-entry                 ; STRUCT
   #:function-env-entry-name            ; ACCESSOR
   #:function-env-entry-arity           ; ACCESSOR
   #:function-env-entry-list            ; TYPE
   #:make-function-env-entry            ; CONSTRUCTOR
   #:name-entry                         ; STRUCT
   #:name-entry-name                    ; ACCESSOR
   #:name-entry-type                    ; ACCESSOR
   #:name-entry-docstring               ; ACCESSOR
   #:make-name-entry                    ; CONSTRUCTOR
   #:directly-applicable-functions      ; FUNCTION
   )
  (:export
   #:derive-expression-type             ; FUNCTION
   #:derive-bindings-type               ; FUNCTION
   #:type=                              ; FUNCTION
   #:parse-and-resolve-type             ; FUNCTION
   #:parse-type-definitons              ; FUNCTION
   #:parse-class-signature              ; FUNCTION
   #:parse-class-definitions            ; FUNCTION
   #:parse-instance-definition          ; FUNCTION
   )
  (:export
   #:parse-type-definitions             ; FUNCTION
   #:type-definition                    ; STRUCT
   #:type-definition-name               ; ACCESSOR
   #:type-definition-type               ; ACCESSOR
   #:type-definition-constructors       ; ACCESSOR
   #:type-definition-constructor-types  ; ACCESSOR
   #:type-definition-list               ; TYPE
   #:type-definition-runtime-type       ; ACCESSOR
   #:type-definition-enum-repr          ; ACCESSOR
   #:type-definition-newtype            ; ACCESSOR
   #:type-definition-docstring          ; ACCESSOR
   )
  (:export
   #:coalton-type-error                 ; SIGNAL
   #:unification-error                  ; SIGNAL
   #:type-application-error             ; SIGNAL
   #:type-construction-error            ; SIGNAL
   #:overlapping-instance-error         ; SIGNAL
   #:cyclic-class-definitions-error     ; SIGNAL
   #:toplevel-monomorphism-restriction  ; SIGNAL
   #:duplicate-definition               ; SIGNAL
   #:with-type-context                  ; MACRO
   )
  (:export
   #:process-toplevel-definitions       ; FUNCTION
   )
  (:export
   #:ty-predicate                       ; STRUCT
   #:ty-predicate-class                 ; ACCESSOR
   #:ty-predicate-types                 ; ACCESSOR
   #:ty-predicate-list                  ; TYPE
   #:static-predicate-p                 ; FUNCTION
   #:ty-class                           ; STRUCT
   #:ty-class-name                      ; ACCESSOR
   #:ty-class-predicate                 ; ACCESSOR
   #:ty-class-superclasses              ; ACCESSOR
   #:ty-class-unqualified-methods       ; ACCESSOR
   #:ty-class-codegen-sym               ; ACCESSOR
   #:ty-class-superclass-dict           ; ACCESSOR
   #:ty-class-list                      ; TYPE
   #:ty-class-instance                  ; STRUCT
   #:ty-class-instance-predicate        ; ACCESSOR
   #:ty-class-instance-constraints      ; ACCESSOR
   #:ty-class-instance-codegen-sym      ; ACCESSOR
   #:ty-class-instance-list             ; TYPE
   )
  (:export
   #:by-super                           ; FUNCTION
   #:by-inst                            ; FUNCTION
   #:entail                             ; FUNCTION
   #:hnf-p                              ; FUNCTION
   #:to-hnf                             ; FUNCTION
   #:simplify-context                   ; FUNCTION
   #:reduce-context                     ; FUNCTION
   #:split-context                      ; FUNCTION
   #:apply-type-argument                ; FUNCTION
   #:set-class                          ; FUNCTION
   #:constructor-arguments              ; FUNCTION
   #:add-class                          ; FUNCTION
   #:add-instance                       ; FUNCTION
   )
  (:export
   #:instance-definition                ; STRUCT
   #:make-instance-definition           ; CONSTRUCTOR
   #:instance-definition-class-name     ; ACCESSOR
   #:instance-definition-predicate      ; ACCESSOR
   #:instance-definition-context        ; ACCESSOR
   #:instance-definition-methods        ; ACCESSOR
   #:instance-definition-codegen-sym    ; ACCESSOR
   #:instance-definition-list           ; TYPE
   )
  ;; Pretty printers
  (:export
   #:with-pprint-variable-context       ; MACRO
   #:with-pprint-variable-scope         ; MACRO
   #:pprint-predicate                   ; FUNCTION
   )
  (:export
   #:*coalton-print-unicode*
   #:*coalton-pretty-print-tyvars*))

(uiop:define-package #:coalton-impl/codegen
  (:documentation "Implementation of code generation for COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl
        #:coalton-impl/util
        #:coalton-impl/algorithm
        #:coalton-impl/ast
        #:coalton-impl/typechecker)
  (:export
   #:function-entry
   #:construct-function-entry
   #:apply-function-entry
   #:codegen-program
   #:compile-expression
   #:update-function-env
   #:a1 #:a2 #:a3 #:a4 #:a5 #:a6 #:a7 #:a8 #:a9 #:a10
   #:f1 #:f2 #:f3 #:f4 #:f5 #:f6 #:f7 #:f8 #:f9 #:f10
   )
  (:export
   #:toplevel-value-definition          ; STRUCT
   #:make-toplevel-value-definition     ; FUNCTION
   #:toplevel-value-definition-name     ; ACCESSOR
   #:toplevel-value-definition-type     ; ACCESSOR
   #:toplevel-value-definition-node     ; ACCESSOR
   #:toplevel-value-definition-list     ; TYPE
   )
  (:export
   #:*emit-type-annotations*
   ))

(uiop:define-package #:coalton-impl
  (:documentation "Implementation and runtime for COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl
        #:coalton-impl/util
        #:coalton-impl/algorithm
        #:coalton-impl/ast
        #:coalton-impl/typechecker
        #:coalton-impl/codegen)
  (:import-from #:global-vars
                #:define-global-var
                #:define-global-var*)
  ;; settings
  (:export
   #:coalton-release-p)

  (:export
   #:coalton-parse-error                ; CONDITION
   #:coalton-parse-error-form           ; READER
   #:coalton-parse-error-reason-control ; READER
   #:coalton-parse-error-reason-args    ; READER
   #:coalton-type-error                 ; CONDITION
   #:unification-error                  ; CONDITION
   #:unification-error-first-type       ; READER
   #:unification-error-second-type      ; READER
   #:type-mismatch                      ; CONDITION
   #:type-mismatch-types                ; READER
   #:arity-mismatch                     ; CONDITION
   #:arity-mismatch-arities             ; READER
   #:non-terminating-unification-error  ; CONDITION
   #:non-terminating-unification-error-contained-type
                                        ; READER
   #:non-terminating-unification-error-containing-type
                                        ; READER
   )
  (:export
   #:print-value-db
   #:print-type-db
   #:print-class-db
   #:print-instance-db
   )
  (:export
   #:*emit-type-annotations*
   #:*freeze-emitted-types*
   #:*coalton-print-unicode*
   ))

;;
;; External Packages
;;

(uiop:define-package #:coalton
  (:documentation "Public interface to COALTON.")
  (:use)                                ; Keep the package clean!
  (:export
   #:coalton-toplevel
   #:coalton-codegen
   #:coalton-codegen-types
   #:coalton-codegen-ast
   #:coalton
   #:declare
   #:define
   #:define-type
   #:define-class
   #:define-instance
   #:repr
   #:unable-to-codegen)
  ;; Early Types
  (:export
   #:-> #:→
   #:=> #:⇒
   #:∀
   #:Unit
   #:Void
   #:Boolean #:True #:False
   #:Char
   #:U8
   #:U16
   #:U32
   #:U64
   #:I8
   #:I16
   #:I32
   #:I64
   #:Integer
   #:IFix
   #:UFix
   #:Single-Float
   #:Double-Float
   #:String
   #:Arrow
   #:Void
   #:Lisp-Object
   #:List #:Cons #:Nil)
  (:export
   #:fn #:λ
   #:match
   #:let
   #:=                                  ; Syntax
   #:lisp
   #:<-                                 ; Syntax
   #:_
   #:seq
   #:the)
  ;; Macros
  (:export
   #:if
   #:when
   #:unless
   #:and
   #:or
   #:cond
   #:nest
   #:pipe
   #:.<
   #:.>
   #:make-list
   #:to-boolean
   #:do
   #:progn
   #:assert)
  (:import-from
   #:coalton-impl
   #:print-value-db
   #:print-type-db
   #:print-class-db
   #:print-instance-db)
  (:export
   #:print-value-db
   #:print-type-db
   #:print-class-db
   #:print-instance-db
   #:type-of
   #:kind-of))
