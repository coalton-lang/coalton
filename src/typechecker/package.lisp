(defpackage #:coalton-impl/typechecker
  (:documentation "Implementation of types and the typechecker for COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use
   #:cl
   #:coalton-impl/algorithm
   #:coalton-impl/ast)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:ty                                 ; STRUCT
   #:ty-list                            ; TYPE
   #:ty-binding-list                    ; TYPE
   #:tyvar                              ; STRUCT
   #:tyvar-id                           ; ACCESSOR
   #:tyvar-kind                         ; ACCESSOR
   #:tyvar-list                         ; TYPE
   #:make-variable                      ; FUNCTION
   #:tycon                              ; STRUCT
   #:make-tycon                         ; CONSTRUCTOR
   #:tycon-name                         ; ACCESSOR
   #:tycon-kind                         ; ACCESSOR
   #:tapp                               ; STRUCT
   #:make-tapp                          ; CONSTRUCTOR
   #:tapp-from                          ; ACCESSOR
   #:tapp-to                            ; ACCESSOR
   #:tgen                               ; STRUCT
   #:make-tgen                          ; CONSTRUCTOR
   #:tgen-id                            ; ACCESSOR
   #:kind-of                            ; FUNCTION
   #:apply-type-argument                ; FUNCTION
   #:apply-type-argument-list           ; FUNCTION
   #:make-function-type                 ; FUNCTION
   #:make-function-type*                ; FUNCTION
   #:function-type-p                    ; FUNCTION
   #:function-type-from                 ; FUNCTION
   #:function-type-to                   ; FUNCTION
   #:function-type-arigy                ; FUNCTION
   #:function-type-arguments            ; FUNCTION
   #:function-return-type               ; FUNCTION
   #:function-type-arity                ; FUNCTION
   #:unify                              ; FUNCTION
   #:apply-substitution                 ; FUNCTION
   ) 
  (:export
   #:*boolean-type*
   #:*char-type*
   #:*u8-type*
   #:*u16-type*
   #:*u32-type*
   #:*u64-type*
   #:*i8-type*
   #:*i16-type*
   #:*132-type*
   #:*164-type*
   #:*integer-type*
   #:*ifix-type*
   #:*ufix-type*
   #:*single-float-type*
   #:*double-float-type*
   #:*string-type*
   #:*fraction-type*
   #:*arrow-type*
   #:*list-type*)
  (:export
   #:kstar                              ; VARIABLE
   #:kind-arity                         ; FUNCTION
   #:ty-scheme                          ; STRUCT
   #:qualified-ty                       ; STRUCT
   #:scheme-predicates                  ; FUNCTION
   #:fresh-pred                         ; FUNCTION
   #:apply-substitution                 ; FUNCTION
   #:predicate-match                    ; FUNCTION
   #:make-function-type                 ; FUNCTION
   #:kfun                               ; STRUCT
   #:make-kfun                          ; CONSTRUCTOR
   #:fresh-inst                         ; FUNCTION
   #:qualified-ty-predicates            ; ACCESSOR
   #:qualified-ty-type                  ; ACCESSOR
   #:match                              ; FUNCTION
   #:ty-scheme-type                     ; ACCESSOR
   #:qualify                            ; FUNCTION
   #:to-scheme                          ; FUNCTION
   #:replace-node-type                  ; FUNCTION
   )
  (:export
   #:typed-node                           ; STRUCT
   #:typed-binding-list                   ; TYPE
   #:typed-node-type                      ; ACCESSOR
   #:typed-node-unparsed                  ; ACCESSOR
   #:typed-node-list                      ; TYPE
   #:typed-node-literal                   ; STRUCT
   #:typed-node-literal-value             ; ACCESSOR
   #:typed-node-variable                  ; STRUCT
   #:typed-node-variable-name             ; ACCESSOR
   #:typed-node-application               ; STRUCT
   #:typed-node-application-rator         ; ACCESSOR
   #:typed-node-application-rands         ; ACCESSOR
   #:typed-node-abstraction               ; STRUCT
   #:typed-node-abstraction-p             ; FUNCTION
   #:typed-node-abstraction-vars          ; ACCESSOR
   #:typed-node-abstraction-subexpr       ; ACCESSOR
   #:typed-node-abstraction-name-map      ; ACCESSOR
   #:typed-node-bound-abstraction         ; STRUCT
   #:typed-node-bound-abstraction-vars    ; ACCESSOR
   #:typed-node-bound-abstraction-subexpr ; ACCESSSOR
   #:typed-node-let                       ; STRUCT
   #:typed-node-let-bindings              ; ACCESSOR
   #:typed-node-let-subexpr               ; ACCESSOR
   #:typed-node-let-name-map              ; ACCESSOR
   #:typed-node-let-explicit-types        ; ACCESSOR
   #:typed-node-lisp                      ; STRUCT
   #:typed-node-lisp-type                 ; ACCESSOR
   #:typed-node-lisp-form                 ; ACCESSOR
   #:typed-node-lisp-variables            ; ACCESSOR
   #:typed-node-match                     ; STRUCT
   #:typed-node-match-expr                ; ACCESSOR
   #:typed-node-match-branches            ; ACCESSOR
   #:typed-match-branch                   ; STRUCT
   #:typed-match-branch-pattern           ; ACCESSOR
   #:typed-match-branch-subexpr           ; ACCESSOR
   #:typed-match-branch-bindings          ; ACCESSOR
   #:typed-match-branch-unparsed          ; ACCESSOR
   #:typed-match-branch-name-map          ; ACCESSOR
   #:typed-node-seq                       ; STRUCT
   #:typed-node-seq-subnodes              ; ACCESSOR
   #:typed-node-return                    ; STRUCT
   #:typed-node-return-expr               ; ACCESSSOR
   #:typed-node-bind                      ; STRUCT
   #:typed-node-bind-name                 ; ACCESSOR
   #:typed-node-bind-expr                 ; ACCESSOR
   #:typed-node-bind-body                 ; ACCESSOR
   )
  (:export
   #:environment                        ; STRUCT
   #:make-default-environment           ; FUNCTION
   #:environment-function-environment   ; ACCESSOR
   #:set-value-type                     ; FUNCTION
   #:set-type                           ; FUNCTION
   #:set-function                       ; FUNCTION
   #:unset-function                     ; FUNCTION
   #:set-constructor                    ; FUNCTION
   #:set-name                           ; FUNCTION
   #:set-method-inline                  ; FUNCTION
   #:set-code                           ; FUNCTION
   #:lookup-value-type                  ; FUNCTION
   #:lookup-type                        ; FUNCTION
   #:lookup-class                       ; FUNCTION
   #:lookup-constructor                 ; FUNCTION
   #:lookup-function                    ; FUNCTION
   #:lookup-class-instances             ; FUNCTION
   #:lookup-class-instance              ; FUNCTION
   #:lookup-instance-by-codegen-sym     ; FUNCTION
   #:lookup-name                        ; FUNCTION
   #:lookup-method-inline               ; FUNCTION
   #:lookup-code                        ; FUNCTION
   #:add-specialization                 ; FUNCTION
   #:lookup-specialization              ; FUNCTION
   #:lookup-specialization-by-type      ; FUNCTION
   #:type-entry                         ; STRUCT
   #:make-type-entry                    ; CONSTRUCTOR
   #:type-entry-name                    ; ACCESSOR
   #:type-entry-runtime-type            ; ACCESSOR
   #:type-entry-type                    ; ACCESSOR
   #:type-entry-enum-repr               ; ACCESSOR
   #:type-entry-newtype                 ; ACCESSOR
   #:type-entry-docstring               ; ACCESSOR
   #:type-entry-location                ; ACCESSOR
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
   #:code-entry                         ; STRUCT
   #:code-entry-name                    ; ACCESSOR
   #:code-entry-code                    ; ACCESSOR
   #:make-code-entry                    ; CONSTRUCTOR
   #:specialization-entry               ; STRUCT
   #:specialization-entry-from          ; ACCESSOR
   #:specialization-entry-to            ; ACCESSOR
   #:specialization-entry-to-ty         ; ACCESSOR
   #:make-specialization-entry          ; CONSTRUCTOR
   #:specialization-entry-list          ; TYPE
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
   #:parse-type-definitions               ; FUNCTION
   #:type-definition                      ; STRUCT
   #:type-definition-name                 ; ACCESSOR
   #:type-definition-type                 ; ACCESSOR
   #:type-definition-constructors         ; ACCESSOR
   #:type-definition-constructor-types    ; ACCESSOR
   #:type-definition-list                 ; TYPE
   #:type-definition-runtime-type         ; ACCESSOR
   #:type-definition-explicit-repr        ; ACCESSOR
   #:type-definition-enum-repr            ; ACCESSOR
   #:type-definition-newtype              ; ACCESSOR
   #:type-definition-docstring            ; ACCESSOR
   #:explicit-repr-auto-addressable-p     ; FUNCTION
   #:explicit-repr-explicit-addressable-p ; FUNCTION
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
   #:ty-class-superclass-map            ; ACCESSOR
   #:ty-class-list                      ; TYPE
   #:ty-class-instance                  ; STRUCT
   #:make-ty-class-instance             ; CONSTRUCTOR
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
   #:instance-definition                     ; STRUCT
   #:make-instance-definition                ; CONSTRUCTOR
   #:instance-definition-class-name          ; ACCESSOR
   #:instance-definition-predicate           ; ACCESSOR
   #:instance-definition-context             ; ACCESSOR
   #:instance-definition-methods             ; ACCESSOR
   #:instance-definition-codegen-sym         ; ACCESSOR
   #:instance-definition-method-codegen-syms ; ACCESSOR
   #:instance-definition-list                ; TYPE
   )
  (:export
   #:translation-unit                   ; STRUCT
   #:make-translation-unit              ; CONSTRUCTOR
   #:translation-unit-types             ; ACCESSOR
   #:translation-unit-definitions       ; ACCESSOR
   #:translation-unit-instances         ; ACCESSOR
   #:translation-unit-classes           ; ACCESSOR
   #:translation-unit-attr-table        ; ACCESSOR
   #:translation-unit-package           ; ACCESSOR
   )
  ;; Pretty printers
  (:export
   #:with-pprint-variable-context       ; MACRO
   #:with-pprint-variable-scope         ; MACRO
   #:pprint-predicate                   ; FUNCTION
   )
  (:export
   #:parse-define-form                  ; FUNCTION
   )
  (:export
   #:*coalton-print-unicode*
   #:*coalton-pretty-print-tyvars*)
  (:export
   #:print-value-db
   #:print-type-db
   #:print-class-db
   #:print-instance-db
   #:print-specializations))
