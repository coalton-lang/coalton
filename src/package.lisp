(in-package #:cl-user)

;;;
;;; Internal packages
;;;

(uiop:define-package #:coalton-global-symbols
  (:documentation "A place that global value stores are stashed. We don't use uninterned symbols so that they can be reified through the compilation process.")
  (:use))

(uiop:define-package #:coalton-impl/algorithm
  (:documentation "Implementation of generic algorithms used by COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl)
  (:export
   #:tarjan-scc				; FUNCTION
   #:shadow-realm			; STRUCT
   #:shadow-realm-data			; ACCESSOR
   #:shadow-realm-parent		; ACCESSOR
   #:make-shadow-realm			; FUNCTION
   #:shadow-realm-set			; FUNCTION
   #:shadow-realm-push-frame 		; FUNCTION
   #:shadow-realm-lookup		; FUNCTION
   #:shadow-realm-diff                  ; FUNCTION
   #:shadow-realm-remove		; FUNCTION
   #:shadow-list			; STRUCT
   #:shadow-list-data                   ; ACCESSOR
   #:shadow-list-lookup			; FUNCTION
   #:shadow-list-push			; FUNCTION
   #:shadow-list-replace		; FUNCTION
   #:shadow-list-diff                   ; FUNCTION
   ))

(uiop:define-package #:coalton-impl/ast
    (:documentation "Implementation of the abstract syntax tree used by COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl #:coalton-impl/algorithm)
  (:export
   #:literal-value			; TYPE
   #:node				; STRUCT
   #:node-list                          ; TYPE
   #:node-unparsed			; ACCESSOR
   #:symbol-list                        ; TYPE
   #:binding-list                       ; TYPE
   #:node-literal			; STRUCT
   #:node-literal-value			; ACCESSOR
   #:node-variable			; STRUCT
   #:node-variable-name			; ACCESSOR
   #:node-application			; STRUCT
   #:node-application-rator		; ACCESSOR
   #:node-application-rands		; ACCESSOR
   #:node-abstraction			; STRUCT
   #:node-abstraction-vars		; ACCESSOR
   #:node-abstraction-subexpr		; ACCESSOR
   #:node-abstraction-name-map		; ACCESSOR
   #:node-let				; STRUCT
   #:node-let-bindings			; ACCESSOR
   #:node-let-subexpr			; ACCESSOR
   #:node-let-name-map			; ACCESSOR
   #:node-lisp				; STRUCT
   #:node-lisp-type			; ACCESSOR
   #:node-lisp-form			; ACCESSOR
   #:node-lisp-variables		; ACCESSOR
   #:node-match                         ; STRUCT
   #:node-match-expr                    ; ACCESSOR
   #:node-match-branches                ; ACCESSOR
   #:node-branch                        ; STRUCT
   #:node-branch-pattern                ; ACCESSOR
   #:node-branch-subexpr                ; ACCESSOR
   #:node-seq				; STRUCT
   #:node-seq-subnodes			; ACCESSOR
   #:match-branch-pattern		; ACCESSOR
   #:match-branch-subexpr		; ACCESSOR
   #:match-branch-unparsed		; ACCESOR
   #:match-branch-name-map		; ACCESSOR
   #:pattern				; STRUCT
   #:pattern-var			; STRUCT
   #:pattern-var-id			; ACCESSOR
   #:pattern-wildcard			; STRUCT
   #:pattern-literal			; STRUCT
   #:pattern-literal-value		; ACCESSOR
   #:pattern-constructor		; STRUCT
   #:pattern-constructor-name		; ACCESSOR
   #:pattern-constructor-patterns	; ACCESSOR
   #:pattern-variables                  ; FUNCTION
   )
  (:export
   #:parse-form                         ; FUNCTION
   #:coalton-parse-error                ; CONDITION
   #:coalton-parse-error-form           ; READER
   #:coalton-parse-error-reason-control ; READER
   #:coalton-parse-error-reason-args    ; READER
   #:with-parsing-context               ; MACRO
   )
  (:export
   #:bindings-to-dag			; FUNCTION
   ))

(uiop:define-package #:coalton-impl/typechecker
    (:documentation "Implementation of types and the typechecker for COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl
        #:coalton-impl/algorithm
        #:coalton-impl/ast)
  (:import-from #:serapeum
		#:true)
  (:export
   #:ty					; STRUCT
   #:ty-scheme                          ; STRUCT
   #:qualified-ty                       ; STRUCT
   #:function-type-p                    ; FUNCTION
   #:function-type-from                 ; FUNCTION
   #:function-type-to                   ; FUNCTION
   #:scheme-predicates                  ; FUNCTION
   )
  (:export   
   #:Unit				; SYMBOL
   #:Char				; SYMBOL
   #:Int				; SYMBOL
   #:String				; SYMBOL
   #:Lisp-Object                        ; SYMBOL
   #:ConsList                           ; SYMBOL
   )
  (:export
   #:typed-node                               ; STRUCT
   #:typed-binding-list                       ; TYPE
   #:typed-node-type                          ; ACCESSOR
   #:typed-node-unparsed		      ; ACCESSOR
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
   #:typed-node-abstraction-name-map	      ; ACCESSOR
   #:typed-node-bound-abstraction             ; STRUCT
   #:typed-node-bound-abstraction-vars        ; ACCESSOR
   #:typed-node-bound-abstraction-subexpr     ; ACCESSSOR
   #:typed-node-let                           ; STRUCT
   #:typed-node-let-bindings                  ; ACCESSOR
   #:typed-node-let-subexpr                   ; ACCESSOR
   #:typed-node-let-sorted-bindings           ; ACCESSOR
   #:typed-node-let-dynamic-extent-bindings   ; ACCESSOR
   #:typed-node-let-name-map		      ; ACCESSOR
   #:typed-node-lisp                          ; STRUCT
   #:typed-node-lisp-type                     ; ACCESSOR
   #:typed-node-lisp-form                     ; ACCESSOR
   #:typed-node-lisp-variables		      ; ACCESSOR
   #:typed-node-match                         ; STRUCT
   #:typed-node-match-expr                    ; ACCESSOR
   #:typed-node-match-branches                ; ACCESSOR
   #:typed-match-branch                       ; STRUCT
   #:typed-match-branch-pattern               ; ACCESSOR
   #:typed-match-branch-subexpr               ; ACCESSOR
   #:typed-match-branch-bindings              ; ACCESSOR
   #:typed-match-branch-unparsed	      ; ACCESSOR
   #:typed-match-branch-name-map	      ; ACCESSOR
   #:typed-node-seq			      ; STRUCT
   #:typed-node-seq-subnodes		      ; ACCESSOR
   )
  (:export
   #:environment			; STRUCT
   #:make-default-environment		; FUNCTION
   #:environment-diff                   ; FUNCTION
   #:environment-shadow			; FUNCTION
   #:generate-environment-update        ; FUNCTION
   #:set-value-type			; FUNCTION
   #:set-type                           ; FUNCTION
   #:set-function			; FUNCTION
   #:unset-function			; FUNCTION
   #:set-constructor                    ; FUNCTION
   #:set-name				; FUNCTION
   #:lookup-value-type			; FUNCTION
   #:lookup-type			; FUNCTION
   #:lookup-class                       ; FUNCTION
   #:lookup-constructor                 ; FUNCTION
   #:lookup-function			; FUNCTION
   #:lookup-class-instances             ; FUNCTION
   #:lookup-class-instance              ; FUNCTION
   #:lookup-name			; FUNCTION
   #:constructor-entry                  ; STRUCT
   #:constructor-entry-name		; ACCESSOR
   #:constructor-entry-arity            ; ACCESSOR
   #:constructor-entry-constructs	; ACCESSOR
   #:constructor-entry-scheme		; ACCESSOR
   #:constructor-entry-arguments	; ACCESSOR
   #:constructor-entry-classname	; ACCESSOR
   #:make-constructor-entry             ; CONSTRUCTOR
   #:constructor-entry-list             ; TYPE
   #:function-env-entry			; STRUCT
   #:function-env-entry-name		; ACCESSOR
   #:function-env-entry-arity		; ACCESSOR
   #:function-env-entry-list		; TYPE
   #:make-function-env-entry		; CONSTRUCTOR
   #:name-entry				; STRUCT
   #:name-entry-name			; ACCESSOR
   #:name-entry-type			; ACCESSOR
   #:make-name-entry			; CONSTRUCTOR
   #:directly-applicable-functions	; FUNCTION
   )
  (:export
   #:derive-expression-type             ; FUNCTION
   #:derive-bindings-type		; FUNCTION
   #:type=                              ; FUNCTION
   #:parse-and-resolve-type             ; FUNCTION
   #:parse-type-definitons              ; FUNCTION
   #:parse-class-signature              ; FUNCTION
   #:parse-class-definitions            ; FUNCTION
   #:parse-instance-definition          ; FUNCTION
   )
  (:export
   #:type-definition			; STRUCT
   #:type-definition-name		; ACCESSOR
   #:type-definition-type		; ACCESSOR
   #:type-definition-constructors	; ACCESSOR
   #:type-definition-list		; TYPE
   )
  (:export
   #:coalton-type-error                 ; SIGNAL
   #:unification-error			; SIGNAL
   #:type-application-error		; SIGNAL
   #:type-construction-error		; SIGNAL
   #:overlapping-instance-error		; SIGNAL
   #:cyclic-class-definitions-error     ; SIGNAL
   )
  (:export
   #:process-toplevel-definitions       ; FUNCTION
   )
  (:export
   #:ty-predicate			; STRUCT
   #:ty-predicate-class                 ; ACCESSOR
   #:ty-predicate-types                 ; ACCESSOR
   #:ty-predicate-list                  ; TYPE
   #:ty-class				; STRUCT
   #:ty-class-name			; ACCESSOR
   #:ty-class-predicate                 ; ACCESSOR
   #:ty-class-superclasses		; ACCESSOR
   #:ty-class-unqualified-methods       ; ACCESSOR
   #:ty-class-codegen-sym		; ACCESSOR
   #:ty-class-superclass-dict           ; ACCESSOR
   #:ty-class-list			; TYPE
   #:ty-class-instance			; STRUCT
   #:ty-class-instance-predicate	; ACCESSOR
   #:ty-class-instance-constraints	; ACCESSOR
   #:ty-class-instance-codegen-sym      ; ACCESSOR
   #:ty-class-instance-list		; TYPE
   )
  (:export
   #:by-super				; FUNCTION
   #:by-inst				; FUNCTION
   #:entail				; FUNCTION
   #:hnf-p				; FUNCTION
   #:to-hnf				; FUNCTION
   #:simplify-context			; FUNCTION
   #:reduce-context			; FUNCTION
   #:split-context			; FUNCTION
   #:apply-type-argument		; FUNCTION
   #:set-class
   #:add-class
   #:add-instance
   )
  (:export
   #:instance-definition		; STRUCT
   #:make-instance-definition		; CONSTRUCTOR
   #:instance-definition-class-name	; ACCESSOR
   #:instance-definition-predicate	; ACCESSOR
   #:instance-definition-context	; ACCESSOR
   #:instance-definition-methods	; ACCESSOR
   #:instance-definition-codegen-sym	; ACCESSOR
   #:instance-definition-list		; TYPE
   )
  ;; Pretty printers
  (:export
   #:with-pprint-variable-context       ; MACRO
   #:with-pprint-variable-scope         ; MACRO
   #:pprint-predicate			; FUNCTION
   )
  (:export
   #:*coalton-print-unicode*
   #:*coalton-pretty-print-tyvars*))

(uiop:define-package #:coalton-impl/codegen
  (:documentation "Implementation of code generation for COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl
        #:coalton-impl/algorithm
        #:coalton-impl/ast
        #:coalton-impl/typechecker)
  (:export
   #:construct-function-entry
   #:apply-function-entry
   #:codegen-program
   #:update-function-env
   )
  (:export
   #:toplevel-value-definition		; STRUCT
   #:make-toplevel-value-definition     ; FUNCTION
   #:toplevel-value-definition-name	; ACCESSOR
   #:toplevel-value-definition-type	; ACCESSOR
   #:toplevel-value-definition-node	; ACCESSOR
   #:toplevel-value-definition-list	; TYPE
   )
  (:export
   #:*emit-type-annotations*
   #:*freeze-emitted-types*
   ))

(uiop:define-package #:coalton-impl/doc
  (:documentation "Implementation of documentation generation for COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl
        #:coalton-impl/algorithm
        #:coalton-impl/ast
        #:coalton-impl/typechecker
        #:coalton-impl/codegen)
  (:export
   #:write-library-documentation-to-markdown))

(uiop:define-package #:coalton-impl
  (:documentation "Implementation and runtime for COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl
        #:coalton-impl/algorithm
        #:coalton-impl/ast
        #:coalton-impl/typechecker
        #:coalton-impl/codegen)
  (:import-from #:global-vars
                #:define-global-var
                #:define-global-var*)
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
   #:Unit
   #:Char
   #:Int
   #:String
   #:ConsList
   #:Arrow)
  ;; LISP-OBJECT
  (:export
   #:lisp-object
   #:veil
   #:unveil)
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
   #:coalton
   #:declare
   #:define
   #:define-type
   #:define-class
   #:define-instance)
  ;; Early Types
  (:export
   #:-> #:→
   #:=> #:⇒
   #:∀
   #:Unit
   #:Char
   #:Int
   #:Integer
   #:Single-Float
   #:Double-Float
   #:String
   ;; #:Lisp-Object                      ; See below.
   #:Arrow
   #:Void)
  (:export
   #:fn #:λ
   #:match
   #:let #:=
   #:if #:unless #:when #:cond
   #:lisp
   #:do #:<-
   #:_
   #:seq
   #:progn)
  (:import-from
   #:coalton-impl
   #:print-value-db
   #:print-type-db
   #:print-class-db
   #:print-instance-db
   #:lisp-object
   #:veil
   #:unveil)
  (:export
   #:print-value-db
   #:print-type-db
   #:print-class-db
   #:print-instance-db
   #:type-of
   #:kind-of
   #:lisp-object
   #:veil
   #:unveil))

(uiop:define-package #:coalton-library
  (:documentation "The Coalton standard library.")
  (:use #:coalton)
    ;; Macros
  (:export
   #:if
   #:unless
   #:when
   #:cond
   #:nest
   #:pipe
   #:make-list
   #:to-boolean
   #:do
   #:progn)
  ;; Types
  (:export
   #:Unit
   #:Boolean #:True #:False
   #:not
   #:or
   #:and
   #:xor
   #:List #:Cons #:Nil
   #:Tuple
   #:Optional #:Some #:None
   #:undefined
   )
  ;; Classes
  (:export
   #:Show
   #:Eq #:== #:/=
   #:Ord #:LT #:EQ #:GT
   #:<=> #:> #:< #:>= #:<=
   #:max
   #:min
   #:Num #:+ #:- #:* #:fromInt
   #:Semigroup #:<>
   #:Monoid #:mempty
   #:Functor #:map
   #:Applicative #:pure #:liftA2
   #:Monad #:>>= #:>>
   #:MonadFail #:fail
   #:Applicative #:alt #:empty
   #:Into
   #:TryInto)
  ;; Builtin
  (:export
   #:expt
   #:mod
   #:even
   #:odd
   #:gcd
   #:lcm)
  ;; String
  (:export
   #:concat-string
   #:unpack-string
   #:pack-string
   #:parse-int)
  ;; Optional
  (:export
   #:fromSome
   #:isSome
   #:isNone)
  ;; List
  (:export
   #:head
   #:tail
   #:null
   #:singleton
   #:repeat
   #:reverse
   #:find
   #:fold
   #:foldr
   #:filter
   #:length
   #:index
   #:elemIndex
   #:findIndex
   #:range
   #:append
   #:concat
   #:concatMap
   #:member
   #:union
   #:intersection
   #:lookup
   #:remove-duplicates
   #:delete
   #:list-difference
   #:zipWith
   #:zip
   #:insert
   #:insertBy
   #:sort
   #:sortBy
   #:intersperse
   #:intercalate
   #:transpose
   #:partition
   #:maximum
   #:minimum
   #:sum
   #:product
   #:all
   #:any)
  ;; Tuple
  (:export
   #:fst
   #:snd)
  ;; Result
  (:export
   #:Result #:Err #:Ok
   #:isOk
   #:isErr
   #:mapErr)
  ;; Functions
  (:export
   #:error
   #:fix
   #:id
   #:const
   #:flip
   #:compose
   #:traverse
   #:mapM
   #:liftM
   #:liftM2
   #:sequence
   #:mconcat
   #:asum)
  ;; Vector
  (:export
   #:Vector
   #:make-vector
   #:make-vector-capacity
   #:vector-length
   #:vector-capacity
   #:vector-empty
   #:vector-push
   #:vector-pop
   #:vector-pop-unsafe
   #:vector-index
   #:vector-index-unsafe
   #:vector-set
   #:vector-head
   #:vector-head-unsafe
   #:vector-last
   #:vector-last-unsafe
   #:vector-foreach
   #:vector-foreach-index
   #:vector-foreach2
   #:vector-append
   #:vector-to-list
   #:vector-swap-remove
   #:vector-swap-remove-unsafe)
  ;; Graph
  (:export
   #:NodeIndex
   #:EdgeIndex
   #:GraphType #:Undirected #:Directed
   #:Graph
   #:make-graph
   #:make-digraph
   #:graph-nodes
   #:graph-edges
   #:graph-number-count
   #:graph-edge-count
   #:graph-lookup-node
   #:graph-lookup-edge
   #:graph-add-node
   #:graph-remove-node
   #:graph-add-edge
   #:graph-remove-edge
   #:graph-viz))

(uiop:define-package #:coalton-user
  (:documentation "A default user package for Coalton.")
  (:use #:coalton #:coalton-library))

