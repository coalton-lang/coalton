(defpackage #:coalton-impl/typechecker/type-errors
  (:use
   #:cl
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/predicate)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error))
  (:import-from
   #:coalton-impl/error
   #:coalton-type-error)
  (:export
   #:coalton-type-error)
  (:export
   #:unknown-binding-error                ; CONDITION
   #:unification-error                    ; CONDITION
   #:infinite-type-unification-error      ; CONDITION
   #:type-declaration-too-general-error   ; CONDITION
   #:type-construction-error              ; CONDITION
   #:kind-mismatch-error                  ; CONDITION
   #:type-kind-mismatch-error             ; CONDITION
   #:invalid-operator-type-error          ; CONDITION
   #:predicate-unification-error          ; CONDITION
   #:overlapping-instance-error           ; CONDITION
   #:instance-missing-context-error       ; CONDITION
   #:cyclic-class-definitions-error       ; CONDITION
   #:invalid-typed-node-type              ; CONDITION
   #:unknown-type-variable                ; CONDITION
   #:unknown-constructor                  ; CONDITION
   #:invalid-constructor-arguments        ; CONDITION
   #:context-reduction-failure            ; CONDITION
   #:weak-context-error                   ; CONDITION
   #:recursive-binding-error              ; CONDITION
   #:self-recursive-toplevel-form         ; CONDITION
   #:self-recursive-non-constructor-call  ; CONDITION
   #:self-recursive-partial-application   ; CONDITION
   #:self-recursive-non-default-repr      ; CONDITION
   #:mutually-recursive-function-and-data ; CONDITION
   #:declared-type-missing-predicates     ; CONDITION
   #:declared-type-additional-predicates  ; CONDITION
   #:toplevel-monomorphism-restriction    ; CONDITION
   #:ambigious-constraint                 ; CONDITION
   #:ambigious-constraint-pred            ; ACCESSOR
   #:unresolvable-constraint              ; CONDITION
   #:duplicate-definition                 ; CONDITION
   #:unexpected-return                    ; CONDITION
   #:duplicate-ctor                       ; CONDITION
   #:coalton-type-parse-error             ; CONDITION
   #:error-parsing-type                   ; FUNCTION
   #:fundep-conflict                      ; CONDITION
   #:fundep-variable-error                ; CONDITION
   #:ambigious-constraint-variables       ; CONDITION
   #:invalid-fundep-change                ; CONDITION
   ))

(in-package #:coalton-impl/typechecker/type-errors)


(define-condition unknown-binding-error (coalton-type-error)
  ((symbol :initarg :symbol
           :reader unknown-binding-error-symbol)
   (alternatives :initarg :alternatives
                 :initform nil
                 :reader unknown-binding-error-alternatives))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           (symbol (unknown-binding-error-symbol c))
           (alts (unknown-binding-error-alternatives c)))
       (if (null alts)
           (format s "Unknown binding ~S" symbol)
           (format s "Unknown binding ~S. Did you mean ~{~S~^, ~}?" symbol alts))))))

(define-condition unification-error (coalton-type-error)
  ((type1 :initarg :type1
          :reader unification-error-type1)
   (type2 :initarg :type2
          :reader unification-error-type2))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Failed to unify types ~A and ~A"
               (unification-error-type1 c)
               (unification-error-type2 c))))))

(define-condition infinite-type-unification-error (coalton-type-error)
  ((type :initarg :type
         :reader infinite-type-unification-error-type))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Cannot construct infinite type by unifying ~A with internal variable."
               (infinite-type-unification-error-type c))))))

(define-condition type-declaration-too-general-error (coalton-type-error)
  ((name :initarg :name
         :reader type-declaration-too-general-error-name)
   (declared-type :initarg :declared-type
                  :reader type-declaration-too-general-error-declared-type)
   (derived-type :initarg :derived-type
                 :reader type-declaration-too-general-error-derived-type))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Type declaration for ~S of ~A is too general for the inferred type ~A"
               (type-declaration-too-general-error-name c)
               (type-declaration-too-general-error-declared-type c)
               (type-declaration-too-general-error-derived-type c))))))



(define-condition type-construction-error (coalton-type-error)
  ((type :initarg :type
         :reader type-construction-error-type))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Cannot construct partially applied type ~A of kind ~A"
               (type-construction-error-type c)
               (kind-of (type-construction-error-type c)))))))

(define-condition kind-mismatch-error (coalton-type-error)
  ((type :initarg :type
         :reader kind-mismatch-error-type)
   (kind :initarg :kind
         :reader kind-mismatch-error-kind))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Kind mismatch between type ~A of kind ~A and kind ~A"
               (kind-mismatch-error-type c)
               (kind-of (kind-mismatch-error-type c))
               (kind-mismatch-error-kind c))))))

(define-condition type-kind-mismatch-error (coalton-type-error)
  ((type1 :initarg :type1
          :reader type-kind-mismatch-error-type1)
   (type2 :initarg :type2
          :reader type-kind-mismatch-error-type2))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Kind mismatch between type ~A of kind ~A and type ~A kind ~A"
               (type-kind-mismatch-error-type1 c)
               (kind-of (type-kind-mismatch-error-type1 c))
               (type-kind-mismatch-error-type2 c)
               (kind-of (type-kind-mismatch-error-type2 c)))))))

(define-condition invalid-operator-type-error (coalton-type-error)
  ((type :initarg :type
         :reader invalid-operator-type-error-type))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Cannot call non-function operator of type ~A"
               (invalid-operator-type-error-type c))))))


(define-condition predicate-unification-error (coalton-type-error)
  ((pred1 :initarg :pred1
          :reader unification-error-pred1)
   (pred2 :initarg :pred2
          :reader unification-error-pred2))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Failed to unify types ~A and ~A"
               (unification-error-pred1 c)
               (unification-error-pred2 c))))))

(define-condition overlapping-instance-error (coalton-type-error)
  ((inst1 :initarg :inst1
          :reader overlapping-instance-error-inst1)
   (inst2 :initarg :inst2
          :reader overlapping-instance-error-inst2))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Instance ~A overlaps with instance ~A"
               (overlapping-instance-error-inst1 c)
               (overlapping-instance-error-inst2 c))))))

(define-condition instance-missing-context-error (coalton-type-error)
  ((pred :initarg :pred
         :reader instance-missing-context-error-pred)
   (super :initarg :super
          :reader instance-missing-context-error-super))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "No instance for ~A~%arising from context of superclass ~A"
               (instance-missing-context-error-pred c)
               (instance-missing-context-error-super c))))))

(define-condition cyclic-class-definitions-error (coalton-type-error)
  ((classes :initarg :classes
            :reader cyclic-class-definitions-error-classes))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Class definitions form cycle ~S"
               (cyclic-class-definitions-error-classes c))))))

(define-condition invalid-typed-node-type (coalton-type-error)
  ((node :initarg :node
         :reader invalid-typed-node-type-node)
   (inferred-type :initarg :inferred-type
                  :reader invalid-typed-node-type-inferred-type))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Type of node ~A does not match inferred type ~A"
               (invalid-typed-node-type-node c)
               (invalid-typed-node-type-inferred-type c))))))

(define-condition unknown-type-variable (coalton-type-error)
  ((node :initarg :node
         :reader unknown-type-variable-node)
   (variables :initarg :variables
              :reader unknown-type-variable-variables))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Node has unknown type variable. Node ~A variables ~A"
               (unknown-type-variable-node c)
               (unknown-type-variable-variables c))))))

(define-condition unknown-constructor (coalton-type-error)
  ((symbol :initarg :symbol
           :reader unknown-constructor-symbol))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Unknown symbol in match branch. ~A is not a constructor."
               (unknown-constructor-symbol c))))))

(define-condition invalid-constructor-arguments (coalton-type-error)
  ((constructor :initarg :constructor
                :reader invalid-constructor-arguments-constructor)
   (expected :initarg :expected
             :reader invalid-constructor-arguments-expected)
   (received :initarg :received
             :reader invalid-constructor-arguments-received))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Constructor ~A needs ~A arguments but got ~A."
               (invalid-constructor-arguments-constructor c)
               (invalid-constructor-arguments-expected c)
               (invalid-constructor-arguments-received c))))))

(define-condition context-reduction-failure (coalton-type-error)
  ((pred :initarg :pred
         :reader context-reduction-failure-pred))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Failed to reduce context for ~A"
               (context-reduction-failure-pred c))))))

(define-condition weak-context-error (coalton-type-error)
  ((name :initarg :name
         :reader weak-context-error-name)
   (declared-type :initarg :declared-type
                  :reader weak-context-error-declared-type)
   (preds :initarg :preds
          :reader weak-context-error-preds))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Explicit type of ~A~%for binding ~A missing inferred predicates ~{~A~^, ~}"
               (weak-context-error-declared-type c)
               (weak-context-error-name c)
               (weak-context-error-preds c))))))

(define-condition recursive-binding-error (coalton-type-error) ())

(define-condition self-recursive-variable-definition (recursive-binding-error)
    ((name :initarg :name
           :reader self-recursive-variable-definition-name)))

(define-condition self-recursive-toplevel-form (self-recursive-variable-definition)
  ()
  (:report (lambda (c s)
             (format s "Cannot recursively bind ~S in a toplevel form."
                     (self-recursive-variable-definition-name c)))))

(defparameter *self-recursive-binding-requirements-message*
  "Recursive data bindings may only be fully-applied direct applications of constructors for default-repr types, or `Cons'.")

(define-condition self-recursive-non-constructor-call (self-recursive-variable-definition)
  ((function :initarg :function
             :reader self-recursive-non-constructor-call-function))
  (:report (lambda (c s)
             (format s "Cannot recursively bind ~S to non-constructor function ~S.

~A"
                     (self-recursive-variable-definition-name c)
                     (self-recursive-non-constructor-call-function c)
                     *self-recursive-binding-requirements-message*))))

(define-condition self-recursive-partial-application (self-recursive-variable-definition)
  ((function :initarg :function
             :reader self-recursive-partial-application-function)
   (required-arg-count :initarg :required-arg-count
                       :reader self-recursive-partial-application-required-arg-count)
   (supplied-args :initarg :supplied-args
                  :reader self-recursive-partial-application-supplied-args))
  (:report (lambda (c s)
             (format s "Cannot recursively bind ~S to partial application of constructor ~S.

Wanted ~D arguments, but found only ~D: ~S.

~A"
                     (self-recursive-variable-definition-name c)
                     (self-recursive-partial-application-function c)
                     (self-recursive-partial-application-required-arg-count c)
                     (length (self-recursive-partial-application-supplied-args c))
                     (self-recursive-partial-application-supplied-args c)
                     *self-recursive-binding-requirements-message*))))

(define-condition self-recursive-non-default-repr (self-recursive-variable-definition)
  ((function :initarg :function
             :reader self-recursive-non-default-repr-function)
   (type :initarg :type
         :reader self-recursive-non-default-repr-type))
  (:report (lambda (c s)
             (format s "Cannot recursively bind ~S to application of constructor ~S for non-default-repr type ~S.

~A"
                     (self-recursive-variable-definition-name c)
                     (self-recursive-non-default-repr-function c)
                     (self-recursive-non-default-repr-type c)
                     *self-recursive-binding-requirements-message*))))

(define-condition mutually-recursive-function-and-data (coalton-type-error)
  ((names :initarg :names
          :reader mutually-recursive-function-and-data-names))
  (:report (lambda (c s)
             (format s "Cannot bind mutually recursive mixture of functions and data to names ~S.

Mutually recursive bindings must be either all functions or all constructor applications, never a mixture."
                     (mutually-recursive-function-and-data-names c)))))

(define-condition declared-type-missing-predicates (coalton-type-error)
  ((preds :initarg :preds
          :reader declared-type-missing-predicates-preds)
   (type :initarg :type
         :reader declared-type-missing-predicates-type))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Declared type ~A has missing predicates~{ ~A~}."
               (declared-type-missing-predicates-type c)
               (declared-type-missing-predicates-preds c))))))

(define-condition declared-type-additional-predicates (coalton-type-error)
  ((preds :initarg :preds
          :reader declared-type-additional-predicates-preds)
   (type :initarg :type
         :reader declared-type-additional-predicates-type))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Declared type ~A has extra predicates~{ ~A~}."
               (declared-type-additional-predicates-type c)
               (declared-type-additional-predicates-preds c))))))

(define-condition toplevel-monomorphism-restriction (coalton-type-error)
  ((name :initarg :name
         :reader toplevel-monomorphism-restriction-name
         :type symbol)
   (type :initarg :type
         :reader toplevel-monomorphism-restriction-type
         :type ty-scheme)
   (preds :initarg :preds
          :reader toplevel-monomorphism-restriction-preds
          :type ty-predicate-list))
  (:report
   (lambda (c s)
     (declare (notinline qualified-ty-predicates))
     (let* ((*print-circle* nil) ; Prevent printing using reader macros
            (name (toplevel-monomorphism-restriction-name c))
            (type (toplevel-monomorphism-restriction-type c))
            (preds (toplevel-monomorphism-restriction-preds c)))
       (format s "Unable to resolve ambiguous constraint~p~{ ~A~} in definition of ~A~%   with type ~A~%~%This can be resolved by giving ~A an explicit type declaration.~%"
               (length preds)
               preds
               name
               type
               name)))))

(define-condition ambigious-constraint (coalton-type-error)
  ((pred :initarg :pred
         :reader ambigious-constraint-pred))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Ambigious constraint ~A~%"
               (ambigious-constraint-pred c))))))

(define-condition unresolvable-constraint (coalton-type-error)
  ((pred :initarg :pred
         :reader unresolvable-constraint-pred))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Unresolvable constraint ~A"
               (unresolvable-constraint-pred c))))))



(define-condition duplicate-definition (coalton-type-error)
  ((name :initarg :name
         :reader duplicate-definition-name))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Duplicate definition of ~A"
               (duplicate-definition-name c))))))

(define-condition unexpected-return (coalton-type-error)
  ()
  (:report
   (lambda (c s)
     (declare (ignore c))
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Unexpected return statement")))))

(define-condition duplicate-ctor (coalton-type-error)
  ((ctor-name :initarg :ctor-name
              :reader duplicate-ctor-ctor-name
              :type symbol)
   (ty-name   :initarg :ty-name
              :reader duplicate-ctor-ty-name
              :type symbol))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Constructor ~A is already used in type ~A"
               (duplicate-ctor-ctor-name c)
               (duplicate-ctor-ty-name c))))))

(defun error-parsing-type (form reason-control &rest reason-args)
  (error 'coalton-type-parse-error
         :form form
         :reason-control reason-control
         :reason-args reason-args))

(define-condition fundep-conflict (coalton-type-error)
  ((new-pred :initarg :new-pred
             :reader fundep-conflict-new-pred
             :type ty-predicate)
   (old-pred :initarg :old-pred
             :reader fundep-conflict-old-pred
             :type ty-predicate)
   (fundep   :initarg :fundep
             :reader fundep-conflict-fundep
             :type t)
   (class    :initarg :class
             :reader fundep-conflict-class
             :type symbol)
   (class-vars :initarg :class-vars
               :reader fundep-conflict-class-vars
               :type util:symbol-list)
   (class-fundeps :initarg :class-fundeps
                  :reader fundep-conflict-class-fundeps
                  :type t)
   (old-from-tys :initarg :old-from-tys
                 :reader fundep-conflict-old-from-tys
                 :type ty-list)
   (new-from-tys :initarg :new-from-tys
                 :reader fundep-conflict-new-from-tys
                 :type ty-list)
   (old-to-tys :initarg :old-to-tys
               :reader fundep-conflict-old-to-tys
               :type ty-list)
   (new-to-tys :initarg :new-to-tys
               :reader fundep-conflict-new-to-tys
               :type ty-list))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (with-pprint-variable-context ()
         (format s "Instance ~A conflicts previous instance ~A~%the fundep ~S on class ~S~{ ~S~}~{ ~S~}~%binds~{ ~A~} to~{ ~A~}~%which conflicts with the previous binding of~{ ~A~} to~{ ~A~}"
                 (fundep-conflict-new-pred c)
                 (fundep-conflict-old-pred c)
                 (fundep-conflict-fundep c)
                 (fundep-conflict-class c)
                 (fundep-conflict-class-vars c)
                 (fundep-conflict-class-fundeps c)
                 (fundep-conflict-new-from-tys c)
                 (fundep-conflict-new-to-tys c)
                 (fundep-conflict-old-from-tys c)
                 (fundep-conflict-old-to-tys c)))))))

(define-condition fundep-variable-error (coalton-type-error)
  ((pred :initarg :pred
         :reader fundep-variable-error-pred
         :type ty-predicate)
   (vars :initarg :vars
         :reader fundep-variable-error-vars
         :type ty-list)
   (type :initarg :type
         :reader fundep-variable-error-type
         :type ty-list)
   (class :initarg :class
          :reader fundep-variable-error-class
          :type symbol)
   (class-vars :initarg :class-vars
               :reader fundep-variable-error-class-vars
               :type util:symbol-list)
   (class-fundeps :initarg :class-fundeps
                  :reader fundep-variable-error-class-fundeps
                  :type t)
   (fundep :initarg :fundep
         :reader fundep-variable-error-fundep
         :type t)
   (closure :initarg :closure
            :reader fundep-variable-error-closure
            :type util:symbol-list))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (with-pprint-variable-context ()
         (format s "The type variable~p~{ ~S~} do~A not appear in the type~p~{ ~S~}~%in the predicate ~S~%in definition of class ~S~{ ~S~}~{ ~S~}~%the fundep ~S binds class variables~{ ~S~}"
                 (length (fundep-variable-error-vars c))
                 (fundep-variable-error-vars c)
                 (if (= 1 (length (fundep-variable-error-vars c)))
                     "es"
                     "")
                 (length (fundep-variable-error-type c))
                 (fundep-variable-error-type c)
                 (fundep-variable-error-pred c)
                 (fundep-variable-error-class c)
                 (fundep-variable-error-class-vars c)
                 (fundep-variable-error-class-fundeps c)
                 (fundep-variable-error-fundep c)
                 (fundep-variable-error-closure c)))))))

(define-condition ambigious-constraint-variables (coalton-type-error)
  ((type :initarg :type
                  :reader ambigious-method-variables-type
                  :type (or symbol list qualified-ty))
   (variables :initarg :variables
              :reader ambigious-method-variables-variables))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (with-pprint-variable-context ()
         (format s "The variable~p~{ ~S~} ~A ambigious in the type ~S"
                 (length (ambigious-method-variables-variables c))
                 (ambigious-method-variables-variables c)
                 (if (= 1 (length (ambigious-method-variables-variables c)))
                     "is"
                     "are")
                 (ambigious-method-variables-type c)))))))

(define-condition invalid-fundep-change (error:coalton-error)
  ((name :initarg :name
         :reader invalid-fundep-change-name
         :type symbol))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Unable to change the functional dependencies on already defined class ~S"
               (invalid-fundep-change-name c))))))
