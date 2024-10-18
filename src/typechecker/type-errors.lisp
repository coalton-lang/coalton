(defpackage #:coalton-impl/typechecker/type-errors
  (:use
   #:cl
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/predicate)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:unification-error                  ; CONDITION
   #:infinite-type-unification-error    ; CONDITION
   #:type-declaration-too-general-error ; CONDITION
   #:type-construction-error            ; CONDITION
   #:kind-mismatch-error                ; CONDITION
   #:type-kind-mismatch-error           ; CONDITION
   #:predicate-unification-error        ; CONDITION
   )
  (:export
   #:overlapping-instance-error                ; CONDITION
   #:overlapping-instance-error-inst1          ; ACCESSOR
   #:overlapping-instance-error-inst2          ; ACCESSOR
   #:ambiguous-constraint                      ; CONDITION
   #:ambiguous-constraint-pred                 ; ACCESSOR
   #:fundep-conflict                           ; CONDITION
   #:overlapping-specialization-error          ; CONDITION
   #:overlapping-specialization-error-new      ; ACCESSOR
   #:overlapping-specialization-error-existing ; ACCESSOR
   ))

(in-package #:coalton-impl/typechecker/type-errors)

(define-condition unification-error (coalton-internal-type-error)
  ((type1 :initarg :type1
          :reader unification-error-type1)
   (type2 :initarg :type2
          :reader unification-error-type2))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           (*print-readably* nil)
           (*pprint-type-aliases* nil))
       (format s "Failed to unify types ~S and ~S"
               (unification-error-type1 c)
               (unification-error-type2 c))))))

(define-condition infinite-type-unification-error (coalton-internal-type-error)
  ((type :initarg :type
         :reader infinite-type-unification-error-type))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           (*print-readably* nil)
           (*pprint-type-aliases* nil))
       (format s "Cannot construct infinite type by unifying ~S with internal variable."
               (infinite-type-unification-error-type c))))))


(define-condition kind-mismatch-error (coalton-internal-type-error)
  ((type :initarg :type
         :reader kind-mismatch-error-type)
   (kind :initarg :kind
         :reader kind-mismatch-error-kind))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           (*print-readably* nil)
           (*pprint-type-aliases* nil))
       (format s "Kind mismatch between type ~S of kind ~S and kind ~S"
               (kind-mismatch-error-type c)
               (kind-of (kind-mismatch-error-type c))
               (kind-mismatch-error-kind c))))))

(define-condition type-kind-mismatch-error (coalton-internal-type-error)
  ((type1 :initarg :type1
          :reader type-kind-mismatch-error-type1)
   (type2 :initarg :type2
          :reader type-kind-mismatch-error-type2))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           (*print-readably* nil)
           (*pprint-type-aliases* nil))
       (format s "Kind mismatch between type ~S of kind ~S and type ~S kind ~S"
               (type-kind-mismatch-error-type1 c)
               (kind-of (type-kind-mismatch-error-type1 c))
               (type-kind-mismatch-error-type2 c)
               (kind-of (type-kind-mismatch-error-type2 c)))))))

(define-condition predicate-unification-error (coalton-internal-type-error)
  ((pred1 :initarg :pred1
          :reader unification-error-pred1)
   (pred2 :initarg :pred2
          :reader unification-error-pred2))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           (*print-readably* nil)
           (*pprint-type-aliases* nil))
       (format s "Failed to unify types ~S and ~S"
               (unification-error-pred1 c)
               (unification-error-pred2 c))))))

(define-condition ambiguous-constraint (coalton-internal-type-error)
  ((pred :initarg :pred
         :reader ambiguous-constraint-pred))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           (*print-readably* nil)
           (*pprint-type-aliases* nil))
       (format s "Ambiguous constraint ~S~%"
               (ambiguous-constraint-pred c))))))

(define-condition overlapping-instance-error (coalton-internal-type-error)
  ((inst1 :initarg :inst1
          :reader overlapping-instance-error-inst1)
   (inst2 :initarg :inst2
          :reader overlapping-instance-error-inst2))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           (*print-readably* nil)
           (*pprint-type-aliases* nil))
       (format s "Instance ~S overlaps with instance ~S"
               (overlapping-instance-error-inst1 c)
               (overlapping-instance-error-inst2 c))))))

(define-condition fundep-conflict (coalton-internal-type-error)
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
           (*print-readably* nil)
           (*pprint-type-aliases* nil))
       (with-pprint-variable-context ()
         (format s "instance conflicts previous instance ~S"
                 (fundep-conflict-old-pred c)))))))

(define-condition overlapping-specialization-error (coalton-internal-type-error)
  ((new :initarg :new
        :reader overlapping-specialization-error-new)
   (existing :initarg :existing
             :reader overlapping-specialization-error-existing)))
