(defpackage #:coalton-impl/typechecker/translation-unit
  (:use
   #:cl
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/environment
   #:coalton-impl/typechecker/expression
   #:coalton-impl/typechecker/define
   #:coalton-impl/typechecker/define-type
   #:coalton-impl/typechecker/toplevel)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:translation-unit                   ; STRUCT
   #:make-translation-unit              ; CONSTRUCTOR
   #:translation-unit-types             ; ACCESSOR
   #:translation-unit-definitions       ; ACCESSOR
   #:translation-unit-instances         ; ACCESSOR
   #:translation-unit-classes           ; ACCESSOR
   #:translation-unit-attr-table        ; ACCESSOR
   #:translation-unit-package           ; ACCESSOR
   #:translation-unit-specializations   ; ACCESSOR
   ))

(in-package #:coalton-impl/typechecker/translation-unit)

(defstruct translation-unit
  (types           nil                      :type type-definition-list           :read-only t)
  (definitions     nil                      :type toplevel-define-list           :read-only t)
  (instances       nil                      :type toplevel-define-instance-list  :read-only t)
  (classes         nil                      :type ty-class-list                  :read-only t)
  (package         (util:required 'package) :type package                        :read-only t)
  (specializations nil                      :type specialization-entry-list      :read-only t))
