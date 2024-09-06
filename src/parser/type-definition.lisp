;;;;
;;;; This file defines a generic protocol for writing code that
;;;; operates on both toplevel-define-type and toplevel-define-struct
;;;; structs.
;;;;

(defpackage #:coalton-impl/parser/type-definition
  (:use
   #:cl
   #:coalton-impl/parser/base
   #:coalton-impl/parser/types
   #:coalton-impl/parser/toplevel)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:export
   #:type-definition                    ; TYPE
   #:type-definition-list               ; TYPE
   #:type-definition-name               ; FUNCTION
   #:type-definition-vars               ; FUNCTION
   #:type-definition-repr               ; FUNCTION
   #:type-definition-ctors              ; FUNCTION
   #:type-definition-ctor-name          ; FUNCTION
   #:type-definition-ctor-field-types   ; FUNCTION
   ))

(in-package #:coalton-impl/parser/type-definition)

(deftype type-definition ()
  '(or toplevel-define-type toplevel-define-struct toplevel-define-alias))

(defun type-definition-p (x)
  (typep x 'type-definition))

(defun type-definition-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'type-definition-p x)))

(deftype type-definition-list ()
  '(satisfies type-definition-list-p))

(defgeneric type-definition-name (def)
  (:method ((def toplevel-define-type))
    (declare (values identifier-src))
    (toplevel-define-type-name def))

  (:method ((def toplevel-define-alias))
    (declare (values identifier-src))
    (toplevel-define-alias-name def))

  (:method ((def toplevel-define-struct))
    (declare (values identifier-src))
    (toplevel-define-struct-name def)))

(defgeneric type-definition-vars (def)
  (:method ((def toplevel-define-type))
    (declare (values keyword-src-list))
    (toplevel-define-type-vars def))

  (:method ((def toplevel-define-struct))
    (declare (values keyword-src-list))
    (toplevel-define-struct-vars def)))

(defgeneric type-definition-repr (def)
  (:method ((def toplevel-define-type))
    (declare (values (or null attribute-repr)))
    (toplevel-define-type-repr def))

  (:method ((def toplevel-define-struct))
    (declare (values (or null attribute-repr)))
    (toplevel-define-struct-repr def)))

(defgeneric type-definition-ctors (def)
  (:method ((def toplevel-define-type))
    (declare (values constructor-list))
    (toplevel-define-type-ctors def))

  (:method ((def toplevel-define-struct))
    (declare (values toplevel-define-struct-list))
    (list def)))

(defgeneric type-definition-ctor-name (ctor)
  (:method ((ctor constructor))
    (declare (values identifier-src))
    (constructor-name ctor))

  (:method ((ctor toplevel-define-struct))
    (declare (values identifier-src))
    (toplevel-define-struct-name ctor)))

(defgeneric type-definition-ctor-field-types (ctor)
  (:method ((ctor constructor))
    (declare (values ty-list))
    (constructor-fields ctor))

  (:method ((ctor toplevel-define-struct))
    (declare (values ty-list))
    (mapcar #'struct-field-type (toplevel-define-struct-fields ctor))))
