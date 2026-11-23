;;;; The class 'string-backend' provides canonical string
;;;; representations of Coalton structures: these are used to sort
;;;; documentation entries.

(defpackage #:coalton/doc/string
  (:documentation "Plain string backend for coalton doc generator.")
  (:use
   #:cl
   #:coalton/doc/base
   #:coalton/doc/model)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton/doc/string)

(defun type-object-name (object)
  (let ((coalton-impl/settings:*coalton-print-unicode* nil))
    (tc:type-to-string object entry:*global-environment*)))

(defmethod object-name ((object tc:ty))
  (type-object-name object))

(defmethod object-name ((object tc:qualified-ty))
  (type-object-name object))

(defmethod object-name ((object tc:ty-scheme))
  (type-object-name object))

(defmethod object-name ((object tc:ty-predicate))
  (type-object-name object))

(defmethod object-name ((object tc:ty-class-instance))
  (type-object-name object))

(defmethod object-name ((ctor tc:constructor-entry))
  (let* ((ctor-name (tc:constructor-entry-name ctor))
         (ctor-source-name (lookup-constructor-source-name ctor-name))
         (ctor-type (tc:lookup-value-type entry:*global-environment* ctor-name)))
    (tc:with-pprint-variable-context ()
      (cond
        ((tc:constructor-entry-gadt-p ctor)
         (format nil "~A :: ~A"
                 ctor-source-name
                 (object-name ctor-type)))
        (t
         (let ((args (type-constructor-args nil ctor-type)))
           (if args
               (format nil "(~A~{ ~A~})"
                       ctor-source-name
                       (mapcar #'object-name args))
               ctor-source-name)))))))

(defmethod object-aname ((ty tc:tycon))
  (let ((tcon-name (tc:tycon-name ty)))
    (package-qualified-anchor
     (symbol-package tcon-name)
     (lookup-type-source-name tcon-name)
     "type")))
