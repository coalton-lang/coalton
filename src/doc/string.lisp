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

(defmethod object-aname ((ty tc:tycon))
  (let ((tcon-name (tc:tycon-name ty)))
    (package-qualified-anchor
     (symbol-package tcon-name)
     (lookup-type-source-name tcon-name)
     "type")))
