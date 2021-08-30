;;;; lisp-object.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:coalton-impl)

;;;; We define an opaque type (as far as Coalton is concerned) that
;;;; can store an arbitrary Lisp object. Mostly used for binding with
;;;; Lisp code.

(declaim (inline veil unveil lisp-object-reference))

(defstruct (lisp-object
            (:predicate nil)
            (:copier nil)
            (:constructor veil (reference)))
  ;; We don't make this READ-ONLY to keep with the idioms of Lisp to
  ;; have things be mutable and free-form. If you're using LISPOBJ,
  ;; you should know what you're doing.
  (reference nil :type t :read-only nil))

#+sbcl (declaim (sb-ext:freeze-type lisp-object))

(defun unveil (lispobj)
  "Reveal the object hidden inside of a LispObj."
  (lisp-object-reference lispobj))

(defmethod print-object ((obj lisp-object) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (print-object (lisp-object-reference obj) stream)))
