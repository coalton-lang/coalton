;;;; fundeps.lisp
;;;;
;;;; - "Type Classes with Functional Dependencies" by Mark P. Jones [2000]
;;;; - "Exploring the Design Space for Type-based Implicit Parameterization" by Mark P. Jones [1999]
;;;; - "The Theory of Relational Databases" by David Mair ch. 4 and 5 [1983]

(defpackage #:coalton-impl/typechecker/fundeps
  (:use
   #:cl)
  (:import-from
   #:coalton-impl/typechecker/types
   #:*coalton-print-unicode*)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:fundep                             ; STRUCT
   #:make-fundep                        ; CONSTRUCTOR
   #:fundep-from                        ; ACCESSOR
   #:fundep-to                          ; ACCESSOR
   #:fundep-list                        ; TYPE
   #:closure                            ; FUNCTION
   #:+fundep-max-depth+                 ; CONSTANT
   ))

(in-package #:coalton-impl/typechecker/fundeps)

(defconstant +fundep-max-depth+ 100
  "Maximum number of times to cycle when waiting for fundeps to fixpoint")

(defstruct fundep
  (from (util:required 'from) :type util:symbol-list :read-only t)
  (to   (util:required 'to)   :type util:symbol-list :read-only t))

(defmethod print-object ((self fundep) stream)
  (write-string "(" stream)
  (format stream "~{~S ~}" (fundep-from self))
  (if *coalton-print-unicode*
      (write-string "→" stream)
      (write-string "->" stream))
  (format stream "~{ ~S~}" (fundep-to self))
  (write-string ")" stream))

(defmethod make-load-form ((self fundep) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun fundep-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'fundep-p x)))

(deftype fundep-list ()
  '(satisfies fundep-list-p))

;; Implementation of closure taken from "The Theory of Relational Databases" pg. 64
(defun closure (x f)
  "Computes the closure of x under f"
  (declare (type util:symbol-list x)
           (type fundep-list f)
           (values util:symbol-list))
  (let ((olddep nil)
        (newdep x))
    (loop :until (null (set-exclusive-or newdep olddep :test #'equalp))
          :do (setf olddep newdep)
          :do (loop :for fd :in f
                    :for w := (fundep-from fd)
                    :for z := (fundep-to fd)
                    :if (subsetp w newdep :test #'equalp)
                      :do (setf newdep (union newdep z :test #'equalp))))
    newdep))

