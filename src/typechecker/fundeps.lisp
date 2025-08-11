;;;; fundeps.lisp
;;;;
;;;; - "Type Classes with Functional Dependencies" by Mark P. Jones [2000]
;;;; - "Exploring the Design Space for Type-based Implicit Parameterization" by Mark P. Jones [1999]
;;;; - "The Theory of Relational Databases" by David Mair ch. 4 and 5 [1983]

(defpackage #:coalton-impl/typechecker/fundeps
  (:use
   #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:settings #:coalton-impl/settings))
  (:export
   #:fundep                             ; STRUCT
   #:make-fundep                        ; CONSTRUCTOR
   #:fundep-from                        ; ACCESSOR
   #:fundep-to                          ; ACCESSOR
   #:fundep-list                        ; TYPE
   #:closure                            ; FUNCTION
   #:generic-closure                    ; FUNCTION
   #:+fundep-max-depth+                 ; CONSTANT
   ))

(in-package #:coalton-impl/typechecker/fundeps)

(defconstant +fundep-max-depth+ 1000
  "Maximum number of times to cycle when waiting for fundeps to fixpoint")

(defstruct fundep
  (from (util:required 'from) :type util:symbol-list :read-only t)
  (to   (util:required 'to)   :type util:symbol-list :read-only t))

(defmethod print-object ((self fundep) stream)
  (when *print-readably*
    (return-from print-object (call-next-method)))

  (write-string "(" stream)
  (format stream "~{~S ~}" (fundep-from self))
  (if settings:*coalton-print-unicode*
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

(deftype generic-fundep ()
  '(cons list list))

(defun generic-fundep-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (x) (typep x 'generic-fundep)) x)))

(deftype generic-fundep-list ()
  '(satisfies generic-fundep-list-p))

(defun generic-closure (x f &key key (test #'eq))
  (declare (type list x)
           (type generic-fundep-list f)
           (type (or null symbol function) key)
           (type (or symbol function) test)
           (values list &optional))
  (loop :with old := nil
        :with new := x
        :until (null (set-exclusive-or new old :key key :test test))
        :do (progn
              (setf old new)
              (loop :for (w . z) :in f
                    :when (subsetp w new :key key :test test)
                      :do (setf new (union new z :key key :test test))))
        :finally (return new)))

