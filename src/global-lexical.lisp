(defpackage #:coalton-impl/global-lexical
  (:use #:cl)
  (:export
   #:get-top-level-binding
   #:define-global-lexical))

(in-package #:coalton-impl/global-lexical)

;;;; Global environments:

;;; The global environment contains top level bindings.
(defstruct global-environment
  ;; A map from variable names to their top level bindings.
  (bindings (make-hash-table :test #'eq) :type hash-table))


(defvar *top-level-environment* (make-global-environment)
  "The current global top level environment.")

(declaim (type global-environment *top-level-environment*))

;;;; Top level bindings:
;;;;
;;;; We represent top level bindings as conses as they are usually
;;;; only two words on most Lisp implementations. The NAME is supplied
;;;; purely for debugging purposes, which is convenient because we get
;;;; an extra word for it in a CONS cell anyway.

(declaim (ftype (function (symbol global-environment) cons) get-top-level-binding))

;;; Get the top level binding for NAME in ENVIRONMENT.
(defun get-top-level-binding (name environment)
  (let ((bindings (global-environment-bindings environment)))
    (or (gethash name bindings)
        (setf (gethash name bindings)
              (cons ':|@@unbound@@| name)))))

(declaim (inline top-level-binding-value
                 top-level-binding-name
                 (setf top-level-binding-value)))

(defun top-level-binding-value (binding) (car binding))
(defun top-level-binding-name (binding) (cdr binding))

(defun (setf top-level-binding-value) (new-value binding)
  (setf (car binding) new-value))

(defmacro define-global-lexical (var val)
  `(progn
     (define-symbol-macro ,var
         (top-level-binding-value
          (load-time-value
           (get-top-level-binding ',var *top-level-environment*))))
     (setf ,var (load-time-value ,val))))
