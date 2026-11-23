(defpackage #:coalton-impl/global-lexical
  (:use #:cl)
  (:export
   #:get-top-level-binding
   #:ensure-top-level-constant
   #:define-global-lexical
   #:define-global-constant))

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
              ;; Wrap the mutable value cell one cons deeper so SBCL's
              ;; depth-1 CTYPE-OF on LOAD-TIME-VALUE constants in EVAL
              ;; sees CONS rather than (EQL :|@@unbound@@|).
              (cons (cons ':|@@unbound@@| nil) name)))))

(declaim (inline top-level-binding-value
                 top-level-binding-name
                 (setf top-level-binding-value)))

(defun top-level-binding-value (binding) (caar binding))
(defun top-level-binding-name (binding) (cdr binding))

(defun (setf top-level-binding-value) (new-value binding)
  (setf (caar binding) new-value))

(defun ensure-top-level-constant (name environment thunk)
  "Return the value of NAME in ENVIRONMENT, initializing it with THUNK if unbound."
  (declare (type symbol name)
           (type global-environment environment)
           (type function thunk))
  (let ((binding (get-top-level-binding name environment)))
    (if (eq (top-level-binding-value binding) ':|@@unbound@@|)
        (setf (top-level-binding-value binding) (funcall thunk))
        (top-level-binding-value binding))))

(defmacro define-global-lexical (var type)
  `(progn
     (define-symbol-macro ,var
         (the ,type
              (top-level-binding-value (load-time-value
                                        (get-top-level-binding ',var *top-level-environment*)))))))

(defmacro define-global-constant (var type init-form)
  `(progn
     (define-symbol-macro ,var
         (the ,type
              (load-time-value
               (ensure-top-level-constant
                ',var
                *top-level-environment*
                (lambda () ,init-form))
               t)))
     (ensure-top-level-constant
      ',var
      *top-level-environment*
      (lambda () ,init-form))))
