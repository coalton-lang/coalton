(defpackage #:coalton-impl/typechecker/base
  (:use
   #:cl)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util))
  (:import-from
   #:coalton-impl/settings
   #:*coalton-type-printing-mode*)
  (:export
   #:*coalton-type-printing-mode*
   #:*coalton-pretty-print-tyvars*
   #:*pprint-tyvar-dict*
   #:*pprint-variable-symbol-code*
   #:*pprint-variable-symbol-suffix*
   #:tc-error                           ; CONDITION, FUNCTION
   #:tc-location
   #:tc-secondary-location
   #:tc-note
   #:tc-secondary-note
   #:coalton-internal-type-error        ; CONDITION
   #:check-duplicates                   ; FUNCTION
   #:check-package                      ; FUNCTION
   #:with-pprint-variable-scope         ; MACRO
   #:with-pprint-variable-context       ; MACRO
   ))

(in-package #:coalton-impl/typechecker/base)

;;;
;;; Shared definitions for type checking environment
;;;

;;;
;;; Pretty printer control
;;;

(defvar *pprint-variable-symbol-code*)

(defvar *pprint-variable-symbol-suffix*)

(defmacro with-pprint-variable-scope (() &body body)
  "If there is no pretty printing variable scope then create one for BODY"
  `(let* ((bound (boundp '*pprint-variable-symbol-code*))
          (*pprint-variable-symbol-code* (if bound
                                             *pprint-variable-symbol-code*
                                             (char-code #\A)))
          (*pprint-variable-symbol-suffix* (if bound
                                               *pprint-variable-symbol-suffix*
                                               0)))
     ,@body))

(defvar *coalton-pretty-print-tyvars* nil
  "Whether to print all tyvars using type variable syntax

This requires a valid PPRINT-VARIABLE-CONTEXT")

(defvar *pprint-tyvar-dict*)

(defmacro with-pprint-variable-context (() &body body)
  "Create a variable context which can be used with PPRINT-TVAR"
  `(let ((*pprint-tyvar-dict* (make-hash-table :test #'equalp))
         (*coalton-pretty-print-tyvars* t))
     (with-pprint-variable-scope ()
       ,@body)))

;;;
;;; Conditions
;;;

(defun tc-location (location format-string &rest format-args)
  (source:note location
               (with-pprint-variable-context ()
                 (apply #'format nil format-string format-args))))

(defun tc-secondary-location (location format-string &rest format-args)
  (source:secondary-note location
                       (with-pprint-variable-context ()
                         (apply #'format nil format-string format-args))))

(defun tc-note (located format-string &rest format-args) 
  (apply #'tc-location (source:location located) format-string format-args))

(defun tc-secondary-note (located format-string &rest format-args)
  (apply #'tc-secondary-location (source:location located) format-string format-args))

(define-condition tc-error (source:source-error)
  ())

(defun tc-error (message &rest notes)
  "Signal a typechecker error with MESSAGE, and optional NOTES that label source locations."
  (declare (type string message))
  (error 'tc-error :message message :notes notes))

(define-condition coalton-internal-type-error (error)
  ()
  (:documentation "An internal Coalton condition for use in signaling. Internal conditions should always be caught.")
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Unhandled internal condition.~%~%If you are seeing this, please file an issue on Github."))))

;;;
;;; Assertions
;;;

(defun check-duplicates (elems f callback)
  "Check for duplicate elements in ELEMS.  F maps items in ELEMS to symbols which are compared for equality.
As soon as two duplicate elements are detected, CALLBACK is invoked with those two elements, ordered by source location."
  (declare (type list elems)
           (type function f)
           (type function callback))

  (loop :with table := (make-hash-table :test #'eq)

        :for elem :in elems
        :for id := (funcall f elem)

        :do (check-type id symbol)

        :if (gethash id table)
          :do (let ((first (gethash id table))
                    (second elem))

                (unless (source:location< (source:location first)
                                          (source:location second))
                  (psetf first second second first))

                (funcall callback first second))
        :else
          :do (setf (gethash (funcall f elem) table) elem)))

(defun check-package (elems f source)
  (declare (type list elems)
           (type function f)
           (type function source))

  (loop :for elem :in elems
        :for id := (funcall f elem)

        :do (check-type id symbol)

        :unless (equalp (symbol-package id) *package*)
          :do (tc-error "Invalid identifier name"
                        (tc-location (funcall source elem)
                                     "The symbol ~S is defined in the package ~A and not the current package ~A"
                                     id
                                     (symbol-package id)
                                     *package*))))
