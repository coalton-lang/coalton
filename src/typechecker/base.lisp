(defpackage #:coalton-impl/typechecker/base
  (:use
   #:cl)
  (:import-from
   #:coalton-impl/error
   #:coalton-file
   #:coalton-error
   #:make-coalton-error-note
   #:make-coalton-error-help)
  (:export
   #:coalton-file
   #:coalton-error
   #:make-coalton-error-note
   #:make-coalton-error-help)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error))
  (:export
   #:*coalton-pretty-print-tyvars*
   #:*pprint-tyvar-dict*
   #:*pprint-variable-symbol-code*
   #:*pprint-variable-symbol-suffix*
   #:tc-error                           ; CONDITION
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
;;; Pretty printing
;;;

(defvar *pprint-variable-symbol-code*)

(defvar *pprint-variable-symbol-suffix*)

(defmacro with-pprint-variable-scope (() &body body)
  "If there is no pretty printing variable scope then create one for BODY"
  `(if (boundp '*pprint-variable-symbol-code*)
       (let ((*pprint-variable-symbol-code* *pprint-variable-symbol-code*)
             (*pprint-variable-symbol-suffix* *pprint-variable-symbol-suffix*))
         ,@body)
       (let ((*pprint-variable-symbol-code* (char-code #\A))
             (*pprint-variable-symbol-suffix* 0))
         ,@body)))

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

(define-condition tc-error (error:coalton-base-error)
  ()
  (:report
   (lambda (c s)
     (if (error:coalton-error-text c)
         (write-string (error:coalton-error-text c) s)
         (with-pprint-variable-context ()
           (error:display-coalton-error s (error:coalton-error-err c)))))))

;;;
;;; Assertions
;;;

(defun check-duplicates (elems f g callback)
  "Check for duplicate elements in ELEMS. F maps items in ELEMS to
symbols which are compared for equality. G maps items in ELEMS to
source tuples which are compared for ordering."
  (declare (type list elems)
           (type function f)
           (type function g)
           (type function callback))

  (loop :with table := (make-hash-table :test #'eq)

        :for elem :in elems
        :for id := (funcall f elem)

        :do (check-type id symbol)

        :if (gethash id table)
          :do (let ((first (gethash id table))
                    (second elem))

                (when (> (car (funcall g first)) (car (funcall g second)))
                  (psetf first second second first))

                (funcall callback first second))
        :else
          :do (setf (gethash (funcall f elem) table) elem)))

(defun check-package (elems f source file)
  (declare (type list elems)
           (type function f)
           (type function source)
           (type coalton-file file))

  (loop :for elem :in elems
        :for id := (funcall f elem)

        :do (check-type id symbol)

        :unless (equalp (symbol-package id) *package*)
          :do (error 'tc-error
                     :err (coalton-error
                           :span (funcall source elem)
                           :file file
                           :message "Invalid identifier name"
                           :primary-note (format nil "The symbol ~S is defined in the package ~A and not the current package ~A"
                                            id
                                            (symbol-package id)
                                            *package*)))))
