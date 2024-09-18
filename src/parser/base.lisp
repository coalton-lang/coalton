(defpackage #:coalton-impl/parser/base
  (:use
   #:cl)
  (:shadow
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:se #:source-error)
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util))
  (:export
   #:identifier                         ; TYPE
   #:identifierp                        ; FUNCTION
   #:identifier-list                    ; TYPE
   #:keyword-src                        ; STRUCT
   #:make-keyword-src                   ; CONSTRUCTOR
   #:keyword-src-name                   ; ACCESSOR
   #:keyword-src-list                   ; TYPE
   #:identifier-src                     ; STRUCT
   #:make-identifier-src                ; CONSTRUCTOR
   #:identifier-src-name                ; ACCESSOR
   #:identifier-src-list                ; TYPE
   #:parse-error                        ; CONDITION
   #:source-note                        ; FUNCTION
   #:parse-list                         ; FUNCTION
   ))

(in-package #:coalton-impl/parser/base)

;;;
;;; Shared definitions for source parser
;;;

(deftype identifier ()
  '(and symbol (not boolean) (not keyword)))

(defun identifierp (x)
  (typep x 'identifier))

(defun identifier-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'identifierp x)))

(deftype identifier-list ()
  '(satisfies identifier-list-p))

(defstruct (keyword-src
            (:copier nil))
  (name     (util:required 'name)     :type keyword :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self keyword-src))
  (keyword-src-location self))

(defun keyword-src-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'keyword-src-p x)))

(deftype keyword-src-list ()
  '(satisfies keyword-src-list-p))

(defstruct (identifier-src
            (:copier nil))
  (name     (util:required 'name)     :type identifier :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self identifier-src))
  (identifier-src-location self))

(defun identifier-src-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'identifier-src-p x)))

(deftype identifier-src-list ()
  '(satisfies identifier-src-list-p))

(defun parse-list (f list_ file)
  (declare (type function f)
           (type cst:cst list_)
           (values list))

  (loop :for list := list_ :then (cst:rest list)
        :while (cst:consp list)
        :collect (funcall f (cst:first list) file)))

;;; Condition types and helper functions specific to errors
;;; encountered during parsing.
;;;
;;; A complex parse error may be signaled with:
;;;
;;;   (parse-error "Overall description of condition"
;;;                (source-note SOURCE CST1 "Primary ~A: ~A" ARG1 ARG2)
;;;                (source-note SOURCE CST2 "Related ~A: ~A" ARG3 ARG4)
;;;                ... )

(define-condition parse-error (se:source-base-error)
  ()
  (:documentation "A condition indicating a syntax error in Coalton source code."))

(defun parse-error (message &rest notes)
  "Signal a PARSE-ERROR with provided MESSAGE and source NOTES."
  (error 'parse-error :err (source:make-source-error ':error message notes)))

(defun source-note (source cst format-string &rest format-args)
  "Helper function to make a source note using SOURCE and CST:SOURCE as location."
  (declare (type cst:cst cst)
           (type string format-string))
  (apply #'source:note
         (source:make-location source (cst:source cst))
         format-string format-args))
