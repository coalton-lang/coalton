(defpackage #:coalton-impl/parser/base
  (:use
   #:cl)
  (:shadow
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error))
  (:export
   #:identifier                         ; TYPE
   #:identifierp                        ; FUNCTION
   #:identifier-list                    ; TYPE
   #:keyword-src                        ; STRUCT
   #:make-keyword-src                   ; CONSTRUCTOR
   #:keyword-src-name                   ; ACCESSOR
   #:keyword-src-source                 ; ACCESSOR
   #:keyword-src-list                   ; TYPE
   #:identifier-src                     ; STRUCT
   #:make-identifier-src                ; CONSTRUCTOR
   #:identifier-src-name                ; ACCESSOR
   #:identifier-src-source              ; ACCESSOR
   #:identifier-src-list                ; TYPE
   #:parse-error                        ; CONDITION
   #:parse-error-err                    ; ACCESSOR
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
  (name   (util:required 'name)   :type keyword :read-only t)
  (source (util:required 'source) :type cons    :read-only t))

(defun keyword-src-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'keyword-src-p x)))

(deftype keyword-src-list ()
  '(satisfies keyword-src-list-p))

(defstruct (identifier-src
            (:copier nil))
  (name   (util:required 'name)   :type identifier :read-only t)
  (source (util:required 'source) :type cons       :read-only t))

(defun identifier-src-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'identifier-src-p x)))

(deftype identifier-src-list ()
  '(satisfies identifier-src-list-p))

(define-condition parse-error (error:coalton-base-error)
  ())

(defun parse-list (f list_ file)
  (declare (type function f)
           (type cst:cst list_)
           (type error:coalton-file file)
           (values list))

  (loop :for list := list_ :then (cst:rest list)
        :while (cst:consp list)
        :collect (funcall f (cst:first list) file)))
