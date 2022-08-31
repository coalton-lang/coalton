(defpackage #:coalton-impl/ast/parse-error
  (:use #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error))
  (:export
   #:coalton-parse-error                ; CONDITION
   #:coalton-parse-error-form           ; ACCESSOR
   #:coalton-parse-error-reason-control ; ACCESSOR
   #:coalton-parse-error-reason-args    ; ACCESSOR
   #:coalton-parse-error-context        ; CONDITION
   #:error-parsing                      ; FUNCTION
   #:coalton-unknown-instance           ; CONDITION
   #:error-unknown-instance             ; FUNCTION
   #:coalton-inherited-symbol           ; CONDITION
   #:error-inherited-symbol             ; FUNCTION
   ))

(in-package #:coalton-impl/ast/parse-error)

(define-condition coalton-parse-error (error:coalton-error)
  ((form :initarg :form
         :reader coalton-parse-error-form)
   (reason-control :initarg :reason-control
                   :reader coalton-parse-error-reason-control)
   (reason-args :initarg :reason-args
                :reader coalton-parse-error-reason-args))
  (:report (lambda (c s)
             (let ((*print-circle* nil))
               (format s "Failed to parse ~S~%~?"
                       (coalton-parse-error-form c)
                       (coalton-parse-error-reason-control c)
                       (coalton-parse-error-reason-args c))))))

(defun error-parsing (form reason-control &rest reason-args)
  (error 'coalton-parse-error
         :form form
         :reason-control reason-control
         :reason-args reason-args))

(define-condition coalton-unknown-instance (error:coalton-error)
  ((instance :initarg :instance
             :reader coalton-unknown-instance-instance))
  (:report (lambda (c s)
             (let ((*print-circle* nil))
               (format s "Missing definition for ~A"
                       (coalton-unknown-instance-instance c))))))

(defun error-unknown-instance (instance)
  (error 'coalton-unknown-instance
         :instance instance))

(define-condition coalton-inherited-symbol (error:coalton-error)
  ((symbol :initarg :symbol
           :reader coalton-inherited-symbol-symbol
           :type symbol)
   (package :initarg :package
            :reader coalton-inherited-symbol-package ))
  (:report (lambda (c s)
             (let ((*print-pretty* nil))
               (format s "Unable to define ~a in ~a~%   that symbol was inherited from ~a"
                       (symbol-name (coalton-inherited-symbol-symbol c))
                       (coalton-inherited-symbol-package c) 
                       (symbol-package (coalton-inherited-symbol-symbol c)))))))

(defun error-inherited-symbol (symbol package)
  (error 'coalton-inherited-symbol
         :symbol symbol
         :package package))
