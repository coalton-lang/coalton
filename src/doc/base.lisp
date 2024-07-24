;;;; Output generics and global variables for doc generator.

(defpackage #:coalton/doc/base
  (:use
   #:cl)
  (:export
   #:make-backend
   #:register-backend

   #:write-object
   #:write-packages

   #:*remote*
   #:*local*

   #:exported-symbol-p))

(in-package #:coalton/doc/base)

(defvar *remote* nil
  "Path to remote git repository, for source links")

(defvar *local* nil
  "Path to local git repository")

;;; Protocol for backends

(defgeneric write-packages (backend packages)
  (:documentation "Write documentation for PACKAGES to the given BACKEND."))

(defgeneric write-object (backend object)
  (:documentation "Write an OBJECT to the given BACKEND."))

;;; *backends* maps keyword backend names to backend class names

(defvar *backends* nil)

(defun ensure-backend (name)
  "Lookup a backend by NAME."
  (or (cdr (assoc name *backends*))
      (error "Unknown backend ~S" name)))

(defun make-backend (name stream)
  "Make an instance of backend NAME that writes documentation to STREAM."
  (make-instance (ensure-backend name) :stream stream))

(defun register-backend (name class)
  "Register a backend class under a well-known NAME."
  (pushnew (cons name class) *backends* :test 'equalp))

;;; symbol and package utilities

(defun exported-symbol-p (symbol package &optional check-package)
  "T if SYMBOL is an exported symbol, optionally checking that it is a member of PACKAGE."
  (and (if check-package (equalp (symbol-package symbol) package) t)
       (eql :external (nth-value 1 (find-symbol (symbol-name symbol) package)))))
