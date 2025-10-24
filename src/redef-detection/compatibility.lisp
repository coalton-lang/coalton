(defpackage #:coalton-impl/redef-detection/compatibility
  (:use #:cl)
  (:local-nicknames
   (#:tc-scheme #:coalton-impl/typechecker/scheme)
   (#:tc-env #:coalton-impl/typechecker/environment)
   (#:settings #:coalton-impl/settings))
  (:export
   #:types-compatible-p
   #:format-type-for-user))
(in-package #:coalton-impl/redef-detection/compatibility)

;;;
;;; Type Compatibility
;;;

(defun types-compatible-p (old-scheme new-scheme env)
  "Check if NEW-SCHEME is compatible with OLD-SCHEME."
  (declare (type tc-scheme:ty-scheme old-scheme)
           (type tc-scheme:ty-scheme new-scheme)
           (type tc-env:environment env)
           (values boolean)
           (ignore env))

  (tc-scheme:ty-scheme= old-scheme new-scheme))

(defun format-type-for-user (ty-scheme)
  "Format a type scheme for display to the user."
  (declare (type tc-scheme:ty-scheme ty-scheme)
           (values string))

  (let ((settings:*coalton-print-unicode* nil))  ; ASCII output for terminals
    (format nil "~A" ty-scheme)))
