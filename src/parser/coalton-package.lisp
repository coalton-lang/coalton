(defpackage #:coalton-impl/parser/package
  (:use
   #:cl
   #:coalton-impl/error
   #:coalton-impl/parser/base)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree))
  (:export
   #:ensure-package
   #:generate-package
   #:parse-package))

(in-package #:coalton-impl/parser/package)

(defun parse-symbol (sequence index file &key missing-error type-error)
  (let ((form (cst:nth index sequence)))
    (unless form
      (parse-error file (cst:source form)
                   "Malformed reference"
                   (or missing-error "missing symbol")))
    (unless (identifierp (cst:raw form))
      (parse-error file (cst:source form)
                   "Malformed reference"
                   (or type-error "Not a symbol")))
    (symbol-name (cst:raw form))))

(defun map-forms (f form)
  (loop :for clauses := form :then (cst:rest clauses)
        :while (cst:consp clauses)
        :collect (funcall f (cst:first clauses))))


(defun parse-import-statement (form file)
  (cond ((cst:consp form)
         (let ((source-package (parse-symbol form 0 file)))
           (flet ((parse-import-symbol (import)
                    (cond ((string= "*" (cst:raw import))
                           (return-from parse-import-statement
                             `(:import-all ,source-package)))
                          ((identifierp (cst:raw import))
                           (symbol-name (cst:raw import)))
                          (t
                           (parse-error file (cst:source import)
                                        "Malformed import statement"
                                        "Unrecognized import type")))))
             `(:import ,source-package
                       ,@(map-forms #'parse-import-symbol
                                      (cst:rest form))))))
        (t
         `(:import-all ,(symbol-name (cst:raw form))))))

(defun parse-package-clause (clause file)
  "Parses a coalton package clause of the form of ({operation} {symbol}* ...)"
  (unless (cst:consp clause)
    (parse-error file (cst:source clause)
                 "Malformed package declaration"
                 "malformed package clause"))
  (let* ((head (cst:first clause))
         (body (cst:rest clause))
         (clause-type (cst:raw head)))
    (cond ((string= clause-type "IMPORT")
           (map-forms (lambda (form)
                        (parse-import-statement form file))
                      body))
          ((string= clause-type "EXPORT")
           (list (cons :export (map-forms (lambda (form)
                                            (symbol-name (cst:raw form)))
                                          body))))
          (t
           (parse-error file (cst:source head)
                        "Malformed package declaration"
                        "Unknown package clause"
                        (list (make-coalton-error-help
                               :span (cst:source head)
                               :replacement #'identity
                               :message "Must be one of import or export")))))))

(defun parse-package (form file)
  "Parse a coalton package declaration in the form of (package {name})"

  ;; Package declarations must start with "PACKAGE"
  (unless (string= (cst:raw (cst:first form)) "PACKAGE")
    (parse-error file (cst:source (cst:first form))
                 "Malformed package declaration"
                 "package declarations must start with `package`"))

  ;; Package declarations must have a name
  (unless (cst:consp (cst:rest form))
    (parse-error file (cst:source (cst:first form))
                 "Malformed package declaration"
                 "missing package name"))

  ;; Remaining forms are package clauses
  (let ((name-form (cst:nth 1 form)))
    (unless name-form
      (parse-error file (cst:source name-form) "Malformed package declaration"
                   "missing package name"))
    (unless (identifierp (cst:raw name-form))
      (parse-error file (cst:source name-form) "Malformed package declaration"
                   "package name must be a symbol"))
    (let ((package-name (symbol-name (cst:raw name-form))))
      (cons (list :package package-name)
            (apply #'append
                   (map-forms (lambda (form)
                                (parse-package-clause form file))
                              (cst:rest (cst:rest form))))))))

(defun do-package-clause (name)
  (let ((p (uiop:ensure-package name)))
    (use-package "COALTON" p)
    p))

(defun do-import-all-clause (package name)
  (use-package (find-package name) package))

(defun do-import-clause (package name &rest symbols)
  (import (mapcar (lambda (sym)
                    (intern sym (find-package name)))
                  symbols)
          package))

(defun do-export-clause (package &rest symbols)
  (import (mapcar (lambda (sym)
                    (intern sym package))
                  symbols)
          package))

(defun ensure-package (parsed-package)
  (let ((package nil))
    (dolist (step parsed-package)
      (destructuring-bind (op . args) step
        (ecase op
          (:package
           (setf package (apply #'do-package-clause args)))
          (:import-all
           (apply #'do-import-all-clause package args))
          (:import
           (apply #'do-import-clause package args))
          (:export
           (apply #'do-export-clause package args)))))
    package))

(defun package-def-name (x)
  (cadr (find :package x :key #'car)))

(defun package-use (x)
  (cons "COALTON"
        (mapcar #'cadr
                (remove-if-not (lambda (clause)
                                 (eql (car clause) :import-all)) x))))

(defun generate-package (x)
  ;; FIXME incomplete
  `(defpackage ,(package-def-name x)
     (:use ,@(package-use x))))
