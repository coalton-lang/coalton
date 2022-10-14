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
   (#:error #:coalton-impl/error)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:tc-error                           ; CONDITION
   #:check-duplicates                   ; FUNCTION
   #:check-package                      ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/base)

(define-condition tc-error (error:coalton-base-error)
  ()
  (:report
   (lambda (c s)
     (if (error:coalton-error-text c)
         (write-string (error:coalton-error-text c) s)
         (tc:with-pprint-variable-context ()
           (error:display-coalton-error s (error:coalton-error-err c)))))))

(defun check-duplicates (elems f callback)
  (declare (type list elems)
           (type function f)
           (type function callback))

  (loop :with table := (make-hash-table :test #'eq)

        :for elem :in elems
        :for id := (funcall f elem)

        :do (check-type id symbol)

        :if (gethash id table)
          :do (funcall callback (gethash id table) elem)
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

