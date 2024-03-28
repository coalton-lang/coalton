(defpackage #:coalton-impl/typechecker/base
  (:use
   #:cl)
  (:import-from
   #:source-error
   #:source-error
   #:make-note
   #:make-help)
  (:export
   #:source-error
   #:make-note
   #:make-help)
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
   (lambda (condition stream)
     (tc:with-pprint-variable-context ()
       (source-error:report-source-condition condition stream)))))

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

(defun check-package (elems f source)
  (declare (type list elems)
           (type function f)
           (type function source))

  (loop :for elem :in elems
        :for id := (funcall f elem)

        :do (check-type id symbol)

        :unless (equalp (symbol-package id) *package*)
          :do (error 'tc-error
                     :location (funcall source elem)
                     :message "Invalid identifier name"
                     :primary-note (format nil "The symbol ~S is defined in the package ~A and not the current package ~A"
                                           id
                                           (symbol-package id)
                                           *package*))))

