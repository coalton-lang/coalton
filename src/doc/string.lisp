;;;; The class 'string-backend' provides canonical string
;;;; representations of Coalton structures: these are used to sort
;;;; documentation entries. This is kept separate from print-object
;;;; and readable printing mechanisms so that it can evolve
;;;; separately. There may be opportunity to unify implementations in
;;;; the future.

(defpackage #:coalton/doc/string
  (:documentation "Plain string backend for coalton doc generator.")
  (:use
   #:cl
   #:coalton/doc/base
   #:coalton/doc/model)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton/doc/string)

(defmethod object-name ((ty tc:tyvar))
  (with-output-to-string (stream)
    (tc:pprint-ty stream (tc:pprint-tvar ty))))

(defmethod object-name ((ty tc:tycon))
  (with-output-to-string (stream)
    (let ((tcon-name (tc:tycon-name ty)))
      (if (string= "KEYWORD" (package-name (symbol-package tcon-name)))
          (format stream "~S" tcon-name)
          (format stream "~A" (lookup-type-source-name tcon-name))))))

(defmethod object-aname ((ty tc:tycon))
  (format nil "~(~A-type~)" (html-entities:encode-entities (object-name ty))))

(defun write-keyword-stage-head (stream stage)
  (declare (type tc:keyword-stage-ty stage)
           (values null &optional))
  (write-string "(&key" stream)
  (dolist (entry (tc:keyword-stage-ty-entries stage))
    (write-string " " stream)
    (write-string (format nil "~S" (tc:keyword-ty-entry-keyword entry)) stream)
    (write-string " " stream)
    (write-string (object-name (tc:keyword-ty-entry-type entry)) stream))
  (write-string ")" stream)
  nil)

(defun write-function-types (ty)
  (with-output-to-string (stream)
    (write-string "(" stream)
    (write-string (object-name (tc:tapp-to (tc:tapp-from ty))) stream)
    ;; Avoid printing extra parentheses on curried functions
    (labels ((print-subfunction (to)
               (cond ((typep to 'tc:keyword-stage-ty)
                      (write-string " -> " stream)
                      (write-keyword-stage-head stream to)
                      (print-subfunction (tc:keyword-stage-ty-to to)))
                     ((and (typep to 'tc:tapp) (tc:function-type-p to))
                      (write-string " -> " stream)
                      (write-string (object-name (tc:tapp-to (tc:tapp-from to))) stream)
                      (print-subfunction (tc:tapp-to to)))
                     (t
                      (write-string " -> " stream)
                      (write-string (object-name to) stream)))))
      (print-subfunction (tc:tapp-to ty)))
    (write-string ")" stream)))

(defun write-type-constructors (ty)
  (with-output-to-string (stream)
    (let* ((tcon ty)
           (tcon-args (loop :while (tc:tapp-p tcon)
                            :collect (tc:tapp-to tcon)
                            :do (setf tcon (tc:tapp-from tcon)))))
      (cond ((and (tc:tycon-p tcon)
                  (tc:simple-kind-p (tc:tycon-kind tcon))
                  (<= (length tcon-args)
                      (tc:kind-arity (tc:tycon-kind tcon))))
             (write-string "(" stream)
             (write-string (object-name tcon) stream)
             (dolist (arg (reverse tcon-args))
               (write-string " " stream)
               (write-string (object-name arg) stream))
             (write-string ")" stream))
            (t
             (write-string "(" stream)
             (write-string (object-name (tc:tapp-from ty)) stream)
             (write-string " " stream)
             (write-string (object-name (tc:tapp-to ty)) stream)
             (write-string ")" stream))))))

(defmethod object-name ((ty tc:tapp))
  (cond ((tc:function-type-p ty)
         (write-function-types ty))
        (t
         (write-type-constructors ty))))

(defmethod object-name ((ty tc:keyword-stage-ty))
  (with-output-to-string (stream)
    (write-string "(" stream)
    (write-keyword-stage-head stream ty)
    (write-string " -> " stream)
    (write-string (object-name (tc:keyword-stage-ty-to ty)) stream)
    (write-string ")" stream)))

(defmethod object-name ((object tc:ty-predicate))
  (with-output-to-string (stream)
    (write-string (lookup-class-source-name (tc:ty-predicate-class object)) stream)
    (dolist (type (tc:ty-predicate-types object))
      (write-char #\Space stream)
      (write-string (object-name type) stream)))) 

(defmethod object-name ((object tc:ty-class-instance))
  (let ((ctx (tc:ty-class-instance-constraints object))
        (pred (tc:ty-class-instance-predicate object)))
    (tc:with-pprint-variable-context ()
      (format nil "~:[~{~A ~}~;~{(~A) ~}~]~:*~:[~*~;~A ~]~A"
              ;; Get the second element to test if we have more than one predicate
              (second ctx)
              (mapcar #'object-name ctx)
              "=>"
              (object-name pred)))))
