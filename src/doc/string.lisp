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
   (#:tc #:coalton-impl/typechecker)))

(in-package #:coalton/doc/string)

(defmethod object-name ((ty tc:tyvar))
  (with-output-to-string (stream)
    (tc:pprint-ty stream (tc:pprint-tvar ty))))

(defmethod object-name ((ty tc:tycon))
  (with-output-to-string (stream)
    (let ((tcon-name (tc:tycon-name ty)))
      (if (string= "KEYWORD" (package-name (symbol-package tcon-name)))
          (format stream "~S" tcon-name)
          (format stream "~A" (symbol-name tcon-name))))))

(defmethod object-aname ((ty tc:tycon))
  (format nil "~(~A-type~)" (html-entities:encode-entities (object-name ty))))

(defun write-function-types (ty)
  (with-output-to-string (stream)
    (write-string "(" stream)
    (object-name (tc:tapp-to (tc:tapp-from ty)))
    (write-string " -> " stream)
    ;; Avoid printing extra parentheses on curried functions
    (labels ((print-subfunction (to)
               (cond ((tc:function-type-p to)
                      (object-name (tc:tapp-to (tc:tapp-from to)))
                      (write-string " -> " stream)
                      (print-subfunction (tc:tapp-to to)))
                     (t
                      (object-name to)))))
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
               (object-name arg))
             (write-string ")" stream))
            (t
             (write-string "(" stream)
             (object-name (tc:tapp-from ty))
             (write-string " " stream)
             (object-name (tc:tapp-to ty))
             (write-string ")" stream))))))

(defmethod object-name ((ty tc:tapp))
  (cond ((tc:function-type-p ty)
         (write-function-types ty))
        (t
         (write-type-constructors ty))))

(defmethod object-name ((object tc:ty-predicate))
  (with-output-to-string (stream)
    (write-string (symbol-name (tc:ty-predicate-class object)) stream)
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
