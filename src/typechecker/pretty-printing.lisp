(in-package #:coalton-impl/typechecker)

(defvar *pprint-variable-symbol-code*)
(defvar *pprint-variable-symbol-suffix*)

(defun next-pprint-variable ()
  "Get the next type variable symbol interned in the keyword package"
  (prog1
      (intern
       (if (= 0 *pprint-variable-symbol-suffix*)
           (format nil "~A" (code-char *pprint-variable-symbol-code*))
           (format nil "~A~A"
                   (code-char *pprint-variable-symbol-code*)
                   *pprint-variable-symbol-suffix*))
       'keyword)
    (incf *pprint-variable-symbol-code*)
    (when (< (char-code #\Z) *pprint-variable-symbol-code*)
      (setf *pprint-variable-symbol-code* (char-code #\A))
      (incf *pprint-variable-symbol-suffix*))))

(defun next-pprint-variable-as-tvar (&optional (kind kStar))
  "Get the next type variable as a TVAR"
  ;; This is an awful awful hack
  (make-tcon :tycon (make-tycon :name (next-pprint-variable) :kind kind)))

(defmacro with-pprint-variable-scope (() &body body)
  "If there is no pretty printing variable scope then create one for BODY"
  `(if (boundp '*pprint-variable-symbol-code*)
       (let ((*pprint-variable-symbol-code* *pprint-variable-symbol-code*)
             (*pprint-variable-symbol-suffix* *pprint-variable-symbol-suffix*))
         ,@body)
       (let ((*pprint-variable-symbol-code* (char-code #\A))
             (*pprint-variable-symbol-suffix* 0))
         ,@body)))

(defvar *pprint-tyvar-dict*)

(defun pprint-tvar (tvar)
  (unless (boundp '*pprint-tyvar-dict*)
    (error "Unable to pretty print tvar outside pprint variable context"))
  (let ((value (gethash (tyvar-id (tvar-tyvar tvar)) *pprint-tyvar-dict*)))
    (or value
        (setf (gethash (tyvar-id (tvar-tyvar tvar)) *pprint-tyvar-dict*)
              (next-pprint-variable-as-tvar)))))

(defmacro with-pprint-variable-context (() &body body)
  "Create a variable context which can be used with PPRINT-TVAR"
  `(let ((*pprint-tyvar-dict* (make-hash-table :test #'equalp))
         (*coalton-pretty-print-tyvars* t))
     (with-pprint-variable-scope ()
       ,@body)))
