(defpackage #:coalton-impl/typechecker/partial-type-env
  (:use
   #:cl
   #:coalton-impl/typechecker/base)
  (:local-nicknames
   (#:se #:source-error)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:partial-type-env                   ; STRUCT
   #:make-partial-type-env              ; CONSTRUCTOR
   #:partial-type-env-env               ; ACCESSOR
   #:partial-type-env-ty-table          ; ACCESSOR
   #:partial-type-env-vars-table        ; ACCESSOR
   #:partial-type-env-add-var           ; FUNCTION
   #:partial-type-env-lookup-var        ; FUNCTION
   #:partial-type-env-add-type          ; FUNCTION
   #:partial-type-env-replace-type      ; FUNCTION
   #:partial-type-env-lookup-type       ; FUNCTION
   #:partial-type-env-add-class         ; FUNCTION
   #:partial-type-env-lookup-class      ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/partial-type-env)

;;;
;;; Partial Type Environment
;;;

(defstruct (partial-type-env
            (:copier nil))
  (env         (util:required 'env)         :type tc:environment :read-only t)
  (ty-table    (make-hash-table :test #'eq) :type hash-table     :read-only t)
  (class-table (make-hash-table :test #'eq) :type hash-table     :read-only t))

(defun partial-type-env-add-var (env var)
  (declare (type partial-type-env env)
           (type symbol var)
           (values tc:tyvar))
  (setf (gethash var (partial-type-env-ty-table env)) (tc:make-variable (tc:make-kvariable))))

(defun partial-type-env-lookup-var (env var source)
  (declare (type partial-type-env env)
           (type symbol var)
           (type source:location source)
           (values tc:tyvar))
  (let ((ty (gethash var (partial-type-env-ty-table env))))
    (unless ty
      (tc-error source
                "Unknown type variable"
                (format nil "Unknown type variable ~S" var)))
    ty))

(defun partial-type-env-add-type (env name type)
  (declare (type partial-type-env env)
           (type symbol name)
           (type tc:ty type)
           (values null))

  (when (gethash name (partial-type-env-ty-table env))
    (util:coalton-bug "Attempt to add already defined type with name ~S." name))

  (setf (gethash name (partial-type-env-ty-table env)) type)
  nil)

(defun partial-type-env-replace-type (env name type)
  (declare (type partial-type-env env)
           (type symbol name)
           (type tc:ty type)
           (values null))

  (unless (gethash name (partial-type-env-ty-table env))
    (util:coalton-bug "Attempt to update undefined type with name ~S." name))

  (setf (gethash name (partial-type-env-ty-table env)) type)
  nil)

(defun partial-type-env-lookup-type (env tycon)
  (declare (type partial-type-env env)
           (type parser:tycon tycon)
           (values tc:ty))

  (let* ((name (parser:tycon-name tycon))

         (partial (gethash name (partial-type-env-ty-table env))))

    (when partial
      (return-from partial-type-env-lookup-type partial))

    (let ((type-entry (tc:lookup-type (partial-type-env-env env) name :no-error t)))

      (unless type-entry
        (tc-error (parser:ty-location tycon)
                  "Unknown type"
                  (format nil "unknown type ~S" (parser:tycon-name tycon))))

      (tc:type-entry-type type-entry))))

(defun partial-type-env-add-class (env pred)
  (declare (type partial-type-env env)
           (type tc:ty-predicate pred)
           (values null))

  (when (gethash (tc:ty-predicate-class pred) (partial-type-env-class-table env))
    (util:coalton-bug "Attempt to add alredy defined class with name ~S." (tc:ty-predicate-class pred)))

  (setf (gethash (tc:ty-predicate-class pred) (partial-type-env-class-table env)) pred)
  nil)

(defun partial-type-env-lookup-class (env pred)
  (declare (type partial-type-env env)
           (type parser:ty-predicate pred)
           (values tc:ty-predicate))

  (let* ((name (parser:identifier-src-name (parser:ty-predicate-class pred)))

         (partial (gethash name (partial-type-env-class-table env))))

    (when partial
      (return-from partial-type-env-lookup-class partial))

    (let ((class-entry (tc:lookup-class (partial-type-env-env env) name :no-error t)))

      (unless class-entry
        (tc-error pred
                  "Unknown class"
                  (format nil "unknown class ~S"
                          (parser:identifier-src-name
                           (parser:ty-predicate-class pred)))))

      (tc:ty-class-predicate class-entry))))
