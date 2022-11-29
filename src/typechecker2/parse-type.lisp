(defpackage #:coalton-impl/typechecker2/parse-type
  (:use
   #:cl
   #:coalton-impl/typechecker2/base)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:error #:coalton-impl/error)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:partial-type-env                   ; STRUCT
   #:make-partial-type-env              ; CONSTRUCTOR
   #:partial-type-env-env               ; ACCESSOR
   #:partial-type-env-ty-table          ; ACCESSOR
   #:partial-type-env-vars-table        ; ACCESSOR
   #:partial-type-env-add-var           ; FUNCTION
   #:partial-type-env-lookup-var        ; FUNCTION
   #:partial-type-env-add-type          ; FUNCTION
   #:partial-type-env-lookup-type       ; FUNCTION
   #:parse-type                         ; FUNCTION
   #:infer-type-kinds                   ; FUNCTION
   #:collect-referenced-types           ; FUNCTION
   #:collect-type-variables             ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker2/parse-type)

;;;
;;; Partial Type Environment
;;;

(defstruct (partial-type-env
            (:copier nil))
  (env        (util:required 'env)             :type tc:environment :read-only t)
  (ty-table   (make-hash-table :test #'eq)     :type hash-table     :read-only t)
  (vars-table (make-hash-table :test #'equalp) :type hash-table     :read-only t))

(defun partial-type-env-add-var (env type var)
  (declare (type partial-type-env env)
           (type symbol type)
           (type symbol var)
           (values tc:tyvar))
  (setf (gethash (cons type var) (partial-type-env-vars-table env)) (tc:make-variable (tc:make-kvariable))))

(defun partial-type-env-lookup-var (env type var)
  (declare (type partial-type-env env)
           (type symbol type)
           (type symbol var)
           (values tc:tyvar))
  (let ((kvar (gethash (cons type var) (partial-type-env-vars-table env))))
    kvar))

(defun partial-type-env-add-type (env name type)
  (declare (type partial-type-env env)
           (type symbol name)
           (type tc:ty type)
           (values null))

  (setf (gethash name (partial-type-env-ty-table env)) type)
  nil)

(defun partial-type-env-lookup-type (env tycon file)
  (declare (type partial-type-env env)
           (type parser:tycon tycon)
           (type file-stream file)
           (values tc:ty))
  (let* ((name (parser:tycon-name tycon))

         (partial (gethash name (partial-type-env-ty-table env))))

    (when partial
      (return-from partial-type-env-lookup-type partial))

    (let ((type-entry (tc:lookup-type (partial-type-env-env env) name :no-error t)))

      (unless type-entry
        (error 'tc-error
               :err (coalton-error
                     :span (parser:ty-source tycon)
                     :file file
                     :message "Unknown type"
                     :primary-note (format nil "unknown type ~S" (parser:tycon-name tycon)))))

      (tc:type-entry-type type-entry))))

;;;
;;; Entrypoints
;;;

(defun parse-type (ty env file)
  (declare (type parser:ty ty)
           (type tc:environment env)
           (type file-stream file)
           (values tc:ty &optional))

  (let ((tvars (collect-type-variables ty))

        (partial-env (make-partial-type-env :env env)))

    (loop :for tvar :in tvars
          :for tvar-name := (parser:tyvar-name tvar)
          :do (partial-type-env-add-var partial-env nil tvar-name))

    (multiple-value-bind (ty ksubs)
        (infer-type-kinds ty
                          tc:+kstar+
                          nil
                          nil
                          partial-env
                          file)

      (setf ty (tc:apply-ksubstitution ksubs ty))
      (setf ksubs (tc:kind-monomorphize-subs (tc:kind-variables ty) ksubs))
      (tc:apply-ksubstitution ksubs ty))))

;;;
;;; Kind Inference
;;;

(defgeneric infer-type-kinds (type expected-kind current-type ksubs env file)
  (:method ((type parser:tyvar) expected-kind current-type ksubs env file)
    (declare (type tc:kind expected-kind)
             (type symbol current-type)
             (type tc:ksubstitution-list ksubs)
             (type file-stream file))
    (let* ((tvar (partial-type-env-lookup-var env current-type (parser:tyvar-name type)))

           (kvar (tc:kind-of tvar)))
      (setf kvar (tc:apply-ksubstitution ksubs kvar))

      (handler-case
          (progn
            (setf ksubs (tc:kunify kvar expected-kind ksubs))
            (values (tc:apply-ksubstitution ksubs tvar) ksubs))
        (error:coalton-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:ty-source type)
                       :file file
                       :message "Kind mismatch"
                       :primary-note (format nil "Expected kind '~A' but variable is of kind '~A'"
                                             expected-kind
                                             kvar)))))))

  (:method ((type parser:tycon) expected-kind current-type ksubs env file)
    (declare (type tc:kind expected-kind)
             (type symbol current-type)
             (type tc:ksubstitution-list ksubs)
             (type partial-type-env env)
             (type file-stream file)
             (values tc:ty tc:ksubstitution-list))

    (let ((type_ (partial-type-env-lookup-type env type file)))
      (handler-case
          (progn
            (setf ksubs (tc:kunify (tc:kind-of type_) expected-kind ksubs))
            (values (tc:apply-ksubstitution ksubs type_) ksubs))
        (error:coalton-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:ty-source type)
                       :file file
                       :message "Kind mismatch"
                       :primary-note (format nil "Expected kind '~A' but got kind '~A'"
                                             expected-kind
                                             (tc:kind-of type_))))))))

  (:method ((type parser:tapp) expected-kind current-type ksubs env file)
    (declare (type tc:kind expected-kind)
             (type symbol current-type)
             (type tc:ksubstitution-list ksubs)
             (type partial-type-env env)
             (type file-stream file)
             (values tc:ty tc:ksubstitution-list &optional))

    (let ((fun-kind (tc:make-kvariable))

          (arg-kind (tc:make-kvariable)))

      (multiple-value-bind (fun-ty ksubs)
          (infer-type-kinds (parser:tapp-from type) fun-kind current-type ksubs env file)

        (setf fun-kind (tc:apply-ksubstitution ksubs fun-kind))

        (when (tc:kfun-p fun-kind)
          ;; SAFETY: unification against variable will never fail
          (setf ksubs (tc:kunify arg-kind (tc:kfun-from fun-kind) ksubs))
          (setf arg-kind (tc:apply-ksubstitution ksubs arg-kind)))

        (multiple-value-bind (arg-ty ksubs)
            (infer-type-kinds (parser:tapp-to type) arg-kind current-type ksubs env file)

          (handler-case
              (progn
                (setf ksubs (tc:kunify fun-kind (tc:make-kfun :from arg-kind :to expected-kind) ksubs)) 
                (tc:apply-type-argument fun-ty arg-ty :ksubs ksubs))
            (error:coalton-type-error ()
              (error 'tc-error
                     :err (coalton-error
                           :span (parser:ty-source (parser:tapp-from type))
                           :file file
                           :message "Kind mismatch"
                           :primary-note (format nil "Expected kind '~A' but got kind '~A'"
                                                 (tc:make-kfun
                                                  :from (tc:apply-ksubstitution ksubs arg-kind)
                                                  :to (tc:apply-ksubstitution ksubs expected-kind))
                                                 (tc:apply-ksubstitution ksubs fun-kind)))))))))))

(defun collect-referenced-types (type)
  "Returns a deduplicated list of all `PARSER:TYCON's in TYPE."
  (declare (type t type)
           (values parser:tycon-list))
  (delete-duplicates (collect-referenced-types-generic% type) :test #'eq))

(defgeneric collect-referenced-types-generic% (type)
  (:method ((type parser:tyvar))
    (declare (values parser:tycon-list))
    nil)

  (:method ((type parser:tycon))
    (declare (values parser:tycon-list))
    (list type))

  (:method ((type parser:tapp))
    (declare (values parser:tycon-list))
    (nconc (collect-referenced-types-generic% (parser:tapp-from type))
           (collect-referenced-types-generic% (parser:tapp-to type))))

  (:method ((pred parser:ty-predicate))
    (declare (values parser:tycon-list))
    (mapcan #'collect-referenced-types-generic% (parser:ty-predicate-types pred)))

  (:method ((type parser:qualified-ty))
    (declare (values parser:tycon-list))
    (nconc
     (collect-referenced-types-generic% (parser:qualified-ty-type type))
     (mapcan #'collect-referenced-types-generic% (parser:qualified-ty-predicates type))))

  (:method ((ctor parser:constructor))
    (declare (values parser:tycon-list))
    (mapcan #'collect-referenced-types-generic% (parser:constructor-fields ctor)))

  (:method ((type parser:toplevel-define-type))
    (declare (values parser:tycon-list))
    (mapcan #'collect-referenced-types-generic% (parser:toplevel-define-type-ctors type))))

(defun collect-type-variables (type)
  "Returns a deduplicated list of all `PARSER:TYVAR's in TYPE."
  (declare (type t type)
           (values parser:tyvar-list))
  (delete-duplicates (collect-type-variables-generic% type) :test #'eq))

(defgeneric collect-type-variables-generic% (type)
  (:method ((type parser:tyvar))
    (declare (values parser:tyvar-list))
    (list type))

  (:method ((type parser:tycon))
    (declare (values parser:tyvar-list))
    nil)

  (:method ((type parser:tapp))
    (declare (values parser:tyvar-list))
    (nconc (collect-type-variables-generic% (parser:tapp-from type))
           (collect-type-variables-generic% (parser:tapp-to type))))

  (:method ((pred parser:ty-predicate))
    (declare (values parser:tyvar-list))
    (mapcan #'collect-type-variables-generic% (parser:ty-predicate-types pred)))

  (:method ((type parser:qualified-ty))
    (declare (values parser:tyvar-list))
    (nconc
     (collect-type-variables-generic% (parser:qualified-ty-type type))
     (mapcan #'collect-type-variables-generic% (parser:qualified-ty-predicates type))))

  (:method ((ctor parser:constructor))
    (declare (values parser:tyvar-list))
    (mapcan #'collect-type-variables-generic% (parser:constructor-fields ctor)))

  (:method ((type parser:toplevel-define-type))
    (declare (values parser:tyvar-list))
    (mapcan #'collect-type-variables-generic% (parser:toplevel-define-type-ctors type))))
