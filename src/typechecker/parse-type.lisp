;;;;
;;;; Type parsing and kind inference. The implementation is a
;;;; simplified version of the HM type inference used by the type
;;;; checker. After kind inference all remaining kind variables are
;;;; substituted with `tc:+kstar+' in the "kind monomorphization" step
;;;; because Coalton does not support polykinds.
;;;;

(defpackage #:coalton-impl/typechecker/parse-type
  (:use
   #:cl
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/partial-type-env)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:parse-type                         ; FUNCTION
   #:parse-qualified-type               ; FUNCTION
   #:parse-ty-scheme                    ; FUNCTION
   #:infer-type-kinds                   ; FUNCTION
   #:infer-predicate-kinds              ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/parse-type)

;;;
;;; Entrypoints
;;;

(defun parse-type (ty env)
  (declare (type parser:ty ty)
           (type tc:environment env)
           (values tc:ty &optional))

  (let ((tvars (parser:collect-type-variables ty))

        (partial-env (make-partial-type-env :env env)))

    (loop :for tvar :in tvars
          :for tvar-name := (parser:tyvar-name tvar)
          :do (partial-type-env-add-var partial-env tvar-name))

    (multiple-value-bind (ty ksubs)
        (infer-type-kinds ty
                          tc:+kstar+
                          nil
                          partial-env)

      (setf ty (tc:apply-ksubstitution ksubs ty))
      (setf ksubs (tc:kind-monomorphize-subs (tc:kind-variables ty) ksubs))
      (tc:apply-ksubstitution ksubs ty))))

(defun parse-qualified-type (unparsed-ty env)
  (declare (type parser:qualified-ty unparsed-ty)
           (type tc:environment env)
           (values tc:qualified-ty &optional))

  (let ((tvars (parser:collect-type-variables unparsed-ty))

        (partial-env (make-partial-type-env :env env)))

    (loop :for tvar :in tvars
          :for tvar-name := (parser:tyvar-name tvar)
          :do (partial-type-env-add-var partial-env tvar-name))

    (multiple-value-bind (qual-ty ksubs)
        (infer-type-kinds unparsed-ty tc:+kstar+ nil partial-env)

      (setf qual-ty (tc:apply-ksubstitution ksubs qual-ty))
      (setf ksubs (tc:kind-monomorphize-subs (tc:kind-variables qual-ty) ksubs))

      (let* ((qual-ty (tc:apply-ksubstitution ksubs qual-ty))

             (preds (tc:qualified-ty-predicates qual-ty))

             (ty (tc:qualified-ty-type qual-ty)))

        (check-for-ambiguous-variables preds ty unparsed-ty env)
        (check-for-reducible-context preds unparsed-ty env)

        qual-ty))))

(defun parse-ty-scheme (ty env)
  (declare (type parser:qualified-ty ty)
           (type tc:environment env)
           (values tc:ty-scheme &optional))

  (let* ((qual-ty (parse-qualified-type ty env))
         (tvars (tc:type-variables qual-ty)))
    (tc:quantify tvars qual-ty)))

(defun check-for-ambiguous-variables (preds type qual-ty env)
  (declare (type tc:ty-predicate-list preds)
           (type tc:ty type)
           (type parser:qualified-ty qual-ty)
           (type tc:environment env))

  (let* ((old-unambiguous-vars (tc:type-variables type))
         (unambiguous-vars old-unambiguous-vars)) 

    (block fundep-fixpoint
      (loop :for i :below tc:+fundep-max-depth+
            :do (setf old-unambiguous-vars unambiguous-vars)
            :do (loop :for pred :in preds
                      :for pred-tys := (tc:ty-predicate-types pred)
                      :for class-name := (tc:ty-predicate-class pred)
                      :for class := (tc:lookup-class env class-name)
                      :for map := (tc:get-table (tc:ty-class-class-variable-map class))
                      :when (tc:ty-class-fundeps class) :do
                        (loop :for fundep :in (tc:ty-class-fundeps class)
                              :for from-vars := (util:project-map (tc:fundep-from fundep) map pred-tys)
                              :do (when (subsetp from-vars unambiguous-vars :test #'equalp)
                                    (let ((to-vars (util:project-map (tc:fundep-to fundep) map pred-tys)))
                                      (setf unambiguous-vars
                                            (remove-duplicates (append to-vars unambiguous-vars) :test #'equalp))))))

            :when (equalp unambiguous-vars old-unambiguous-vars) ; Exit when progress stops
              :do (return-from fundep-fixpoint)

            :finally (util:coalton-bug "Fundep solving failed to fixpoint")))

    (unless (subsetp (tc:type-variables preds) unambiguous-vars :test #'equalp)
      (let* ((ambiguous-vars (set-difference (tc:type-variables preds) unambiguous-vars :test #'equalp))
             (single-variable (= 1 (length ambiguous-vars))))
        (tc-error "Invalid qualified type"
                  (tc-note qual-ty "The type ~A ~{~S ~}ambiguous in the type ~S"
                           (if single-variable
                               "variable is"
                               "variables are")
                           ambiguous-vars
                           (tc:make-qualified-ty :predicates preds
                                                 :type type)))))))

(defun check-for-reducible-context (preds qual-ty env)
  (declare (type tc:ty-predicate-list preds)
           (type parser:qualified-ty qual-ty)
           (type tc:environment env))
  (let ((reduced-preds (tc:reduce-context env preds nil)))
    (unless (null (set-exclusive-or preds reduced-preds :test #'tc:type-predicate=))
      (source:warn "Declared context can be reduced"
                   (source:note (source:location qual-ty)
                                (if (null reduced-preds)
                                    "declared predicates are redundant"
                                    (format nil "context can be reduced to ~{ ~S~}"
                                            reduced-preds)))))))

;;;
;;; Kind Inference
;;;

(defgeneric infer-type-kinds (type expected-kind ksubs env)
  (:method ((type parser:tyvar) expected-kind ksubs env)
    (declare (type tc:kind expected-kind)
             (type tc:ksubstitution-list ksubs))
    (let* ((tvar (partial-type-env-lookup-var
                  env
                  (parser:tyvar-name type)
                  (parser:ty-location type)))
           (kvar (tc:kind-of tvar)))

      (setf kvar (tc:apply-ksubstitution ksubs kvar))

      (handler-case
          (progn
            (setf ksubs (tc:kunify kvar expected-kind ksubs))
            (values (tc:apply-ksubstitution ksubs tvar) ksubs))
        (tc:coalton-internal-type-error ()
          (tc-error "Kind mismatch"
                    (tc-note type "Expected kind '~S' but variable is of kind '~S'"
                             expected-kind
                             kvar))))))

  (:method ((type parser:tycon) expected-kind ksubs env)
    (declare (type tc:kind expected-kind)
             (type tc:ksubstitution-list ksubs)
             (type partial-type-env env)
             (values tc:ty tc:ksubstitution-list))

    (let ((type_ (partial-type-env-lookup-type env type)))
      (handler-case
          (progn
            (setf ksubs (tc:kunify (tc:kind-of type_) expected-kind ksubs))
            (values (tc:apply-ksubstitution ksubs type_) ksubs))
        (tc:coalton-internal-type-error ()
          (tc-error "Kind mismatch"
                    (tc-note type "Expected kind '~S' but got kind '~S'"
                             expected-kind
                             (tc:kind-of type_)))))))

  (:method ((type parser:tapp) expected-kind ksubs env)
    (declare (type tc:kind expected-kind)
             (type tc:ksubstitution-list ksubs)
             (type partial-type-env env)
             (values tc:ty tc:ksubstitution-list &optional))

    (let ((fun-kind (tc:make-kvariable))
          (arg-kind (tc:make-kvariable)))

      (multiple-value-bind (fun-ty ksubs)
          (infer-type-kinds (parser:tapp-from type) fun-kind ksubs env)

        (setf fun-kind (tc:apply-ksubstitution ksubs fun-kind))

        (when (tc:kfun-p fun-kind)
          ;; SAFETY: unification against variable will never fail
          (setf ksubs (tc:kunify arg-kind (tc:kfun-from fun-kind) ksubs))
          (setf arg-kind (tc:apply-ksubstitution ksubs arg-kind)))

        (multiple-value-bind (arg-ty ksubs)
            (infer-type-kinds (parser:tapp-to type) arg-kind ksubs env)

          (handler-case
              (progn
                (setf ksubs (tc:kunify fun-kind (tc:make-kfun :from arg-kind
                                                              :to expected-kind)
                                       ksubs))
                (values
                 (tc:apply-type-argument fun-ty arg-ty :ksubs ksubs)
                 ksubs))
            (tc:coalton-internal-type-error ()
              (tc-error "Kind mismatch"
                        (tc-note (parser:tapp-from type) "Expected kind '~S' but got kind '~S'"
                                 (tc:make-kfun
                                  :from (tc:apply-ksubstitution ksubs arg-kind)
                                  :to (tc:apply-ksubstitution ksubs expected-kind))
                                 (tc:apply-ksubstitution ksubs fun-kind)))))))))

  (:method ((type parser:qualified-ty) expected-kind ksubs env)
    (declare (type tc:kind expected-kind)
             (type tc:ksubstitution-list ksubs)
             (type partial-type-env env)
             (values tc:qualified-ty tc:ksubstitution-list))

    ;; CCL >:(
    (assert (equalp expected-kind tc:+kstar+))

    (let ((preds (loop :for pred :in (parser:qualified-ty-predicates type)
                       :collect (multiple-value-bind (pred ksubs_)
                                    (infer-predicate-kinds pred ksubs env)
                                  (setf ksubs ksubs_)
                                  pred))))

      (multiple-value-bind (ty ksubs)
          (infer-type-kinds (parser:qualified-ty-type type) tc:+kstar+ ksubs
                            env)
        (values (tc:make-qualified-ty :predicates preds
                                      :type ty)
                ksubs)))))

(defun infer-predicate-kinds (pred ksubs env)
  (declare (type parser:ty-predicate pred)
           (type tc:ksubstitution-list ksubs)
           (type partial-type-env env)
           (values tc:ty-predicate tc:ksubstitution-list))

  (let* ((class-name
           (parser:identifier-src-name (parser:ty-predicate-class pred)))
         (class-pred
           (partial-type-env-lookup-class env pred))
         (class-arity
           (length (tc:ty-predicate-types class-pred))))

    ;; Check that pred has the correct number of arguments
    (unless (= class-arity (length (parser:ty-predicate-types pred)))
      (tc-error "Predicate arity mismatch"
                (tc-note pred "Expected ~D arguments but received ~D"
                         class-arity
                         (length (parser:ty-predicate-types pred)))))

    (let ((types (loop :for ty :in (parser:ty-predicate-types pred)
                       :for class-ty :in (tc:ty-predicate-types class-pred)
                       :collect (multiple-value-bind (ty ksubs_)
                                    (infer-type-kinds ty (tc:kind-of class-ty)
                                                      ksubs
                                                      env)
                                  (setf ksubs ksubs_)
                                  ty))))
      (values (tc:make-ty-predicate :class class-name
                                    :types types)
              ksubs))))
