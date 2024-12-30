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
   #:apply-type-alias-substitutions          ; FUNCTION
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

(defgeneric apply-type-alias-substitutions (type parser-type env)
  (:documentation "Replace all type aliases in TYPE with the true types represented by them.")
  (:method ((type tc:tycon) parser-type env)
    (declare (type parser:ty parser-type)
             (type partial-type-env env)
             (values tc:ty))
    (let ((alias (tc:lookup-type-alias (partial-type-env-env env) (tc:tycon-name type) :no-error t)))
      (when alias
        ;; Kind information is tracked with type aliases.
        ;; So, kind mismatch is caught earlier and we do not check for it here.
        (if (zerop (length (tc:type-alias-entry-tyvars alias)))
            (setf type (tc:type-alias-entry-type alias))
            (tc-error "Incomplete type alias application"
                      (tc-note parser-type
                               "Type alias ~S is applied to 0 arguments, but ~D argument~:P ~:*~[are~;is~:;are~] required."
                               (tc:type-alias-entry-name alias)
                               (length (tc:type-alias-entry-tyvars alias))))))
      type))

  (:method ((type tc:tapp) parser-type env)
    (declare (type parser:ty parser-type)
             (type partial-type-env env)
             (values tc:tapp))
    ;; Flatten the type-checker and parser types.
    (let ((flattened-tapp (tc:flatten-type type))
          (flattened-parser-tapp (parser:flatten-type parser-type)))
      ;; Apply substitutions to the type arguments.
      (setf flattened-tapp (cons (first flattened-tapp)
                                 (mapcar (lambda (tc-ty parser-ty)
                                           (apply-type-alias-substitutions tc-ty parser-ty env))
                                         (rest flattened-tapp)
                                         (rest flattened-parser-tapp))))
      ;; Check if the foremost tapp-from is an alias.
      (if (typep (first flattened-tapp) 'tc:tycon)
          (let ((alias (tc:lookup-type-alias (partial-type-env-env env) (tc:tycon-name (first flattened-tapp)) :no-error t)))
            (when alias
              (let ((var-count (length (tc:type-alias-entry-tyvars alias)))
                    (arg-count (length (rest flattened-tapp))))
                ;; Kind mismatches are caught earlier.
                ;; Ensure sufficient parameters are supplied.
                (if (> var-count arg-count)
                    (tc-error "Incomplete type alias application"
                              (tc-note parser-type
                                       "Type alias ~S is applied to ~D argument~:P, but ~D argument~:P ~:*~[are~;is~:;are~] required."
                                       (tc:type-alias-entry-name alias)
                                       arg-count
                                       var-count))
                    ;; Apply the type parameters to the parametric type alias.
                    (let ((substs nil))
                      (loop :for var :in (tc:type-alias-entry-tyvars alias)
                            :for arg :in (subseq flattened-tapp 1 (1+ var-count))
                            :do (setf substs (tc:merge-substitution-lists substs (tc:match var arg))))
                      ;; Replace the alias and its parameters with the corresponding type
                      ;; in the flattened type.
                      (setf flattened-tapp (cons (tc:apply-substitution substs (tc:type-alias-entry-type alias))
                                                 (nthcdr var-count (rest flattened-tapp)))))))))
          ;; If the first type in the flattened type is not a tycon,
          ;; then apply alias substitutions directly to it.
          (setf flattened-tapp (cons (apply-type-alias-substitutions (first flattened-tapp) (first flattened-parser-tapp) env)
                                     (rest flattened-tapp))))
      (setf type (first flattened-tapp))
      ;; Reconstruct the flattened type with any remaining types to be applied.
      (loop :for arg :in (rest flattened-tapp)
            :do (setf type (tc:apply-type-argument type arg)))
      type))

  (:method ((type tc:qualified-ty) parser-type env)
    (declare (type parser:qualified-ty parser-type)
             (type partial-type-env env)
             (values tc:qualified-ty))
    (tc:make-qualified-ty
     :predicates (tc:qualified-ty-predicates type)
     :type (apply-type-alias-substitutions (tc:qualified-ty-type type)
                                           (parser:qualified-ty-type parser-type)
                                           env)))

  (:method ((type tc:ty) parser-type env)
    (declare (type parser:ty parser-type)
             (type partial-type-env env)
             (ignore env)
             (values tc:ty))
    type))

(defun parse-type (parser-ty env &optional ksubs (kind tc:+kstar+))
  (declare (type parser:ty parser-ty)
           (type (or tc:environment partial-type-env) env)
           (type tc:ksubstitution-list ksubs)
           (type tc:kind kind)
           (values tc:ty  tc:ksubstitution-list &optional))

  (let ((partial-env (if (typep env 'tc:environment)
                         (make-partial-type-env :env env)
                         env)))

    (when (typep env 'tc:environment)
      (loop :for tvar :in (parser:collect-type-variables parser-ty)
            :for tvar-name := (parser:tyvar-name tvar)
            :do (partial-type-env-add-var partial-env tvar-name)))

    (multiple-value-bind (ty ksubs)
        (infer-type-kinds parser-ty
                          kind
                          ksubs
                          partial-env)

      (setf ty (tc:apply-ksubstitution ksubs ty))
      (setf ksubs (tc:kind-monomorphize-subs (tc:kind-variables ty) ksubs))
      (setf ty (tc:apply-ksubstitution ksubs ty))
      (setf ty (apply-type-alias-substitutions ty parser-ty partial-env))
      (values ty ksubs))))

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
      (setf qual-ty (tc:make-qualified-ty
                     :predicates (tc:qualified-ty-predicates qual-ty)
                     :type (tc:qualified-ty-type qual-ty)))
      (setf ksubs (tc:kind-monomorphize-subs (tc:kind-variables qual-ty) ksubs))

      (let* ((qual-ty (tc:apply-ksubstitution ksubs qual-ty))

             (preds (tc:qualified-ty-predicates qual-ty))

             (ty (tc:qualified-ty-type qual-ty)))

        (check-for-ambiguous-variables preds ty unparsed-ty env)
        (check-for-reducible-context preds unparsed-ty env)

        (apply-type-alias-substitutions qual-ty unparsed-ty partial-env)))))

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
             (type tc:ksubstitution-list ksubs)
             (type partial-type-env env)
             (values tc:ty tc:ksubstitution-list &optional))
    (let* ((tvar (partial-type-env-lookup-var
                  env
                  (parser:tyvar-name type)
                  type))
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
             (values tc:ty tc:ksubstitution-list &optional))

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
             (values tc:qualified-ty tc:ksubstitution-list &optional))

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
                                    (parse-type ty env ksubs (tc:kind-of class-ty))
                                  (setf ksubs ksubs_)
                                  ty))))
      (values (tc:make-ty-predicate :class class-name
                                    :types types)
              ksubs))))
