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
   (#:settings #:coalton-impl/settings)
   (#:tc #:coalton-impl/typechecker/stage-1)
   (#:type-string #:coalton-impl/typechecker/type-string))
  (:export
   #:apply-type-alias-substitutions     ; FUNCTION
   #:parse-type                         ; FUNCTION
   #:parse-output-slot-type             ; FUNCTION
   #:parse-qualified-type               ; FUNCTION
   #:parse-qualified-type-info          ; FUNCTION
   #:parse-ty-scheme                    ; FUNCTION
   #:infer-type-kinds                   ; FUNCTION
   #:infer-predicate-kinds              ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/parse-type)

(defun type-object-string (object env)
  (let ((settings:*coalton-print-unicode* nil))
    (type-string:type-to-string object env)))

(defun normalize-keyword-input-types (entries)
  (declare (type list entries)
           (values tc:keyword-ty-entry-list &optional))
  (sort (copy-list entries)
        #'string<
        :key (lambda (entry)
               (symbol-name (tc:keyword-ty-entry-keyword entry)))))

;;;
;;; Entrypoints
;;;

(defun compute-result-capable-type-variables (type &optional (root-output-slot-p nil))
  "Return the set of type variables that denote whole function-output slots.

These binders may unify with result packs (Void or multiple values).
Any occurrence in an ordinary value position cancels that privilege."
  (declare (type (or parser:ty parser:qualified-ty) type)
           (type boolean root-output-slot-p)
           (values hash-table &optional))
  (let ((result-capable (make-hash-table :test #'eq))
        (ordinary (make-hash-table :test #'eq)))
    (labels ((mark (table name)
               (setf (gethash name table) t))
             (walk (type output-slot-p)
               (typecase type
                 (parser:tyvar
                  (if output-slot-p
                      (mark result-capable (parser:tyvar-name type))
                      (mark ordinary (parser:tyvar-name type))))
                 (parser:tycon
                  nil)
                 (parser:tapp
                  (walk (parser:tapp-from type) nil)
                  (walk (parser:tapp-to type) nil))
                 (parser:keyword-ty-entry
                  (walk (parser:keyword-ty-entry-type type) nil))
                 (parser:function-ty
                  (map nil (lambda (input)
                             (walk input nil))
                       (parser:function-ty-positional-input-types type))
                  (map nil (lambda (entry)
                             (walk entry nil))
                       (parser:function-ty-keyword-input-types type))
                  (let ((outputs (parser:function-ty-output-types type)))
                    (cond
                      ((null outputs)
                       nil)
                      ;; A single output slot may itself be result-polymorphic.
                      ;; Components of an explicit output pack are ordinary
                      ;; value types, not result-pack binders.
                      ((and (null (cdr outputs))
                            (not (typep (first outputs) 'parser:result-ty)))
                       (walk (first outputs) t))
                      (t
                       (map nil (lambda (output)
                                  (walk output nil))
                            outputs)))))
                 (parser:result-ty
                  (map nil (lambda (output)
                             (walk output nil))
                       (parser:result-ty-output-types type)))
                 (parser:qualified-ty
                  (map nil (lambda (pred)
                             (walk pred nil))
                       (parser:qualified-ty-predicates type))
                  (walk (parser:qualified-ty-type type) nil))
                 (parser:ty-predicate
                  (map nil (lambda (pred-type)
                             (walk pred-type nil))
                       (parser:ty-predicate-types type)))
                 (list
                  (map nil (lambda (elt)
                             (walk elt nil))
                       type)))))
      (walk type root-output-slot-p)
      (maphash (lambda (name _value)
                 (declare (ignore _value))
                 (when (gethash name ordinary)
                   (remhash name result-capable)))
               result-capable)
      result-capable)))

(defun seed-qualified-type-variables (unparsed-ty partial-env)
  (declare (type parser:qualified-ty unparsed-ty)
           (type partial-type-env partial-env)
           (values tc:tyvar-list boolean &optional))
  (let ((result-capable-vars (compute-result-capable-type-variables unparsed-ty)))
    (cond
      ((parser:qualified-ty-explicit-p unparsed-ty)
       (let ((explicit-vars (parser:qualified-ty-explicit-variables unparsed-ty)))
         (check-duplicates
          explicit-vars
          #'parser:keyword-src-name
          (lambda (first second)
            (tc-error "Duplicate quantified type variable"
                      (tc-note first "first binding here")
                      (tc-note second "second binding here"))))
         (values
          (loop :for tvar :in explicit-vars
                :for name := (parser:keyword-src-name tvar)
                :collect (partial-type-env-add-var partial-env
                                                   name
                                                   (or (parser:keyword-src-source-name tvar)
                                                       name)
                                                   (gethash name result-capable-vars)))
          t)))
      (t
       (loop :for tvar :in (parser:collect-type-variables unparsed-ty)
             :for tvar-name := (parser:tyvar-name tvar)
             :do (partial-type-env-ensure-var partial-env
                                              tvar-name
                                              (or (parser:tyvar-source-name tvar)
                                                  tvar-name)
                                              (gethash tvar-name result-capable-vars)))
       (values nil nil)))))

(defun parse-qualified-type-internal (unparsed-ty env)
  (declare (type parser:qualified-ty unparsed-ty)
           (type (or tc:environment partial-type-env) env)
           (values tc:qualified-ty tc:tyvar-list boolean &optional))
  (let* ((partial-env (if (typep env 'tc:environment)
                          (make-partial-type-env :env env)
                          env))
         (base-env (partial-type-env-env partial-env)))
    (multiple-value-bind (explicit-tvars explicit-p)
        (seed-qualified-type-variables unparsed-ty partial-env)
      (multiple-value-bind (qual-ty ksubs)
          (infer-type-kinds unparsed-ty tc:+kstar+ nil partial-env)

        (setf qual-ty (tc:apply-ksubstitution ksubs qual-ty))
        (setf qual-ty (tc:make-qualified-ty
                       :predicates (tc:qualified-ty-predicates qual-ty)
                       :type (tc:qualified-ty-type qual-ty)))
        (setf ksubs (tc:kind-monomorphize-subs (tc:kind-variables qual-ty) ksubs))

        (let* ((qual-ty (tc:apply-ksubstitution ksubs qual-ty))
               (explicit-tvars (mapcar (lambda (tvar)
                                         (tc:apply-ksubstitution ksubs tvar))
                                       explicit-tvars))
               (preds (tc:qualified-ty-predicates qual-ty))
               (ty (tc:qualified-ty-type qual-ty))
               (qual-ty (apply-type-alias-substitutions qual-ty unparsed-ty partial-env)))

          (check-for-ambiguous-variables preds ty unparsed-ty base-env)
          (check-for-reducible-by-fundeps preds ty unparsed-ty base-env)
          (check-for-reducible-context preds ty unparsed-ty base-env)

          (values qual-ty explicit-tvars explicit-p))))))

(defun warn-on-unused-explicit-type-variables (unparsed-ty explicit-tvars qual-ty)
  (declare (type parser:qualified-ty unparsed-ty)
           (type tc:tyvar-list explicit-tvars)
           (type tc:qualified-ty qual-ty)
           (values null))
  (let* ((used-tvars (tc:type-variables qual-ty))
         (unused-vars
           (loop :for parser-var :in (parser:qualified-ty-explicit-variables unparsed-ty)
                 :for explicit-tvar :in explicit-tvars
                 :unless (find explicit-tvar used-tvars :test #'tc:ty=)
                   :collect parser-var)))
    (when unused-vars
      (apply #'source:warn
             (if (= 1 (length unused-vars))
                 "Unused quantified type variable"
                 "Unused quantified type variables")
             (source:note
              (source:location (first unused-vars))
              "quantified type variable ~S is not used in the declared type"
              (parser:keyword-src-name (first unused-vars)))
             (loop :for unused-var :in (rest unused-vars)
                   :collect (source:secondary-note
                             (source:location unused-var)
                             "quantified type variable ~S is not used in the declared type"
                             (parser:keyword-src-name unused-var))))))
  nil)

(defgeneric apply-type-alias-substitutions (type parser-type env)
  (:documentation "Replace all type aliases in TYPE with the true types represented by them.")
  (:method ((type tc:tycon) parser-type env)
    (declare (type parser:ty parser-type)
             (type partial-type-env env)
             (values tc:ty))
    (let ((alias (tc:lookup-type-alias (partial-type-env-env env) (tc:tycon-name type) :no-error t)))
      (if alias
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
             (values tc:ty))
    ;; Flatten the type-checker and parser types.
    (let ((flattened-tapp (tc:flatten-type type))
          (flattened-parser-tapp (parser:flatten-type parser-type)))
      ;; Apply substitutions to the type arguments.
      (setf flattened-tapp (cons (first flattened-tapp)
                                 (loop :for tc-ty :in (rest flattened-tapp)
                                       :for parser-ty :in (rest flattened-parser-tapp)
                                       :collect (apply-type-alias-substitutions tc-ty parser-ty env))))
      ;; Check if the foremost tapp-from is an alias.
      (if (typep (first flattened-tapp) 'tc:tycon)
          (let ((alias (tc:lookup-type-alias (partial-type-env-env env) (tc:tycon-name (first flattened-tapp)) :no-error t)))
            (if alias
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
            :do (setf type (nth-value 0 (tc:apply-type-argument type arg))))
      type))

  (:method ((entry tc:keyword-ty-entry) (parser-entry parser:keyword-ty-entry) env)
    (declare (type partial-type-env env))
    (tc:make-keyword-ty-entry
     :keyword (tc:keyword-ty-entry-keyword entry)
     :type (apply-type-alias-substitutions (tc:keyword-ty-entry-type entry)
                                           (parser:keyword-ty-entry-type parser-entry)
                                           env)))

  (:method ((type tc:function-ty) (parser-type parser:function-ty) env)
    (declare
             (type partial-type-env env)
             (values tc:function-ty))
    (tc:make-function-ty
     :alias (mapcar (lambda (alias)
                      (apply-type-alias-substitutions alias parser-type env))
                    (tc:ty-alias type))
     :positional-input-types
     (loop :for input :in (tc:function-ty-positional-input-types type)
           :for parser-input :in (parser:function-ty-positional-input-types parser-type)
           :collect (apply-type-alias-substitutions input parser-input env))
     :keyword-input-types
     (normalize-keyword-input-types
      (loop :for entry :in (tc:function-ty-keyword-input-types type)
            :for parser-entry :in (parser:function-ty-keyword-input-types parser-type)
            :collect (apply-type-alias-substitutions entry parser-entry env)))
     :output-types
     (loop :for output :in (tc:function-ty-output-types type)
           :for parser-output :in (parser:function-ty-output-types parser-type)
           :collect (apply-type-alias-substitutions output parser-output env))))

  (:method ((type tc:result-ty) (parser-type parser:result-ty) env)
    (declare (type partial-type-env env)
             (values tc:result-ty))
    (tc:make-result-ty
     :output-types
     (loop :for output :in (tc:result-ty-output-types type)
           :for parser-output :in (parser:result-ty-output-types parser-type)
           :collect (apply-type-alias-substitutions output parser-output env))))

  (:method ((type tc:qualified-ty) parser-type env)
    (declare (type parser:qualified-ty parser-type)
             (type partial-type-env env)
             (values tc:qualified-ty))
    (tc:make-qualified-ty
     ;; Predicates will have already had their aliases substituted.
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
                         env))
        (result-capable-vars (compute-result-capable-type-variables parser-ty)))

    (loop :for tvar :in (parser:collect-type-variables parser-ty)
          :for tvar-name := (parser:tyvar-name tvar)
          :do (partial-type-env-ensure-var partial-env
                                           tvar-name
                                           (or (parser:tyvar-source-name tvar)
                                               tvar-name)
                                           (gethash tvar-name result-capable-vars)))

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

(defun parse-output-slot-type (parser-ty env &optional ksubs (kind tc:+kstar+))
  "Parse PARSER-TY as a whole function/lisp output slot.

Unlike PARSE-TYPE, a bare type variable here may denote a result pack
(for example `(lisp (-> :a) ...)` in a Void context)."
  (declare (type parser:ty parser-ty)
           (type (or tc:environment partial-type-env) env)
           (type tc:ksubstitution-list ksubs)
           (type tc:kind kind)
           (values tc:ty tc:ksubstitution-list &optional))
  (let ((partial-env (if (typep env 'tc:environment)
                         (make-partial-type-env :env env)
                         env))
        (result-capable-vars (compute-result-capable-type-variables parser-ty t)))

    (loop :for tvar :in (parser:collect-type-variables parser-ty)
          :for tvar-name := (parser:tyvar-name tvar)
          :do (partial-type-env-ensure-var partial-env
                                           tvar-name
                                           (or (parser:tyvar-source-name tvar)
                                               tvar-name)
                                           (gethash tvar-name result-capable-vars)))

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

(defun parse-qualified-type-info (unparsed-ty env &optional ksubs (run-context-checks-p t))
  "Parse UNPARSED-TY and return the parsed type plus explicit binder metadata.

The returned values are:
1. the parsed qualified type
2. the explicit forall binders as instantiated tyvars, in source order
3. whether the source type used an explicit forall
4. the updated kind substitution

When RUN-CONTEXT-CHECKS-P is false, ambiguity, fundep, and reducibility
checks are skipped. That mode is used while class definitions are still
being assembled."
  (declare (type parser:qualified-ty unparsed-ty)
           (type (or tc:environment partial-type-env) env)
           (type tc:ksubstitution-list ksubs)
           (values tc:qualified-ty tc:tyvar-list boolean tc:ksubstitution-list &optional))
  (let* ((partial-env (if (typep env 'tc:environment)
                          (make-partial-type-env :env env)
                          env))
         (base-env (partial-type-env-env partial-env)))
    (multiple-value-bind (explicit-tvars explicit-p)
        (seed-qualified-type-variables unparsed-ty partial-env)
      (multiple-value-bind (qual-ty ksubs)
          (infer-type-kinds unparsed-ty tc:+kstar+ ksubs partial-env)

        (setf qual-ty (tc:apply-ksubstitution ksubs qual-ty))
        (setf qual-ty (tc:make-qualified-ty
                       :predicates (tc:qualified-ty-predicates qual-ty)
                       :type (tc:qualified-ty-type qual-ty)))
        (setf ksubs (tc:kind-monomorphize-subs (tc:kind-variables qual-ty) ksubs))

        (let* ((qual-ty (tc:apply-ksubstitution ksubs qual-ty))
               (explicit-tvars (mapcar (lambda (tvar)
                                         (tc:apply-ksubstitution ksubs tvar))
                                       explicit-tvars))
               (preds (tc:qualified-ty-predicates qual-ty))
               (ty (tc:qualified-ty-type qual-ty))
               (qual-ty (apply-type-alias-substitutions qual-ty unparsed-ty partial-env)))

          (when run-context-checks-p
            (check-for-ambiguous-variables preds ty unparsed-ty base-env)
            (check-for-reducible-by-fundeps preds ty unparsed-ty base-env)
            (check-for-reducible-context preds ty unparsed-ty base-env))

          (values qual-ty explicit-tvars explicit-p ksubs))))))

(defun parse-qualified-type (unparsed-ty env)
  (declare (type parser:qualified-ty unparsed-ty)
           (type (or tc:environment partial-type-env) env)
           (values tc:qualified-ty &optional))
  (nth-value 0 (parse-qualified-type-info unparsed-ty env)))

(defun parse-ty-scheme (ty env)
  (declare (type parser:qualified-ty ty)
           (type (or tc:environment partial-type-env) env)
           (values tc:ty-scheme &optional))

  (let ((in-scope-tvars
          (if (typep env 'partial-type-env)
              (remove-duplicates
               (loop :for type :being :the :hash-values :of (partial-type-env-ty-table env)
                     :when (tc:tyvar-p type)
                       :collect type)
               :test #'tc:ty=)
              nil)))
    (multiple-value-bind (qual-ty explicit-tvars explicit-p)
        (parse-qualified-type-internal ty env)
      (cond
        (explicit-p
         (warn-on-unused-explicit-type-variables ty explicit-tvars qual-ty)
         (tc:quantify-using-tvar-order explicit-tvars qual-ty t))
        (t
         (tc:quantify (set-difference (tc:type-variables qual-ty)
                                      in-scope-tvars
                                      :test #'tc:ty=)
                      qual-ty))))))

(defun check-for-ambiguous-variables (preds type qual-ty env)
  (declare (type tc:ty-predicate-list preds)
           (type tc:ty type)
           (type parser:qualified-ty qual-ty)
           (type tc:environment env))

  (let* ((type-vars (tc:type-variables type))
         (pred-vars (tc:type-variables preds))
         (fundeps (tc:collect-fundep-vars env preds))
         (closure (tc:generic-closure type-vars fundeps :test #'tc:ty=))
         (ambiguous-vars (set-difference pred-vars closure :test #'tc:ty=)))
    (when (consp ambiguous-vars)
      (tc-error
       "Invalid qualified type"
       (tc-note qual-ty
                "The type variable~p ~{~A~^ ~} ~[~;is~:;are~] ~
                 ambiguous in the type ~A"
                (length ambiguous-vars)
                (mapcar (lambda (var)
                          (type-object-string var env))
                        ambiguous-vars)
                (length ambiguous-vars)
                (type-object-string (tc:make-qualified-ty :predicates preds :type type)
                                    env))))))

(defun check-for-reducible-by-fundeps (preds ty unparsed-ty env)
  "This check is used to ensure that PREDs cannot be reduced by instance
definitions in ENV or by each other. 

For example, consider the following class definition.

(define-class (C :a :b (:a -> :b)))

Now, the following predicate list would fail this check, because the
substitution :a +-> :b can be inferred.

((C :a :b) (C :a :c))

Consider the following instance definition.

(define-instance (C :a T))

Now, the following predicate list would also fail this check, because
the substitution :b +-> T can be inferred.

((C :a :b))."
  (declare (type tc:ty-predicate-list preds)
           (type tc:ty ty)
           (type parser:qualified-ty unparsed-ty)
           (type tc:environment env))
  (handler-case
      (let ((subs (nth-value 1 (tc:solve-fundeps env preds nil))))
        (when (consp subs)
          (tc-error
           "Declared context is too general"
           (tc-note
            unparsed-ty
            (with-pprint-variable-context ()
              (format nil "the substitution~p ~{~A +-> ~A~^, ~} ~[~;is~:;are~] ~
                           determined for ~A by functional dependencies."
                      (length subs)
                      (loop :for sub :in subs
                            :collect (type-object-string (tc:substitution-from sub) env)
                            :collect (type-object-string (tc:substitution-to sub) env))
                      (length subs)
                      (type-object-string (tc:make-qualified-ty :predicates preds :type ty)
                                          env)))))))
    (tc:context-fundep-conflict (e)
      (tc-error
       "Context conflicts with functional dependencies"
       (tc-note unparsed-ty
                "the predicates ~A and ~A conflict with functional dependencies"
                (type-object-string (tc:context-fundep-conflict-first-pred e) env)
                (type-object-string (tc:context-fundep-conflict-second-pred e) env))))))

(defun check-for-reducible-context (preds ty qual-ty env)
  (declare (type tc:ty-predicate-list preds)
           (type tc:ty ty)
           (type parser:qualified-ty qual-ty)
           (type tc:environment env))

  (let ((reduced-preds (tc:reduce-context env preds nil)))
    (unless (null (set-exclusive-or preds reduced-preds :test #'tc:type-predicate=))
      (source:warn
       "Declared context can be reduced"
       (source:note
        (source:location qual-ty)
        (if (null reduced-preds)
            "declared predicates are redundant"
            (with-pprint-variable-context ()
              (format nil "qualified type can be reduced to ~A"
                      (type-object-string (tc:make-qualified-ty :predicates reduced-preds :type ty)
                                          env)))))))))

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

          (when (typep fun-ty 'tc:result-ty)
            (tc-error "Malformed type"
                      (tc-note (parser:tapp-from type)
                               "Void and multi-value types cannot be applied as ordinary types")))

          (when (typep arg-ty 'tc:result-ty)
            (tc-error "Malformed type"
                      (tc-note (parser:tapp-to type)
                               "Void and multi-value types cannot be used as type arguments")))

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

  (:method ((entry parser:keyword-ty-entry) expected-kind ksubs env)
    (declare (ignore expected-kind)
             (type tc:ksubstitution-list ksubs)
             (type partial-type-env env)
             (values tc:keyword-ty-entry tc:ksubstitution-list &optional))
    (multiple-value-bind (type ksubs)
        (infer-type-kinds (parser:keyword-ty-entry-type entry) tc:+kstar+ ksubs env)
      (values (tc:make-keyword-ty-entry
               :keyword (parser:keyword-src-name
                         (parser:keyword-ty-entry-keyword entry))
               :type type)
              ksubs)))

  (:method ((type parser:function-ty) expected-kind ksubs env)
    (declare (type tc:kind expected-kind)
             (type tc:ksubstitution-list ksubs)
             (type partial-type-env env)
             (values tc:function-ty tc:ksubstitution-list &optional))
    (handler-case
        (setf ksubs (tc:kunify expected-kind tc:+kstar+ ksubs))
      (tc:coalton-internal-type-error ()
        (tc-error "Kind mismatch"
                  (tc-note type "Expected kind '~S' but function types are of kind '*'"
                           expected-kind))))
    (let ((positional-input-types
            (loop :for input :in (parser:function-ty-positional-input-types type)
                  :collect (multiple-value-bind (input-type ksubs_)
                               (infer-type-kinds input tc:+kstar+ ksubs env)
                             (setf ksubs ksubs_)
                             input-type)))
          (keyword-input-types
            (normalize-keyword-input-types
             (loop :for entry :in (parser:function-ty-keyword-input-types type)
                   :collect (multiple-value-bind (entry-type ksubs_)
                                (infer-type-kinds entry tc:+kstar+ ksubs env)
                              (setf ksubs ksubs_)
                              entry-type))))
          (output-types
            (loop :for output :in (parser:function-ty-output-types type)
                  :collect (multiple-value-bind (output-type ksubs_)
                               (infer-type-kinds output tc:+kstar+ ksubs env)
                             (setf ksubs ksubs_)
                             output-type))))
      (values (tc:make-function-ty
               :positional-input-types positional-input-types
               :keyword-input-types keyword-input-types
               :output-types output-types)
              ksubs)))

  (:method ((type parser:result-ty) expected-kind ksubs env)
    (declare (type tc:kind expected-kind)
             (type tc:ksubstitution-list ksubs)
             (type partial-type-env env)
             (values tc:result-ty tc:ksubstitution-list &optional))
    (handler-case
        (setf ksubs (tc:kunify expected-kind tc:+kstar+ ksubs))
      (tc:coalton-internal-type-error ()
        (tc-error "Kind mismatch"
                  (tc-note type "Expected kind '~S' but output packs are of kind '*'"
                           expected-kind))))
    (let ((output-types
            (loop :for output :in (parser:result-ty-output-types type)
                  :collect (multiple-value-bind (output-type ksubs_)
                               (infer-type-kinds output tc:+kstar+ ksubs env)
                             (setf ksubs ksubs_)
                             output-type))))
      (values (tc:output-types-result-type output-types)
              ksubs)))

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
