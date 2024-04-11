(defpackage #:coalton-impl/entry
  (:use
   #:cl)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)
   (#:analysis #:coalton-impl/analysis)
   (#:codegen #:coalton-impl/codegen))
  (:export
   #:*global-environment*
   #:entry-point                        ; FUNCTION
   #:expression-entry-point             ; FUNCTION
   ))

(in-package #:coalton-impl/entry)

(defvar *global-environment* (tc:make-default-environment))

;; This is the compiler entry point. PROGRAM is compiled using the
;; current entry:*global-environment*, with environment and code
;; emitted to OUT, which should implement codegen:emit.

(defun entry-point (program out)
  (declare (type parser:program program))

  (let* ((*package* (parser:program-package program))

         (program (parser:rename-variables program))

         (file (parser:program-file program))

         (env *global-environment*))
    (tc:with-update-hook (lambda (name args)
                           (codegen:emit-env out name args))
      (multiple-value-bind (type-definitions instances env)
          (tc:toplevel-define-type (parser:program-types program)
                                   (parser:program-structs program)
                                   file
                                   env)
        (multiple-value-bind (class-definitions env)
            (tc:toplevel-define-class (parser:program-classes program)
                                      file
                                      env)
          (multiple-value-bind (ty-instances env)
              (tc:toplevel-define-instance (append instances (parser:program-instances program)) env file)
            (multiple-value-bind (toplevel-definitions env)
                (tc:toplevel-define (parser:program-defines program)
                                    (parser:program-declares program)
                                    file
                                    env)
              (multiple-value-bind (toplevel-instances)
                  (tc:toplevel-typecheck-instance ty-instances
                                                  (append instances (parser:program-instances program))
                                                  env
                                                  file)
                (setf env (tc:toplevel-specialize (parser:program-specializations program) env file))
                (let ((monomorphize-table (make-hash-table :test #'eq))
                      (translation-unit (tc:make-translation-unit :types type-definitions
                                                                  :definitions toplevel-definitions
                                                                  :classes class-definitions
                                                                  :instances toplevel-instances
                                                                  :package *package*)))
                  (loop :for define :in (parser:program-defines program)
                        :when (parser:toplevel-define-monomorphize define)
                          :do (setf (gethash (parser:node-variable-name (parser:toplevel-define-name define))
                                             monomorphize-table)
                                    t))
                  (loop :for declare :in (parser:program-declares program)
                        :when (parser:toplevel-declare-monomorphize declare)
                          :do (setf (gethash (parser:identifier-src-name (parser:toplevel-declare-name declare))
                                             monomorphize-table)
                                    t))
                  (analysis:analyze-translation-unit translation-unit env file)
                  (emit-prologue out)
                  (codegen:compile-translation-unit out translation-unit monomorphize-table env))))))))))


(defun emit-prologue (out)
  (codegen:emit out
                `(eval-when (:load-toplevel)
                   (unless (eq (settings:coalton-release-p)
                               ,(settings:coalton-release-p))
                     ,(if (settings:coalton-release-p)
                          `(error "~A was compiled in release mode but loaded in development."
                                  ,(or *compile-file-pathname* *load-truename*))
                          `(error "~A was compiled in development mode but loaded in release."
                                  ,(or *compile-file-pathname* *load-truename*)))))))


(defun expression-entry-point (node file)
  (declare (type parser:node node)
           (type error:coalton-file file))

  (let ((env *global-environment*))

    (multiple-value-bind (ty preds accessors node subs)
        (tc:infer-expression-type (parser:rename-variables node)
                                  (tc:make-variable)
                                  nil
                                  (tc:make-tc-env :env env)
                                  file)

      (multiple-value-bind (preds subs)
          (tc:solve-fundeps env preds subs)

        (setf accessors (tc:apply-substitution subs accessors))

        (multiple-value-bind (accessors subs_)
            (tc:solve-accessors accessors file env)
          (setf subs (tc:compose-substitution-lists subs subs_))

          (when accessors
            (error 'tc:tc-error
                   :err (tc:coalton-error
                         :span (tc:accessor-source (first accessors))
                         :file file
                         :message "Ambiguous accessor"
                         :primary-note "accessor is ambiguous")))

          (let* ((preds (tc:reduce-context env preds subs))
                 (subs (tc:compose-substitution-lists
                        (tc:default-subs env nil preds)
                        subs))
                 (preds (tc:reduce-context env preds subs))

                 (node (tc:apply-substitution subs node))
                 (ty (tc:apply-substitution subs ty))

                 (qual-ty (tc:qualify preds ty))
                 (scheme (tc:quantify (tc:type-variables qual-ty) qual-ty)))

            (when (null preds)
              (return-from expression-entry-point
                (let ((node (codegen:optimize-node
                             (codegen:translate-expression node nil env)
                             env)))
                  (codegen:codegen-expression 
                   (codegen:direct-application
                    node
                    (codegen:make-function-table env))
                   nil
                   env))))

            (let* ((tvars
                     (loop :for i :to (1- (length (remove-duplicates (tc:type-variables qual-ty)
                                                                     :test #'equalp)))
                           :collect (tc:make-variable)))
                   (qual-type (tc:instantiate
                               tvars
                               (tc:ty-scheme-type scheme))))

              (error 'tc:tc-error
                     :err (error:coalton-error
                           :span (tc:node-source node)
                           :file file
                           :message "Unable to codegen"
                           :primary-note (format nil
                                                 "expression has type ~A~{ ~S~}.~{ (~S)~} => ~S with unresolved constraint~A ~S"
                                                 (if settings:*coalton-print-unicode*
                                                     "∀"
                                                     "FORALL")
                                                 tvars
                                                 (tc:qualified-ty-predicates qual-type)
                                                 (tc:qualified-ty-type qual-type)
                                                 (if (= (length (tc:qualified-ty-predicates qual-type)) 1)
                                                     ""
                                                     "s")
                                                 (tc:qualified-ty-predicates qual-type))
                           :notes
                           (list
                            (error:make-coalton-error-note
                             :type :secondary
                             :span (tc:node-source node)
                             :message "Add a type assertion with THE to resolve ambiguity")))))))))))
