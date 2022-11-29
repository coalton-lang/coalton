(defpackage #:coalton-impl/typechecker2/define
  (:use
   #:cl
   #:coalton-impl/typechecker2/base
   #:coalton-impl/typechecker2/parse-type)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:error #:coalton-impl/error)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:toplevel-define))

(in-package #:coalton-impl/typechecker2/define)

;;;
;;; Typechecking Environment
;;;

(defstruct (tc-env
            (:predicate nil))
  (env      (util:required 'env)         :type tc:environment :read-only t)
  (ty-table (make-hash-table :test #'eq) :type hash-table     :read-only t))

(defun tc-env-add-variable (env name)
  "Add a variable named NAME to ENV and return the scheme."
  (declare (type tc-env env)
           (type symbol name)
           (values tc:tyvar))

  (when (gethash name (tc-env-ty-table env))
    (util:coalton-bug "Attempt to add already defined variable with name ~S." name))

  (tc:qualified-ty-type (tc:fresh-inst (setf (gethash name (tc-env-ty-table env)) (tc:to-scheme (tc:make-variable))))))

(defun tc-env-lookup-value (env var file)
  "Lookup a variable named VAR in ENV."
  (declare (type tc-env env)
           (type parser:node-variable var)
           (type file-stream file)
           (values tc:ty))

  (let ((ty (gethash (parser:node-variable-name var) (tc-env-ty-table env))))

    (when ty
      (return-from tc-env-lookup-value
        ;; TODO: preds
        (tc:qualified-ty-type (tc:fresh-inst ty))))

    (let ((ty (tc:lookup-value-type (tc-env-env env) (parser:node-variable-name var) :no-error t)))

      (if ty
          (tc:qualified-ty-type (tc:fresh-inst ty)) ; TOOD: preds
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:node-source var)
                       :file file
                       :message "Unknown variable"
                       :primary-note "unknown variable"))))))

(defun tc-env-add-definition (env name scheme)
  "Add a type named NAME to ENV."
  (declare (type tc-env env)
           (type symbol name)
           (type tc:ty-scheme scheme)
           (values null))

  (when (gethash name (tc-env-ty-table env))
    (util:coalton-bug "Attemt to add already defined type with name ~S." name))

  (setf (gethash name (tc-env-ty-table env)) scheme)

  nil)

;;;
;;; Entrypoint
;;;

(defun toplevel-define (defines declares file env)
  "Entrypoint for typechecking a group of parsed defines and declares."
  (declare (type parser:toplevel-define-list defines)
           (type parser:toplevel-declare-list declares)
           (type file-stream file)
           (type tc:environment env))

  (let ((def-table (make-hash-table :test #'eq))

        (dec-table (make-hash-table :test #'eq)))

    ;; Ensure that there are no duplicate definitions
    (loop :for define :in defines
          :for name := (parser:identifier-src-name (parser:toplevel-define-name define))

          :if (gethash name def-table)
            :do (error 'tc-error
                       :err (coalton-error
                             :span (parser:identifier-src-source (parser:toplevel-define-name define))
                             :file file
                             :message "Duplicate definition"
                             :primary-note "second definition here"
                             :notes
                             (list
                              (make-coalton-error-note
                               :type :primary
                               :span (parser:identifier-src-source
                                      (parser:toplevel-define-name
                                       (gethash name def-table)))
                               :message "first defintion here"))))
          :else
            :do (setf (gethash name def-table) define))


    ;; Ensure that there are no duplicate declerations
    (loop :for declare :in declares
          :for name := (parser:identifier-src-name (parser:toplevel-declare-name declare))

          :if (gethash name dec-table)
            :do (error 'tc-error
                       :err (coalton-error
                             :span (parser:identifier-src-source (parser:toplevel-declare-name declare))
                             :file file
                             :message "Duplicate decleration"
                             :primary-note "second decleration here"
                             :notes
                             (list
                              (make-coalton-error-note
                               :type :primary
                               :span (parser:identifier-src-source
                                      (parser:toplevel-declare-name
                                       (gethash name dec-table)))
                               :message "first decleration here"))))
          :else
            :do (setf (gethash name dec-table) declare))

    ;; Ensure that each decleration has an associated definition
    (loop :for declare :in declares
          :for name := (parser:identifier-src-name (parser:toplevel-declare-name declare))

          :unless (gethash name def-table)
            :do (error 'tc-error
                       :err (coalton-error
                             :span (parser:identifier-src-source (parser:toplevel-declare-name declare))
                             :file file
                             :message "Orphan decleration"
                             :primary-note "decleration does not have an associated definition")))

    (let ((parsed-dec-table (make-hash-table :test #'eq)))

      (loop :for declare :in declares
            :for name := (parser:identifier-src-name (parser:toplevel-declare-name declare))
            :for ty := (parse-declare-type declare env file)
            :do (setf (gethash name parsed-dec-table) ty))

      ;; TODO: add parameters to the environment
      (loop :for define :in defines
            :for tc-env := (make-tc-env :env env)
            :for name := (parser:identifier-src-name (parser:toplevel-define-name define))
            :for ty := (infer-expression-type
                        (parser:toplevel-define-body define)
                        (gethash name parsed-dec-table)
                        nil
                        tc-env
                        file)))))

;; TODO: handle predicates here
(defun parse-declare-type (dec env file)
  (declare (type parser:toplevel-declare dec)
           (type tc:environment env)
           (type file-stream file)
           (values tc:ty))

  (let ((partial-env (make-partial-type-env :env env)))

    (loop :for var :in (collect-type-variables (parser:qualified-ty-type (parser:toplevel-declare-type dec)))
          :for var-name := (parser:tyvar-name var)
          :do (partial-type-env-add-var partial-env nil var-name))

    (multiple-value-bind (ty ksubs)
        (infer-type-kinds (parser:qualified-ty-type (parser:toplevel-declare-type dec))
                          tc:+kstar+
                          nil
                          nil
                          partial-env
                          file)

      (let* ((ksubs (tc:kind-monomorphize-subs (tc:kind-variables ty) ksubs))

             (ty (tc:apply-ksubstitution ksubs ty)))

        ty))))

;;;
;;; Expression Type Inference
;;;

(defgeneric infer-expression-type (node expected-type subs env file)
  (:documentation "Infer the type of NODE and then unify against EXPECTED-TYPE")
  (:method ((node parser:node-literal) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list))

    (let ((ty (etypecase (parser:node-literal-value node)
                (ratio tc:*fraction-type* )
                (single-float tc:*single-float-type*)
                (double-float tc:*double-float-type*)
                (string tc:*string-type*)
                (character tc:*char-type*))))

      ;; TODO: this error message isn't very helpful
      (handler-case
          (progn
            (setf subs (tc:unify subs ty expected-type))
            (values
             (tc:apply-substitution subs ty)
             subs))
        (error:coalton-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:node-source node)
                       :file file
                       :message "Type mismatch"
                       :primary-note (format nil "Expected type '~A' but got type '~A'"
                                             (tc:apply-substitution subs expected-type)
                                             (tc:apply-substitution subs ty))))))))

  (:method ((node parser:node-variable) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list))

    (let ((ty (tc-env-lookup-value env node file)))

      (handler-case
          (progn
            (setf subs (tc:unify subs ty expected-type))
            (values
             (tc:apply-substitution subs ty)
             subs))
        (error:coalton-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:node-source node)
                       :file file
                       :message "Type mismatch"
                       :primary-note (format nil "Expected type '~A' but got type '~A'"
                                             (tc:apply-substitution subs expected-type)
                                             (tc:apply-substitution subs ty))))))))

  (:method ((node parser:node-application) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file))

    (when (null (parser:node-application-rands node))
      (error ""))

    (multiple-value-bind (fun-ty subs)
        (infer-expression-type (parser:node-application-rator node)
                               (tc:make-variable)
                               subs
                               env file)

      (let* ((arg-tys (tc:function-type-arguments fun-ty))

             (arity (length arg-tys)))

        ;; Error if the function is over applied
        (when (> (length (parser:node-application-rands node))
                 arity)
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:node-source node)
                       :file file
                       :message "Argument error"
                       :primary-note (format nil "Function has ~D arguments but inferred type '~A' only takes ~D"
                                             (length (parser:node-application-rands node))
                                             fun-ty
                                             arity))))

        (loop :for rand :in (parser:node-application-rands node)
              :for arg-ty :in arg-tys
              :do (multiple-value-bind (rand-ty subs_)
                      (infer-expression-type rand
                                             (tc:apply-substitution subs arg-ty)
                                             subs
                                             env
                                             file)
                    (declare (ignore rand-ty))
                    (setf subs subs_)))

        (let* ((ty (tc:make-function-type*
                    (subseq arg-tys (length (parser:node-application-rands node)))
                    (tc:function-return-type fun-ty)))

               (ty (tc:apply-substitution subs ty)))

          (handler-case
              (progn
                (setf subs (tc:unify subs ty expected-type))
                (values
                 (tc:apply-substitution subs ty)
                 subs))
            (error:coalton-type-error ()
              (error 'tc-error
                     :err (coalton-error
                           :span (parser:node-source node)
                           :file file
                           :message "Type mismatch"
                           :primary-note (format nil "Expected type '~A' but got type '~A'"
                                                 (tc:apply-substitution subs expected-type)
                                                 (tc:apply-substitution subs ty))))))))))

  (:method ((node parser:node-bind) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values null tc:substitution-list))

    (multiple-value-bind (expr-ty subs)
        (infer-expression-type (parser:node-bind-expr node)
                               (tc:make-variable)
                               subs
                               env
                               file)

      (multiple-value-bind (pat-ty subs)
          (infer-pattern-type (parser:node-bind-pattern node)
                              expr-ty   ; unify against expr-ty
                              subs
                              env
                              file)
        (declare (ignore pat-ty))

        (values
         nil                ; return nil as this is always thrown away
         subs))))

  (:method ((node parser:node-body) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list &optional))

    ;; Infer the type of each node
    (loop :for node_ :in (parser:node-body-nodes node)
          :do (multiple-value-bind (node_ty_ subs_)
                  (infer-expression-type node_ (tc:make-variable) subs env file)
                (declare (ignore node_ty_))
                (setf subs subs_)))

    (infer-expression-type (parser:node-body-last-node node) (tc:make-variable) subs env file))

  (:method ((node parser:node-abstraction) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file))

    (let ((arg-tys
            (loop :for var :in (parser:node-abstraction-vars node)
                  :collect (tc-env-add-variable env (parser:node-variable-name var)))))

      (multiple-value-bind (body-ty subs)
          (infer-expression-type (parser:node-abstraction-body node)
                                 (tc:make-variable)
                                 subs
                                 env
                                 file)

        (let ((ty (tc:make-function-type* arg-tys body-ty)))
          (handler-case
              (progn
                (setf subs (tc:unify subs ty expected-type))
                (values
                 (tc:apply-substitution subs ty)
                 subs))
            (error:coalton-type-error ()
              (error 'tc-error
                     :err (coalton-error
                           :span (parser:node-source node)
                           :file file
                           :message "Type mismatch"
                           :primary-note (format nil "Expected type '~A' but got type '~A'"
                                                 (tc:apply-substitution subs expected-type)
                                                 (tc:apply-substitution subs ty))))))))))

  (:method ((node parser:node-let) expected-type subs env file)
    (setf subs (infer-let-bindings (parser:node-let-bindings node) (parser:node-let-declares node) subs env file))

    (multiple-value-bind (ty subs)
        (infer-expression-type (parser:node-let-body node)
                               expected-type ; pass through expected type
                               subs
                               env
                               file)
      (values
       ty
       subs)))

  (:method ((node parser:node-lisp) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file))

    (let ((declared-ty (parse-type (parser:node-lisp-type node) (tc-env-env env) file)))

      (handler-case
          (progn
            (setf subs (tc:unify subs declared-ty expected-type))
            (values
             (tc:apply-substitution subs declared-ty)
             subs))
        (error:coalton-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:node-source node)
                       :file file
                       :message "Type mismatch"
                       :primary-note (format nil "Expected type '~A' but got type '~A'"
                                             (tc:apply-substitution subs expected-type)
                                             (tc:apply-substitution subs declared-ty))))))))

  (:method ((node parser:node-match) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list))

    ;; Infer the type of the expression being cased on
    (multiple-value-bind (expr-ty subs)
        (infer-expression-type (parser:node-match-expr node)
                               (tc:make-variable)
                               subs
                               env
                               file)

      ;; Infer the type of each pattern, unifying against expr-ty
      (loop :for branch :in (parser:node-match-branches node)
            :for pattern := (parser:node-match-branch-pattern branch)
            :do (multiple-value-bind (pat-ty subs_)
                    (infer-pattern-type pattern expr-ty subs env file)
                  (declare (ignore pat-ty))
                  (setf subs subs_)))

      (let ((ret-ty (tc:make-variable)))

        ;; Infer the type of each branch, unifying against ret-ty
        (loop :for branch :in (parser:node-match-branches node)
              :for body := (parser:node-match-branch-body branch)
              :do (multiple-value-bind (body-ty subs_)
                      (infer-expression-type body ret-ty subs env file)
                    (declare (ignore body-ty))
                    (setf subs subs_)))

        (values
         (tc:apply-substitution subs ret-ty)
         subs))))

  (:method ((node parser:node-progn) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list &optional))

    (infer-expression-type (parser:node-progn-body node)
                           expected-type
                           subs
                           env
                           file))


  (:method ((node parser:node-the) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list &optional))

    (let ((declared-ty (parse-type (parser:node-the-type node) (tc-env-env env) file)))

      (multiple-value-bind (expr-ty subs)
          (infer-expression-type (parser:node-the-expr node)
                                 (tc:make-variable)
                                 subs
                                 env
                                 file)

        ;; Ensure subs are applied
        (setf expr-ty (tc:apply-substitution subs expr-ty))

        ;; Check that declared-ty and expr-ty unify
        (handler-case
            (tc:unify subs declared-ty expr-ty)
          (error:coalton-type-error ()
            (error 'tc-error
                   :err (coalton-error
                         :span (parser:node-source node)
                         :file file
                         :message "Type mismatch"
                         :primary-note (format nil "Declared type '~A' does not match inferred type '~A'"
                                               declared-ty 
                                               expr-ty)))))

        ;; Check that declared-ty is not more specific than expr-ty
        (handler-case
            (tc:match expr-ty declared-ty)
          (error:coalton-type-error ()
            (error 'tc-error
                   :err (coalton-error
                         :span (parser:node-source node)
                         :file file
                         :message "Declared type too general"
                         :primary-note (format nil "Declared type '~A' is more general than inferred type '~A'"
                                               declared-ty
                                               expr-ty)))))

        ;; SAFETY: If declared-ty and expr-ty unify, and expr-ty is
        ;; more general than declared-ty then matching should be
        ;; infallible
        (setf subs (tc:compose-substitution-lists subs (tc:match declared-ty expr-ty)))

        (handler-case
            (progn
              (setf subs (tc:unify subs expr-ty expected-type))
              (values
               (tc:apply-substitution subs expr-ty)
               subs))
          (error:coalton-type-error ()
            (error 'tc-error
                   :err (coalton-error
                         :span (parser:node-source node)
                         :file file
                         :message "Type mismatch"
                         :primary-note (format nil "Expected type '~A' but got type '~A'"
                                               (tc:apply-substitution subs expected-type)
                                               (tc:apply-substitution subs expr-ty)))))))))

  ;; TODO: node-return

  (:method ((node parser:node-or) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list))

    (loop :for node_ :in (parser:node-or-nodes node)
          :do (multiple-value-bind (node_ty_ subs_)
                  (infer-expression-type node_
                                         tc:*boolean-type*
                                         subs
                                         env
                                         file)
                (declare (ignore node_ty_))
                (setf subs subs_)))

    (handler-case
        (progn
          (setf subs (tc:unify subs tc:*boolean-type* expected-type))
          (values
           tc:*boolean-type*
           subs))
      (error:coalton-type-error ()
        (error 'tc-error
               :err (coalton-error
                     :span (parser:node-source node)
                     :file file
                     :message "Type mismatch"
                     :primary-note (format nil "Expected type '~A' but 'or' evaluates to '~A'"
                                           (tc:apply-substitution subs expected-type)
                                           tc:*boolean-type*))))))

  (:method ((node parser:node-and) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list))

    (loop :for node_ :in (parser:node-and-nodes node)
          :do (multiple-value-bind (node_ty_ subs_)
                  (infer-expression-type node_
                                         tc:*boolean-type*
                                         subs
                                         env
                                         file)
                (declare (ignore node_ty_))
                (setf subs subs_)))

    (handler-case
        (progn
          (setf subs (tc:unify subs tc:*boolean-type* expected-type))
          (values
           tc:*boolean-type*
           subs))
      (error:coalton-type-error ()
        (error 'tc-error
               :err (coalton-error
                     :span (parser:node-source node)
                     :file file
                     :message "Type mismatch"
                     :primary-note (format nil "Expected type '~A' but 'and' evaluates to '~A'"
                                           (tc:apply-substitution subs expected-type)
                                           tc:*boolean-type*))))))

  (:method ((node parser:node-if) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list))

    (multiple-value-bind (expr-ty subs)
        (infer-expression-type (parser:node-if-expr node)
                               tc:*boolean-type* ; unify predicate against boolean
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (then-ty subs)
          (infer-expression-type (parser:node-if-then node)
                                 (tc:make-variable)
                                 subs
                                 env
                                 file)

        (multiple-value-bind (else-ty subs)
            (infer-expression-type (parser:node-if-else node)
                                   then-ty ; unify against then-ty
                                   subs
                                   env
                                   file)

          (handler-case
              (progn
                (setf subs (tc:unify subs else-ty expected-type))
                (values
                 (tc:apply-substitution subs else-ty)
                 subs))
            (error:coalton-type-error ()
              (error 'tc-error
                     :err (coalton-error
                           :span (parser:node-source node)
                           :file file
                           :message "Type mismatch"
                           :primary-note (format nil "Expected type '~A' but got '~A'"
                                                 (tc:apply-substitution subs expected-type)
                                                 (tc:apply-substitution subs else-ty))))))))))

  (:method ((node parser:node-when) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list))

    (multiple-value-bind (expr-ty subs)
        (infer-expression-type (parser:node-when-expr node)
                               tc:*boolean-type*
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty subs)
          (infer-expression-type (parser:node-when-expr node)
                                 tc:*unit-type*
                                 subs
                                 env
                                 file)

        (handler-case
            (progn
              (setf subs (tc:unify subs body-ty expected-type))
              (values
               tc:*unit-type*
               subs))
          (error:coalton-type-error ()
            (error 'tc-error
                   :err (coalton-error
                         :span (parser:node-source node)
                         :file file
                         :message "Type mismatch"
                         :primary-note (format nil "Expected type '~A' but got '~A'"
                                               (tc:apply-substitution subs body-ty)
                                               (tc:apply-substitution subs expected-type)))))))))

  (:method ((node parser:node-unless) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list))

    (multiple-value-bind (expr-ty subs)
        (infer-expression-type (parser:node-unless-expr node)
                               tc:*boolean-type*
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty subs)
          (infer-expression-type (parser:node-unless-expr node)
                                 tc:*unit-type*
                                 subs
                                 env
                                 file)

        (handler-case
            (progn
              (setf subs (tc:unify subs body-ty expected-type))
              (values
               tc:*unit-type*
               subs))
          (error:coalton-type-error ()
            (error 'tc-error
                   :err (coalton-error
                         :span (parser:node-source node)
                         :file file
                         :message "Type mismatch"
                         :primary-note (format nil "Expected type '~A' but got '~A'"
                                               (tc:apply-substitution subs body-ty)
                                               (tc:apply-substitution subs expected-type)))))))))

  (:method ((node parser:node-cond-clause) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list))

    (multiple-value-bind (expr-ty subs)
        (infer-expression-type (parser:node-cond-clause-expr node)
                               tc:*boolean-type*
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty subs)
          (infer-expression-type (parser:node-cond-clause-body node)
                                 expected-type ; unify against expected-type
                                 subs
                                 env
                                 file)

        (values
         (tc:apply-substitution subs body-ty)
         subs))))

  (:method ((node parser:node-cond) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list))

    (let ((ret-ty (tc:make-variable)))

      (loop :for clause :in (parser:node-cond-clauses node)
            :do (multiple-value-bind (clause-ty subs_)
                    (infer-expression-type clause
                                           ret-ty
                                           subs
                                           env
                                           file)
                  (declare (ignore clause-ty))
                  (setf subs subs_)))

      (handler-case
          (progn
            (setf subs (tc:unify subs ret-ty expected-type))
            (values
             (tc:apply-substitution subs ret-ty)))
        (error:coalton-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:node-source node)
                       :file file
                       :message "Type mismatch"
                       :primary-note (format nil "Expected type '~A' but got '~A'"
                                             (tc:apply-substitution subs expected-type)
                                             (tc:apply-substitution subs ret-ty))))))))

  ;; TODO: node-do
  )
  
;;;
;;; Pattern Type Inference
;;;

(defgeneric infer-pattern-type (pat expected-type subs env file)
  (:documentation "Infer the type of pattern PAT and then unify against EXPECTED-TYPE.")
  (:method ((pat parser:pattern-var) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list))

    (let ((ty (tc-env-add-variable env (parser:pattern-var-name pat))))

      ;; SAFETY: unification against a variable will never fail
      (setf subs (tc:unify subs ty expected-type))

      (values
       (tc:apply-substitution subs ty)
       subs)))

  (:method ((pat parser:pattern-literal) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list))

    (let ((ty (etypecase (parser:pattern-literal-value pat)
                (integer tc:*integer-type*)
                (ratio tc:*fraction-type*)
                (single-float tc:*single-float-type*)
                (double-float tc:*double-float-type*)
                (string tc:*string-type*)
                (character tc:*char-type*))))

      (handler-case
          (progn
            (setf subs (tc:unify ty expected-type subs))
            (values
             (tc:apply-substitution subs ty)
             subs))
        (error:coalton-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:pattern-source pat)
                       :file file
                       :message "Type mismatch"
                       :primary-note (format nil "Expected type '~A' but pattern literal has type '~A'"
                                             expected-type
                                             ty)))))))

  (:method ((pat parser:pattern-wildcard) expected-type subs env file)

    expected-type)

  (:method ((pat parser:pattern-constructor) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type file-stream file)
             (values tc:ty tc:substitution-list))

    (let ((ctor (tc:lookup-constructor (tc-env-env env) (parser:pattern-constructor-name pat) :no-error t)))

      (unless ctor
        (error 'tc-error
               :err (coalton-error
                     :span (parser:pattern-source pat)
                     :file file
                     :message "Unknown constructor"
                     :primary-note "constructor is not known")))


      (let ((arity (tc:constructor-entry-arity ctor))

            (num-args (length (parser:pattern-constructor-patterns pat))))

        (unless (= arity num-args)
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:pattern-source pat)
                       :file file
                       :message "Argument mismatch"
                       :primary-note (format nil "Constructor ~A takes ~D arguments but is given ~D"
                                             (parser:pattern-constructor-name pat)
                                             arity
                                             num-args))))

        (let* ((ctor-ty (tc:qualified-ty-type ;; NOTE: Constructors cannot have predicates
                         (tc:fresh-inst
                          (tc:lookup-value-type (tc-env-env env) (parser:pattern-constructor-name pat)))))

               (pat-ty (tc:function-return-type ctor-ty)))

          
          (loop :for arg :in (parser:pattern-constructor-patterns pat)
                :for arg-ty :in (tc:function-type-arguments ctor-ty)
                :do (multiple-value-bind (ty_ subs_)
                        (infer-pattern-type arg arg-ty subs env file)
                      (declare (ignore ty_))
                      (setf subs subs_)))

          (handler-case
              (progn
                (setf subs (tc:unify subs pat-ty expected-type))
                (values
                 (tc:apply-substitution subs pat-ty)
                 subs))
            (error:coalton-type-error ()
              (error 'tc-error
                     :err (coalton-error
                           :span (parser:pattern-source pat)
                           :file file
                           :message "Type mismatch"
                           :primary-note (format nil "Expected type '~A' but pattern has type '~A'"
                                                 expected-type
                                                 (tc:apply-substitution subs pat-ty)))))))))))

;;;
;;; Binding Group Type Inference
;;;

(defun infer-let-bindings (bindings declares subs env file)
  (declare (type parser:node-let-binding-list bindings)
           (type parser:node-let-declare-list declares)
           (type tc:substitution-list subs)
           (type tc-env env)
           (type file-stream file))

  (let ((def-table (make-hash-table :test #'eq))

        (dec-table (make-hash-table :test #'eq)))

    ;; Ensure that there are no duplicate definitions
    (loop :for binding :in bindings
          :for name := (parser:node-variable-name (parser:node-let-binding-name binding))

          :if (gethash name def-table)
            :do (error 'tc-error
                       :err (coalton-error
                             :span (parser:node-source (parser:node-let-binding-name binding))
                             :file file
                             :message "Duplicate binding in let"
                             :primary-note "second definition here"
                             :notes
                             (list
                              (make-coalton-error-note
                               :type :primary
                               :span (parser:node-source
                                      (parser:node-let-binding-name
                                       (gethash name def-table)))
                               :message "first definition here"))))
          :else
            :do (setf (gethash name def-table) binding))


    ;; Ensure that there are no duplicate declerations
    (loop :for declare :in declares
          :for name := (parser:node-variable-name (parser:node-let-declare-name declare))

          :if (gethash name dec-table)
            :do (error 'tc-error
                       :err (coalton-error
                             :span (parser:node-source (parser:node-let-declare-name declare))
                             :file file
                             :message "Duplicate decleration in let"
                             :primary-note "second decleration here"
                             :notes
                             (list
                              (make-coalton-error-note
                               :type :primary
                               :span (parser:node-source
                                      (parser:node-let-declare-name
                                       (gethash name dec-table)))
                               :message "first decleration here"))))
          :else
            :do (setf (gethash name dec-table) declare))

    ;; Ensure that each decleration has an associated definition
    (loop :for declare :in declares
          :for name := (parser:node-variable-name (parser:node-let-declare-name declare))

          :unless (gethash name def-table)
            :do (error 'tc-error
                       :err (coalton-error
                             :span (parser:node-source (parser:node-let-declare-name declare))
                             :message "Orphan declare in let"
                             :primary-note "decleration does not have an associated definition")))

    ;; Parse the types of all declares
    (loop :for declare :in declares
          :for name := (parser:node-variable-name (parser:node-let-declare-name declare))

          :for ty := (parse-type (parser:node-let-declare-type declare) (tc-env-env env) file)
          :for tyvars := (tc:type-variables ty)
          :for scheme := (tc:quantify tyvars (tc:qualify nil ty))
          :do (util:debug-log name scheme)
          :do (tc-env-add-definition env name scheme))

    (let ((expl-bindings (loop :for binding :in bindings
                               :for name := (parser:node-variable-name (parser:node-let-binding-name binding))

                               :when (gethash name dec-table)
                                 :collect binding))))
    ))
