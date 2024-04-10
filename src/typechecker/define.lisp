;;;;
;;;; Type inference for toplevel definitions and expressions. The
;;;; implementation is based on the paper "Typing Haskell in Haskell"
;;;; by Jones.
;;;;
;;;; This implementation makes use of two insights. The first is that
;;;; by renaming all local bindings the names of variables will never
;;;; conflict, so bindings can be stored in a hash table and variable
;;;; shadowing can be ignored entirely. The second is that the "base
;;;; environment" that stores type types of bindings from previous
;;;; compiler invocations will never contain any free variables. This
;;;; allows skipping searching the base environment for type variables
;;;; and applying substitutions to it.
;;;;

(defpackage #:coalton-impl/typechecker/define
  (:use
   #:cl
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/parse-type
   #:coalton-impl/typechecker/pattern
   #:coalton-impl/typechecker/expression
   #:coalton-impl/typechecker/traverse
   #:coalton-impl/typechecker/toplevel
   #:coalton-impl/typechecker/accessor
   #:coalton-impl/typechecker/tc-env)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:parser #:coalton-impl/parser)
   (#:error #:coalton-impl/error)
   (#:tc #:coalton-impl/typechecker/stage-1)
   (#:types #:coalton-impl/typechecker/types))
  (:export
   #:infer-expression-type              ; FUNCTION
   #:infer-expl-binging-type            ; FUNCTION
   #:attach-explicit-binding-type       ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/define)

(declaim (type (member :toplevel :lambda :do) *return-status*))
(defparameter *return-status* :toplevel)

(deftype node-return-info ()
  '(cons cons tc:ty))

(defun node-return-info-p (x)
  (typep x 'node-return-info))

(defun node-return-info-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-return-info-p x)))

(deftype node-return-info-list ()
  '(satisfies node-return-info-list-p))

(declaim (type node-return-info-list *returns*))
(defparameter *returns* nil)

(defun error-ambiguous-pred (pred file)
  (declare (type tc:ty-predicate pred)
           (type coalton-file file))

  (unless (tc:ty-predicate-source pred)
    (util:coalton-bug "Predicate ~S does not have source information" pred))

  (error 'tc-error
         :err (coalton-error
               :span (tc:ty-predicate-source pred)
               :file file
               :message "Ambiguous predicate"
               :primary-note (format nil "Ambiguous predicate ~S" pred))))

(defun error-unknown-pred (pred file)
  (declare (type tc:ty-predicate pred)
           (type coalton-file file))

  (unless (tc:ty-predicate-source pred)
    (util:coalton-bug "Predicate ~S does not have source information" pred))

  (error 'tc-error
         :err (coalton-error
               :message "Unknown instance"
               :span (tc:ty-predicate-source pred)
               :file file
               :primary-note (format nil "Unknown instance ~S" pred))))


(defun standard-expression-type-mismatch-error (node file subs expected-type ty)
  "Utility for signalling a type-mismatch error in INFER-EXPRESSION-TYPE"
  (error 'tc-error
         :err (coalton-error
               :span (parser:node-source node)
               :file file
               :message "Type mismatch"
               :primary-note (format nil "Expected type '~S' but got '~S'"
                                     (tc:apply-substitution subs expected-type)
                                     (tc:apply-substitution subs ty)))))
;;;
;;; Entrypoint
;;;

(defun toplevel-define (defines declares file env)
  "Entrypoint for typechecking a group of parsed defines and declares."
  (declare (type parser:toplevel-define-list defines)
           (type parser:toplevel-declare-list declares)
           (type coalton-file file)
           (type tc:environment env)
           (values toplevel-define-list tc:environment))

  ;; Ensure that all defines are in the current package
  (check-package
   defines
   (alexandria:compose #'parser:node-variable-name #'parser:toplevel-define-name)
   (alexandria:compose #'parser:node-source #'parser:toplevel-define-name)
   file)

  ;; Ensure that there are no duplicate definitions
  (check-duplicates
   defines
   (alexandria:compose #'parser:node-variable-name #'parser:toplevel-define-name)
   #'parser:toplevel-define-source
   (lambda (first second)
     (error 'tc-error
            :err (coalton-error
                  :span (parser:node-source (parser:toplevel-define-name first))
                  :file file
                  :message "Duplicate definition"
                  :primary-note "first definition here"
                  :notes
                  (list
                   (make-coalton-error-note
                    :type :primary
                    :span (parser:node-source (parser:toplevel-define-name second))
                    :message "second definition here"))))))

  ;; Ensure that there are no duplicate declarations
  (check-duplicates
   declares
   (alexandria:compose #'parser:identifier-src-name #'parser:toplevel-declare-name)
   #'parser:toplevel-define-source
   (lambda (first second)
     (error 'tc-error
            :err (coalton-error
                  :span (parser:identifier-src-source (parser:toplevel-declare-name first))
                  :file file
                  :message "Duplicate declaration"
                  :primary-note "first declaration here"
                  :notes
                  (list
                   (make-coalton-error-note
                    :type :primary
                    :span (parser:identifier-src-source (parser:toplevel-declare-name second))
                    :message "second declaration here"))))))

  ;; Ensure that each declaration has an associated definition
  (loop :with def-table
          := (loop :with table := (make-hash-table :test #'eq)

                   :for def :in defines
                   :for name := (parser:node-variable-name
                                 (parser:toplevel-define-name def))

                   :do (setf (gethash name table) def)

                   :finally (return table))

        :for declare :in declares
        :for name := (parser:identifier-src-name (parser:toplevel-declare-name declare))

        :unless (gethash name def-table)
          :do (error 'tc-error
                     :err (coalton-error
                           :span (parser:identifier-src-source (parser:toplevel-declare-name declare))
                           :file file
                           :message "Orphan declaration"
                           :primary-note "declaration does not have an associated definition")))

  (let ((dec-table (make-hash-table :test #'eq))

        (tc-env (make-tc-env :env env)))

    (loop :for declare :in declares
          :for name := (parser:identifier-src-name (parser:toplevel-declare-name declare))
          :for ty := (parser:toplevel-declare-type declare)
          :do (setf (gethash name dec-table) ty))

    ;; Infer binding types, returning the typed nodes.
    (multiple-value-bind (preds accessors binding-nodes subs)
        (infer-bindings-type defines dec-table nil tc-env file)
      (assert (null preds))
      (assert (null accessors))

      (let (;; Attach explicit types to any explicit bindings

            (binding-nodes
              (loop :for node :in binding-nodes
                    :for explicit-type := (gethash (toplevel-define-name node) dec-table)
                    :if explicit-type
                      :collect (attach-explicit-binding-type node explicit-type)
                    :else
                      :collect node)))

        (loop :for define :in defines
              :for name := (parser:node-variable-name (parser:binding-name define))
              :for scheme := (tc:remove-source-info (gethash name (tc-env-ty-table tc-env)))

              :when (tc:type-variables scheme)
                :do (util:coalton-bug "Scheme ~S should not have any free type variables." scheme)

              :do (setf env (tc:set-value-type env name scheme))

              :do (setf env (tc:set-name env name (tc:make-name-entry
                                                   :name name
                                                   :type :value
                                                   :docstring (parser:toplevel-define-docstring define)
                                                   :location (error:coalton-file-name file))))

              :if (parser:toplevel-define-orig-params define)
                :do (setf env (tc:set-function-source-parameter-names
                               env
                               name
                               (parser:toplevel-define-orig-params define)))
              :else
                :when (tc:lookup-function-source-parameter-names env name)
                  :do (setf env (tc:unset-function-source-parameter-names env name)))


        (values
         (tc:apply-substitution subs binding-nodes)
         env)))))

;;;
;;; Expression Type Inference
;;;



(defgeneric infer-expression-type (node expected-type subs env file)
  (:documentation "Infer the type of NODE and then unify against EXPECTED-TYPE

Returns (VALUES INFERRED-TYPE PREDICATES NODE SUBSTITUTIONS)")
  (:method ((node parser:node-literal) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-literal tc:substitution-list))

    (let ((ty (etypecase (parser:node-literal-value node)
                (ratio tc:*fraction-type*)
                (single-float tc:*single-float-type*)
                (double-float tc:*double-float-type*)
                (string tc:*string-type*)
                (character tc:*char-type*))))

      (handler-case
          (progn
            (setf subs (tc:unify subs ty expected-type))
            (let ((type (tc:apply-substitution subs ty)))
              (values
               type
               nil
               nil
               (make-node-literal
                :type (tc:qualify nil type)
                :source (parser:node-source node)
                :value (parser:node-literal-value node))
               subs)))

        (error:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node file subs expected-type ty)))))

  (:method ((node parser:node-accessor) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-accessor tc:substitution-list))

    (let* ((from-ty (tc:make-variable))

           (to-ty (tc:make-variable))

           (ty (tc:make-function-type from-ty to-ty)))

      (handler-case
          (progn
            (setf subs (tc:unify subs ty expected-type))
            (let ((type (tc:apply-substitution subs ty)))
              (values
               type
               nil
               (list
                (make-accessor
                 :from from-ty
                 :to to-ty
                 :field (parser:node-accessor-name node)
                 :source (parser:node-source node)))
               (make-node-accessor
                :type (tc:qualify nil type)
                :source (parser:node-source node)
                :name (parser:node-accessor-name node))
               subs))))))

  (:method ((node parser:node-integer-literal) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-integer-literal tc:substitution-list))

    (let* ((classes-package (find-package "COALTON-LIBRARY/CLASSES"))

           (num (util:find-symbol "NUM" classes-package))

           (tvar (tc:make-variable))

           (pred (tc:make-ty-predicate :class num :types (list tvar) :source (parser:node-source node))))

      (handler-case
          (progn
            (setf subs (tc:unify subs tvar expected-type))
            (let ((type (tc:apply-substitution subs tvar)))
              (values
               type
               (list pred)
               nil
               (make-node-integer-literal
                :type (tc:qualify nil type)
                :source (parser:node-source node)
                :value (parser:node-integer-literal-value node))
               subs)))
        (error:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node file subs expected-type tvar)))))

  (:method ((node parser:node-variable) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-variable tc:substitution-list))

    (multiple-value-bind (ty preds)
        (tc-env-lookup-value env node file)

      (handler-case
          (progn
            (setf subs (tc:unify subs ty expected-type))

            (let ((type (tc:apply-substitution subs ty))
                  (preds (tc:apply-substitution subs preds)))
              (values
               type
               preds
               nil
               (make-node-variable
                :type (tc:qualify preds type)
                :source (parser:node-source node)
                :name (parser:node-variable-name node))
               subs)))
        (error:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node file subs expected-type ty)))))

  (:method ((node parser:node-application) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-application tc:substitution-list))

    (multiple-value-bind (fun-ty preds accessors rator-node subs)
        (infer-expression-type (parser:node-application-rator node)
                               (tc:make-variable)
                               subs
                               env
                               file)

      (let* ((rands (or (parser:node-application-rands node)
                        (list
                         (parser:make-node-variable
                          :source (parser:node-source node)
                          :name 'coalton:unit))))

             (fun-ty_ fun-ty)
             (rand-nodes
               ;; Apply arguments one at a time for better error messages
               (loop :for rand :in rands
                     :collect (cond
                                ;; If the rator is a function then unify against its argument
                                ((tc:function-type-p fun-ty_)
                                 (multiple-value-bind (ty_ preds_ accessors_ node_ subs_)
                                     (infer-expression-type rand
                                                            (tc:function-type-from fun-ty_)
                                                            subs
                                                            env
                                                            file)
                                   (declare (ignore ty_))
                                   (setf preds (append preds preds_))
                                   (setf accessors (append accessors accessors_))
                                   (setf subs subs_)
                                   (setf fun-ty_ (tc:function-type-to fun-ty_))

                                   node_))

                                ;; If the rator is variable then unify against a new function type
                                ((tc:tyvar-p fun-ty_)
                                 (let* ((new-from (tc:make-variable))

                                        (new-to (tc:make-variable))

                                        (new-ty (tc:make-function-type new-from new-to)))

                                   (setf subs (tc:unify subs fun-ty_ new-ty))
                                   (multiple-value-bind (ty_ preds_ accessors_ node_ subs_)
                                       (infer-expression-type rand
                                                              new-from
                                                              subs
                                                              env
                                                              file)
                                     (declare (ignore ty_))
                                     (setf preds (append preds preds_))
                                     (setf accessors (append accessors accessors_))
                                     (setf subs subs_)
                                     (setf fun-ty_ new-to)

                                     node_)))

                                ;; Otherwise signal an error
                                (t
                                 (setf fun-ty (tc:apply-substitution subs fun-ty))

                                 (error 'tc-error
                                        :err (coalton-error
                                              :span (parser:node-source node)
                                              :file file
                                              :message "Argument error"
                                              :primary-note (if (null (tc:function-type-arguments fun-ty))
                                                                (format nil "Unable to call '~S' it is not a function"
                                                                        fun-ty)
                                                                (format nil "Function call has ~D arguments but inferred type '~S' only takes ~D"
                                                                        (length rands)
                                                                        fun-ty
                                                                        (length (tc:function-type-arguments fun-ty)))))))))))

        (handler-case
            (progn
              (setf subs (tc:unify subs fun-ty_ expected-type))
              (let ((type (tc:apply-substitution subs fun-ty_)))
                (values
                 type
                 preds
                 accessors
                 (make-node-application
                  :type (tc:qualify nil type)
                  :source (parser:node-source node)
                  :rator rator-node
                  :rands rand-nodes)
                 subs)))
          (error:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node file subs expected-type fun-ty_))))))

  (:method ((node parser:node-bind) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values null tc:ty-predicate-list accessor-list node-bind tc:substitution-list))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-bind-expr node)
                               (tc:make-variable)
                               subs
                               env
                               file)

      (multiple-value-bind (pat-ty pat-node subs)
          (infer-pattern-type (parser:node-bind-pattern node)
                              expr-ty   ; unify against expr-ty
                              subs
                              env
                              file)
        (declare (ignore pat-ty))

        (values
         nil                ; return nil as this is always thrown away
         preds
         accessors
         (make-node-bind
          ;; NOTE: We don't attach type here because NODE-BIND has no
          ;; meaningful type.
          :source (parser:node-bind-source node)
          :pattern pat-node
          :expr expr-node)
         subs))))

  (:method ((node parser:node-body) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-body tc:substitution-list))

    (let* ((preds nil)
           (accessors nil)

           ;; Infer the type of each node
           (body-nodes
             (loop :for node_ :in (parser:node-body-nodes node)
                   :collect (multiple-value-bind (node_ty_ preds_ accessors_ node_ subs_)
                                (infer-expression-type node_ (tc:make-variable) subs env file)
                              (declare (ignore node_ty_))
                              (setf subs subs_)
                              (setf preds (append preds preds_))
                              (setf accessors (append accessors accessors_))
                              node_))))

      (multiple-value-bind (ty preds_ accessors_ last-node subs)
          (infer-expression-type (parser:node-body-last-node node) expected-type subs env file)
        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (values
         ty
         preds
         accessors
         (make-node-body
          :nodes body-nodes
          :last-node last-node)
         subs))))

  (:method ((node parser:node-abstraction) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-abstraction tc:substitution-list))

    (check-duplicates
     (parser:pattern-variables (parser:node-abstraction-params node))
     #'parser:pattern-var-name
     #'parser:pattern-source
     (lambda (first second)
       (error 'tc-error
              :err (coalton-error
                    :span (parser:node-source first)
                    :file file
                    :message "Duplicate parameters name"
                    :primary-note "first parameter here"
                    :notes
                    (list
                     (make-coalton-error-note
                      :type :primary
                      :span (parser:node-source second)
                      :message "second parameter here"))))))

    (let* (;; Setup return environment
           (*return-status* :lambda)
           (*returns* nil)

           (arg-tys (if (null (parser:node-abstraction-params node))
                        (list tc:*unit-type*)
                        (loop :for pat :in (parser:node-abstraction-params node)
                              :collect (tc:make-variable))))

           (params
             (if (null (parser:node-abstraction-params node))
                 (list
                  (make-pattern-wildcard
                   :type (tc:qualify nil (first arg-tys))
                   :source (parser:node-source node)))
                 (loop :for pattern :in (parser:node-abstraction-params node)
                       :for ty :in arg-tys
                       :collect (multiple-value-bind (ty_ pattern subs_)
                                    (infer-pattern-type pattern ty subs env file)
                                  (declare (ignore ty_))
                                  (setf subs subs_)
                                  pattern)))))

      (multiple-value-bind (body-ty preds accessors body-node subs)
          (infer-expression-type (parser:node-abstraction-body node)
                                 (tc:make-variable)
                                 subs
                                 env
                                 file)

        ;; Ensure that all early returns unify
        (loop :with returns := (reverse *returns*)
              :for (s1 . ty1) :in returns
              :for (s2 . ty2) :in (cdr returns)
              :do (handler-case
                      (setf subs (tc:unify subs ty1 ty2))
                    (error:coalton-internal-type-error ()
                      (error 'tc-error
                             :err (coalton-error
                                   :span s1
                                   :file file
                                   :message "Return type mismatch"
                                   :primary-note (format nil "First return is of type '~S'"
                                                         (tc:apply-substitution subs ty1))
                                   :notes
                                   (list
                                    (make-coalton-error-note
                                     :type :primary
                                     :span s2
                                     :message (format nil "Second return is of type '~S'"
                                                      (tc:apply-substitution subs ty2)))))))))

        ;; Unify the function's inferred type with one of the early returns.
        (when *returns*
          (handler-case
              (setf subs (tc:unify subs (cdr (first *returns*)) body-ty))
            (error:coalton-internal-type-error ()
              (error 'tc-error
                     :err (coalton-error
                           :span (car (first *returns*))
                           :file file
                           :message "Return type mismatch"
                           :primary-note (format nil "First return is of type '~S'"
                                                 (tc:apply-substitution subs (cdr (first *returns*))))
                           :notes
                           (list
                            (make-coalton-error-note
                             :type :primary
                             :span (parser:node-source (parser:node-body-last-node (parser:node-abstraction-body node)))
                             :message (format nil "Second return is of type '~S'"
                                              (tc:apply-substitution subs body-ty)))))))))

        (let ((ty (tc:make-function-type* arg-tys body-ty)))
          (handler-case
              (progn
                (setf subs (tc:unify subs ty expected-type))
                (let ((type (tc:apply-substitution subs ty)))
                  (values
                   type
                   preds
                   accessors
                   (make-node-abstraction
                    :type (tc:qualify nil type)
                    :source (parser:node-source node)
                    :params params
                    :body body-node)
                   subs)))
            (error:coalton-internal-type-error ()
              (standard-expression-type-mismatch-error node file subs expected-type ty)))))))

  (:method ((node parser:node-let) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-let tc:substitution-list))

    ;; Ensure that there are no duplicate let bindings
    (check-duplicates
     (parser:node-let-bindings node)
     (alexandria:compose #'parser:node-variable-name #'parser:node-let-binding-name)
     #'parser:node-let-binding-source
     (lambda (first second)
       (error 'tc-error
              :err (coalton-error
                    :span (parser:node-let-binding-source first)
                    :file file
                    :message "Duplicate definition in let"
                    :primary-note "first definition here"
                    :notes
                    (list
                     (make-coalton-error-note
                      :type :primary
                      :span (parser:node-let-binding-source second)
                      :message "second definition here"))))))

    (multiple-value-bind (preds accessors binding-nodes subs)
        (infer-let-bindings (parser:node-let-bindings node) (parser:node-let-declares node) subs env file)

      (multiple-value-bind (ty preds_ accessors_ body-node subs)
          (infer-expression-type (parser:node-let-body node)
                                 expected-type ; pass through expected type
                                 subs
                                 env
                                 file)
        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (values
         ty
         preds
         accessors
         (make-node-let
          :type (tc:qualify nil ty)
          :source (parser:node-source node)
          :bindings binding-nodes
          :body body-node)
         subs))))

  (:method ((node parser:node-lisp) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-lisp tc:substitution-list))

    (let ((declared-ty (parse-type (parser:node-lisp-type node) (tc-env-env env) file)))

      (handler-case
          (progn
            (setf subs (tc:unify subs declared-ty expected-type))
            (let ((type (tc:apply-substitution subs declared-ty))

                  (var-nodes
                    (mapcar (lambda (var)
                              (make-node-variable
                               :type (tc:qualify nil (tc-env-lookup-value env var file))
                               :source (parser:node-source var)
                               :name (parser:node-variable-name var)))
                            (parser:node-lisp-vars node))))
              (values
               type
               nil
               nil
               (make-node-lisp
                :type (tc:qualify nil type)
                :source (parser:node-source node)
                :vars var-nodes
                :var-names (parser:node-lisp-var-names node)
                :body (parser:node-lisp-body node))
               subs)))
        (error:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node file subs expected-type declared-ty)))))

  (:method ((node parser:node-match) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-match tc:substitution-list))

    ;; Infer the type of the expression being cased on
    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-match-expr node)
                               (tc:make-variable)
                               subs
                               env
                               file)

      (let* (;; Infer the type of each pattern, unifying against expr-ty
             (pat-nodes
               (loop :for branch :in (parser:node-match-branches node)
                     :for pattern := (parser:node-match-branch-pattern branch)
                     :collect (multiple-value-bind (pat-ty pat-node subs_)
                                  (infer-pattern-type pattern expr-ty subs env file)
                                (declare (ignore pat-ty))
                                (setf subs subs_)
                                pat-node)))

             (ret-ty (tc:make-variable))

             ;; Infer the type of each branch, unifying against ret-ty
             (branch-body-nodes
               (loop :for branch :in (parser:node-match-branches node)
                     :for body := (parser:node-match-branch-body branch)
                     :collect (multiple-value-bind (body-ty preds_ accessors_ body-node subs_)
                                  (infer-expression-type body ret-ty subs env file)
                                (declare (ignore body-ty))
                                (setf subs subs_)
                                (setf preds (append preds preds_))
                                (setf accessors (append accessors accessors_))
                                body-node)))

             (branch-nodes
               (loop :for branch :in (parser:node-match-branches node)
                     :for pat-node :in pat-nodes
                     :for branch-body-node :in branch-body-nodes
                     :collect (make-node-match-branch
                               :pattern pat-node
                               :body branch-body-node
                               :source (parser:node-match-branch-source branch)))))

        (handler-case
            (progn
              (setf subs (tc:unify subs ret-ty expected-type))
              (let ((type (tc:apply-substitution subs ret-ty)))
                (values
                 type
                 preds
                 accessors
                 (make-node-match
                  :type (tc:qualify nil type)
                  :source (parser:node-source node)
                  :expr expr-node
                  :branches branch-nodes)
                 subs)))
          (error:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node file subs expr-node ret-ty))))))

  (:method ((node parser:node-progn) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-progn tc:substitution-list &optional))

    (multiple-value-bind (body-ty preds accessors body-node subs)
        (infer-expression-type (parser:node-progn-body node)
                               expected-type
                               subs
                               env
                               file)
      (values
       body-ty
       preds
       accessors
       (make-node-progn
        :type (tc:qualify nil body-ty)
        :source (parser:node-source node)
        :body body-node)
       subs)))

  (:method ((node parser:node-the) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node tc:substitution-list))

    (let ((declared-ty (parse-type (parser:node-the-type node) (tc-env-env env) file)))

      (multiple-value-bind (expr-ty preds accessors expr-node subs)
          (infer-expression-type (parser:node-the-expr node)
                                 (tc:make-variable)
                                 subs
                                 env
                                 file)

        ;; Ensure subs are applied
        (setf expr-ty (tc:apply-substitution subs expr-ty))

        ;; Check that declared-ty and expr-ty unify
        (handler-case
            (setf subs (tc:unify subs declared-ty expr-ty))
          (error:coalton-internal-type-error ()
            (error 'tc-error
                   :err (coalton-error
                         :span (parser:node-source node)
                         :file file
                         :message "Type mismatch"
                         :primary-note (format nil "Declared type '~S' does not match inferred type '~S'"
                                               (tc:apply-substitution subs declared-ty)
                                               (tc:apply-substitution subs expr-ty))))))

        ;; Check that declared-ty is not more specific than expr-ty
        (handler-case
            (tc:match expr-ty declared-ty)
          (error:coalton-internal-type-error ()
            (error 'tc-error
                   :err (coalton-error
                         :span (parser:node-source node)
                         :file file
                         :message "Declared type too general"
                         :primary-note (format nil "Declared type '~S' is more general than inferred type '~S'"
                                               (tc:apply-substitution subs declared-ty)
                                               (tc:apply-substitution subs expr-ty))))))

        ;; SAFETY: If declared-ty and expr-ty unify, and expr-ty is
        ;; more general than declared-ty then matching should be
        ;; infallible
        (setf subs (tc:compose-substitution-lists subs (tc:match expr-ty declared-ty)))

        (handler-case
            (progn
              (setf subs (tc:unify subs expr-ty expected-type))
              (values
               (tc:apply-substitution subs expr-ty)
               preds
               accessors
               expr-node
               subs))
          (error:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node file subs expected-type expr-ty))))))

  (:method ((node parser:node-return) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-return tc:substitution-list))

    ;; Returns must be inside a lambda
    (when (eq *return-status* :toplevel)
      (error 'tc-error
             :err (coalton-error
                   :span (parser:node-source node)
                   :file file
                   :message "Unexpected return"
                   :primary-note "returns must be inside a lambda")))

    ;; Returns cannot be in a do expression
    (when (eq *return-status* :do)
      (error 'tc-error
             :err (coalton-error
                   :span (parser:node-source node)
                   :file file
                   :message "Invalid return"
                   :primary-note "returns cannot be in a do expression")))

    (multiple-value-bind (ty preds accessors expr-node subs)
        (infer-expression-type (or (parser:node-return-expr node)
                                   ;; If the return looks like (return) then it returns unit
                                   (parser:make-node-variable
                                    :source (parser:node-source node)
                                    :name 'coalton:Unit))
                               (tc:make-variable)
                               subs
                               env
                               file)

      ;; Add node the the list of returns
      (push (cons (parser:node-source node) ty) *returns*)

      (values
       expected-type
       preds
       accessors
       (make-node-return
        :type (tc:qualify nil expected-type)
        :source (parser:node-source node)
        :expr expr-node)
       subs)))

  (:method ((node parser:node-or) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-or tc:substitution-list))

    (let* ((preds nil)
           (accessors nil)

           (body-nodes
             (loop :for node_ :in (parser:node-or-nodes node)
                   :collect (multiple-value-bind (node_ty_ preds_ accessors_ node_ subs_)
                                (infer-expression-type node_
                                                       tc:*boolean-type*
                                                       subs
                                                       env
                                                       file)
                              (declare (ignore node_ty_))
                              (setf subs subs_)
                              (setf preds (append preds preds_))
                              (setf accessors (append accessors accessors_))
                              node_))))

      (handler-case
          (progn
            (setf subs (tc:unify subs tc:*boolean-type* expected-type))
            (values
             tc:*boolean-type*
             preds
             accessors
             (make-node-or
              :type (tc:qualify nil tc:*boolean-type*)
              :source (parser:node-source node)
              :nodes body-nodes)
             subs))
        (error:coalton-internal-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:node-source node)
                       :file file
                       :message "Type mismatch"
                       :primary-note (format nil "Expected type '~S' but 'or' evaluates to '~S'"
                                             (tc:apply-substitution subs expected-type)
                                             tc:*boolean-type*)))))))

  (:method ((node parser:node-and) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-and tc:substitution-list))

    (let* ((preds nil)
           (accessors nil)

           (body-nodes
             (loop :for node_ :in (parser:node-and-nodes node)
                   :collect (multiple-value-bind (node_ty_ preds_ accessors_ node_ subs_)
                                (infer-expression-type node_
                                                       tc:*boolean-type*
                                                       subs
                                                       env
                                                       file)
                              (declare (ignore node_ty_))
                              (setf subs subs_)
                              (setf preds (append preds preds_))
                              (setf accessors (append accessors accessors_))
                              node_))))

      (handler-case
          (progn
            (setf subs (tc:unify subs tc:*boolean-type* expected-type))
            (values
             tc:*boolean-type*
             preds
             accessors
             (make-node-and
              :type (tc:qualify nil tc:*boolean-type*)
              :source (parser:node-source node)
              :nodes body-nodes)
             subs))
        (error:coalton-internal-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:node-source node)
                       :file file
                       :message "Type mismatch"
                       :primary-note (format nil "Expected type '~S' but 'and' evaluates to '~S'"
                                             (tc:apply-substitution subs expected-type)
                                             tc:*boolean-type*)))))))

  (:method ((node parser:node-if) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-if tc:substitution-list))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-if-expr node)
                               tc:*boolean-type* ; unify predicate against boolean
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (then-ty preds_ accessors_ then-node subs)
          (infer-expression-type (parser:node-if-then node)
                                 expected-type
                                 subs
                                 env
                                 file)
        (declare (ignore then-ty))
        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (multiple-value-bind (else-ty preds_ accessors_ else-node subs)
            (infer-expression-type (parser:node-if-else node)
                                   expected-type
                                   subs
                                   env
                                   file)
          (setf preds (append preds preds_))
          (setf accessors (append accessors accessors_))

          (handler-case
              (let ((type (tc:apply-substitution subs else-ty)))
                (values
                 type
                 preds
                 accessors
                 (make-node-if
                  :type (tc:qualify nil type)
                  :source (parser:node-source node)
                  :expr expr-node
                  :then then-node
                  :else else-node)
                 subs))
            (error:coalton-internal-type-error ()
              (standard-expression-type-mismatch-error node file subs expr-node else-ty)))))))

  (:method ((node parser:node-when) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-when tc:substitution-list))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-when-expr node)
                               tc:*boolean-type*
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
          (infer-expression-type (parser:node-when-body node)
                                 tc:*unit-type*
                                 subs
                                 env
                                 file)
        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (handler-case
            (progn
              (setf subs (tc:unify subs body-ty expected-type))
              (values
               tc:*unit-type*
               preds
               accessors
               (make-node-when
                :type (tc:qualify nil tc:*unit-type*)
                :source (parser:node-source node)
                :expr expr-node
                :body body-node)
               subs))
          (error:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node file subs expected-type body-ty))))))

  (:method ((node parser:node-unless) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-unless tc:substitution-list))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-unless-expr node)
                               tc:*boolean-type*
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
          (infer-expression-type (parser:node-unless-body node)
                                 tc:*unit-type*
                                 subs
                                 env
                                 file)
        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (handler-case
            (progn
              (setf subs (tc:unify subs body-ty expected-type))
              (values
               tc:*unit-type*
               preds
               accessors
               (make-node-unless
                :type (tc:qualify nil tc:*unit-type*)
                :source (parser:node-source node)
                :expr expr-node
                :body body-node)
               subs))
          (error:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node file subs expected-type body-ty))))))


  (:method ((node parser:node-while) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-while tc:substitution-list))
    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-while-expr node)
                               tc:*boolean-type*
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
          (infer-expression-type (parser:node-while-body node)
                                 (tc:make-variable)
                                 subs
                                 env
                                 file)
        (declare (ignore body-ty))

        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (handler-case
            (progn
              (setf subs (tc:unify subs tc:*unit-type* expected-type))              
              (values
               tc:*unit-type*
               preds
               accessors
               (make-node-while
                :type (tc:qualify nil tc:*unit-type*)
                :source (parser:node-source node)
                :label (parser:node-while-label node)
                :expr expr-node
                :body body-node)
               subs))
          (error:coalton-internal-type-error () 
            (standard-expression-type-mismatch-error node file subs expected-type tc:*unit-type*))))))

  (:method ((node parser:node-while-let) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-while-let tc:substitution-list))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-while-let-expr node)
                               (tc:make-variable) 
                               subs
                               env
                               file)

      (multiple-value-bind (pat-ty pat-node subs)
          (infer-pattern-type (parser:node-while-let-pattern node) expr-ty subs env file)
        (declare (ignore pat-ty))
        
        (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
            (infer-expression-type (parser:node-while-let-body node)
                                   (tc:make-variable)
                                   subs
                                   env
                                   file)
          (declare (ignore body-ty))
          (setf preds (append preds preds_))
          (setf accessors (append accessors accessors_))
          
          (handler-case
              (progn
                (setf subs (tc:unify subs tc:*unit-type* expected-type))
                (values
                 tc:*unit-type*
                 preds
                 accessors
                 (make-node-while-let
                  :type (tc:qualify nil tc:*unit-type*)
                  :source (parser:node-source node)
                  :label (parser:node-while-let-label node)
                  :pattern pat-node
                  :expr expr-node
                  :body body-node)
                 subs))
            (error:coalton-internal-type-error ()
             (standard-expression-type-mismatch-error node file subs expected-type tc:*unit-type*)))))))


  (:method ((node parser:node-for) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-for tc:substitution-list))

    (let* ((iterator-package
             (find-package "COALTON-LIBRARY/ITERATOR"))
           
           (intoiter-symbol
             (util:find-symbol "INTOITERATOR" iterator-package)))
      
      (multiple-value-bind (pat-ty pat-node subs)
          (infer-pattern-type (parser:node-for-pattern node) (tc:make-variable) subs env file)

        (multiple-value-bind (expr-ty preds accessors expr-node subs)
            (infer-expression-type (parser:node-for-expr node) (tc:make-variable) subs env file)

          (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
              (infer-expression-type (parser:node-for-body node) (tc:make-variable) subs env file)

            (declare (ignore body-ty))

            (setf preds     (append preds     preds_))
            (setf accessors (append accessors accessors_))

            (handler-case
                (progn
                  (setf subs (tc:unify subs tc:*unit-type* expected-type))
                  (values
                   tc:*unit-type*
                   (cons
                    (tc:make-ty-predicate
                     :class intoiter-symbol
                     :types (list expr-ty pat-ty)
                     :source (parser:node-source node))
                    preds)
                   accessors
                   (make-node-for
                    :type (tc:qualify nil tc:*unit-type*)
                    :source (parser:node-source node)
                    :label (parser:node-for-label node)
                    :pattern pat-node
                    :expr expr-node
                    :body body-node)
                   subs))
              (error:coalton-internal-type-error ()
                (standard-expression-type-mismatch-error node file subs expected-type tc:*unit-type*))))))))

  (:method ((node parser:node-loop) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-loop tc:substitution-list))
    (multiple-value-bind (body-ty preds accessors body-node subs)
        (infer-expression-type (parser:node-loop-body node)
                               (tc:make-variable)
                               subs
                               env
                               file)
      (declare (ignore body-ty))
      (handler-case
          (progn 
            (setf subs (tc:unify subs tc:*unit-type* expected-type))
            (values
             tc:*unit-type*
             preds
             accessors
             (make-node-loop
              :type (tc:qualify nil tc:*unit-type*)
              :source (parser:node-source node)
              :label (parser:node-loop-label node)
              :body body-node)
             subs))
        (error:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node file subs expected-type tc:*unit-type* )))))

    (:method ((node parser:node-break) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-break tc:substitution-list))
    (handler-case
        (progn
          (setf subs (tc:unify subs tc:*unit-type* expected-type)) 
          (values
           tc:*unit-type*
           nil
           nil
           (make-node-break
            :type (tc:qualify nil tc:*unit-type*)
            :source (parser:node-source node)
            :label (parser:node-break-label node))
           subs))
      (error:coalton-internal-type-error ()
        (standard-expression-type-mismatch-error node file subs expected-type tc:*unit-type*))))

  (:method ((node parser:node-continue) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-continue tc:substitution-list))
    (handler-case
        (progn
          (setf subs (tc:unify subs tc:*unit-type* expected-type))
          (values
           tc:*unit-type*
           nil
           nil
           (make-node-continue
            :type (tc:qualify nil tc:*unit-type*)
            :source (parser:node-source node)
            :label (parser:node-continue-label node))
           subs))
      (error:coalton-internal-type-error ()
        (standard-expression-type-mismatch-error node file subs expected-type tc:*unit-type*))))


  (:method ((node parser:node-cond-clause) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-cond-clause tc:substitution-list))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-cond-clause-expr node)
                               tc:*boolean-type*
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
          (infer-expression-type (parser:node-cond-clause-body node)
                                 expected-type ; unify against expected-type
                                 subs
                                 env
                                 file)
        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (let ((type (tc:apply-substitution subs body-ty)))
          (values
           type
           preds
           accessors
           (make-node-cond-clause
            :source (parser:node-cond-clause-source node)
            :expr expr-node
            :body body-node)
           subs)))))

  (:method ((node parser:node-cond) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-cond tc:substitution-list))

    (let* ((preds nil)
           (accessors nil)

           (ret-ty (tc:make-variable))

           (clause-nodes
             (loop :for clause :in (parser:node-cond-clauses node)
                   :collect (multiple-value-bind (clause-ty preds_ accessors_ clause-node subs_)
                                (infer-expression-type clause
                                                       ret-ty
                                                       subs
                                                       env
                                                       file)
                              (declare (ignore clause-ty))
                              (setf subs subs_)
                              (setf preds (append preds preds_))
                              (setf accessors (append accessors accessors_))
                              clause-node))))

      (handler-case
          (progn
            (setf subs (tc:unify subs ret-ty expected-type))
            (let ((type (tc:apply-substitution subs ret-ty)))
              (values
               type
               preds
               accessors
               (make-node-cond
                :type (tc:qualify nil type)
                :source (parser:node-source node)
                :clauses clause-nodes)
               subs)))
        (error:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node file subs expected-type ret-ty)))))

  (:method ((node parser:node-do-bind) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-do-bind tc:substitution-list))

    (let ((*return-status* :do))

      (multiple-value-bind (expr-ty preds accessors expr-node subs)
          (infer-expression-type (parser:node-do-bind-expr node)
                                 expected-type ; unify here so that expr-ty is in the form "m a"
                                 subs
                                 env
                                 file)

        (multiple-value-bind (ty_ pattern subs)
            (infer-pattern-type (parser:node-do-bind-pattern node)
                                (tc:tapp-to (tc:apply-substitution subs expr-ty)) ; this should never fail
                                subs
                                env
                                file)
          (declare (ignore ty_))

          (handler-case
              (progn
                (setf subs (tc:unify subs expr-ty expected-type))
                (values
                 expr-ty
                 preds
                 accessors
                 (make-node-do-bind
                  :pattern pattern
                  :expr expr-node
                  :source (parser:node-do-bind-source node))
                 subs))
            (error:coalton-internal-type-error ()
              (error 'tc-error
                     :err (coalton-error
                           :span (parser:node-do-bind-source node)
                           :file file
                           :message "Type mismatch"
                           :primary-note (format nil "Expected type '~S' but got '~S'"
                                                 (tc:apply-substitution subs expected-type)
                                                 (tc:apply-substitution subs expr-ty))))))))))

  (:method ((node parser:node-do) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list accessor-list node-do tc:substitution-list))

    (let* (;; m-type is the type of the monad and has kind "* -> *"
           (m-type (tc:make-variable (tc:make-kfun :from tc:+kstar+ :to tc:+kstar+)))

           (classes-package (find-package "COALTON-LIBRARY/CLASSES"))

           (monad-symbol (util:find-symbol "MONAD" classes-package))

           (preds nil)
           (accessors nil)

           (nodes
             (loop :for elem :in (parser:node-do-nodes node)
                   :collect (etypecase elem 
                              ;; Expressions are typechecked normally
                              ;; and then unified against "m a" where
                              ;; "a" is a fresh tyvar each time
                              (parser:node
                               (multiple-value-bind (ty_ preds_ accessors_ node_ subs_)
                                   (infer-expression-type elem
                                                          (tc:make-tapp
                                                           :from m-type
                                                           :to (tc:make-variable))
                                                          subs
                                                          env
                                                          file)
                                 (declare (ignore ty_))
                                 (setf preds (append preds preds_))
                                 (setf accessors (append accessors accessors_))
                                 (setf subs subs_)
                                 node_))

                              ;; Node-binds are typechecked normally
                              (parser:node-bind
                               (multiple-value-bind (ty_ preds_ accessors_ node_ subs_)
                                   (infer-expression-type elem
                                                          (tc:make-variable)
                                                          subs
                                                          env
                                                          file)
                                 (declare (ignore ty_))
                                 (setf preds (append preds preds_))
                                 (setf accessors (append accessors accessors_))
                                 (setf subs subs_)
                                 node_))

                              ;; Node-do-binds are typechecked and unified against "m a"
                              (parser:node-do-bind
                               (multiple-value-bind (ty_ preds_ accessors_ node_ subs_)
                                   (infer-expression-type elem
                                                          (tc:make-tapp
                                                           :from m-type
                                                           :to (tc:make-variable))
                                                          subs
                                                          env
                                                          file)
                                 (declare (ignore ty_))
                                 (setf preds (append preds preds_))
                                 (setf accessors (append accessors accessors_))
                                 (setf subs subs_)
                                 node_))))))

      

      (multiple-value-bind (ty preds_ accessors_ last-node subs)
          (infer-expression-type
           (parser:node-do-last-node node)
           (tc:make-tapp
            :from m-type
            :to (tc:make-variable))
           subs
           env
           file)

        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (handler-case
            (progn
              (setf subs (tc:unify subs ty expected-type))
              (values
               ty
               (cons
                (tc:make-ty-predicate
                 :class monad-symbol
                 :types (list m-type)
                 :source (parser:node-source node))
                preds)
               accessors
               (make-node-do
                :type (tc:qualify nil ty)
                :source (parser:node-source node)
                :nodes nodes
                :last-node last-node) 
               subs))
          (error:coalton-internal-type-error ()
            (error 'tc-error
                   :err (coalton-error
                         :span (parser:node-source node)
                         :file file
                         :message "Type mismatch"
                         :primary-note (format nil "Expected type '~S' but do expression has type '~S'"
                                               (tc:apply-substitution subs expected-type)
                                               (tc:apply-substitution subs ty))))))))))

;;;
;;; Pattern Type Inference
;;;

(defgeneric infer-pattern-type (pat expected-type subs env file)
  (:documentation "Infer the type of pattern PAT and then unify against EXPECTED-TYPE.

Returns (VALUES INFERRED-TYPE NODE SUBSTITUTIONS)")
  (:method ((pat parser:pattern-var) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty pattern-var tc:substitution-list))

    (let ((ty (tc-env-add-variable env (parser:pattern-var-name pat))))

      ;; SAFETY: unification against a variable will never fail
      (setf subs (tc:unify subs ty expected-type))

      (let ((type (tc:apply-substitution subs ty)))
        (values
         type
         (make-pattern-var
          :type (tc:qualify nil type)
          :source (parser:pattern-source pat)
          :name (parser:pattern-var-name pat)
          :orig-name (parser:pattern-var-orig-name pat))
         subs))))

  (:method ((pat parser:pattern-literal) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty pattern-literal tc:substitution-list))

    (let ((ty (etypecase (parser:pattern-literal-value pat)
                (integer tc:*integer-type*)
                (ratio tc:*fraction-type*)
                (single-float tc:*single-float-type*)
                (double-float tc:*double-float-type*)
                (string tc:*string-type*)
                (character tc:*char-type*))))

      (handler-case
          (progn
            (setf subs (tc:unify subs ty expected-type))
            (let ((type (tc:apply-substitution subs ty)))
              (values
               type
               (make-pattern-literal
                :type (tc:qualify nil type)
                :source (parser:pattern-source pat)
                :value (parser:pattern-literal-value pat))
               subs)))
        (error:coalton-internal-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:pattern-source pat)
                       :file file
                       :message "Type mismatch"
                       :primary-note (format nil "Expected type '~S' but pattern literal has type '~S'"
                                             (tc:apply-substitution subs expected-type)
                                             (tc:apply-substitution subs ty))))))))

  (:method ((pat parser:pattern-wildcard) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty pattern-wildcard tc:substitution-list))

    (values
     expected-type
     (make-pattern-wildcard
      :type (tc:qualify nil expected-type)
      :source (parser:pattern-source pat))
     subs))

  (:method ((pat parser:pattern-constructor) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty pattern-constructor tc:substitution-list))

    (let ((ctor (tc:lookup-constructor (tc-env-env env) (parser:pattern-constructor-name pat) :no-error t)))

      (check-duplicates
       (parser:pattern-variables pat)
       #'parser:pattern-var-name
       #'parser:pattern-source
       (lambda (first second)
         (error 'tc-error
                :err (coalton-error
                      :span (parser:pattern-source first) 
                      :file file
                      :message "Duplicate pattern variable"
                      :primary-note "first definition here"
                      :notes
                      (list
                       (make-coalton-error-note
                        :type :primary
                        :span (parser:pattern-source second) 
                        :message "second definition here"))))))

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

               (pat-ty (tc:function-return-type ctor-ty))

               (pattern-nodes
                 (loop :for arg :in (parser:pattern-constructor-patterns pat)
                       :for arg-ty :in (tc:function-type-arguments ctor-ty)
                       :collect (multiple-value-bind (ty_ node_ subs_)
                                    (infer-pattern-type arg arg-ty subs env file)
                                  (declare (ignore ty_))
                                  (setf subs subs_)
                                  node_))))

          (handler-case
              (progn
                (setf subs (tc:unify subs pat-ty expected-type))
                (let ((type (tc:apply-substitution subs pat-ty)))
                  (values
                   type
                   (make-pattern-constructor
                    :type (tc:qualify nil pat-ty)
                    :source (parser:pattern-source pat)
                    :name (parser:pattern-constructor-name pat)
                    :patterns pattern-nodes)
                   subs)))
            (error:coalton-internal-type-error ()
              (error 'tc-error
                     :err (coalton-error
                           :span (parser:pattern-source pat)
                           :file file
                           :message "Type mismatch"
                           :primary-note (format nil "Expected type '~S' but pattern has type '~S'"
                                                 (tc:apply-substitution subs expected-type)
                                                 (tc:apply-substitution subs pat-ty)))))))))))

;;;
;;; Binding Group Type Inference
;;;

(defun infer-let-bindings (bindings declares subs env file)
  (declare (type parser:node-let-binding-list bindings)
           (type parser:node-let-declare-list declares)
           (type tc:substitution-list subs)
           (type tc-env env)
           (type coalton-file file)
           (values tc:ty-predicate-list accessor-list (or toplevel-define-list node-let-binding-list) tc:substitution-list &optional))

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


    ;; Ensure that there are no duplicate declarations
    (loop :for declare :in declares
          :for name := (parser:node-variable-name (parser:node-let-declare-name declare))

          :if (gethash name dec-table)
            :do (error 'tc-error
                       :err (coalton-error
                             :span (parser:node-source (parser:node-let-declare-name declare))
                             :file file
                             :message "Duplicate declaration in let"
                             :primary-note "second declaration here"
                             :notes
                             (list
                              (make-coalton-error-note
                               :type :primary
                               :span (parser:node-source
                                      (parser:node-let-declare-name
                                       (gethash name dec-table)))
                               :message "first declaration here"))))
          :else
            :do (setf (gethash name dec-table) declare))

    ;; Ensure that each declaration has an associated definition
    (loop :for declare :in declares
          :for name := (parser:node-variable-name (parser:node-let-declare-name declare))

          :unless (gethash name def-table)
            :do (error 'tc-error
                       :err (coalton-error
                             :span (parser:node-source (parser:node-let-declare-name declare))
                             :file file
                             :message "Orphan declare in let"
                             :primary-note "declaration does not have an associated definition")))

    (let ((dec-table
            (loop :with table := (make-hash-table :test #'eq)
                  :for declare :in declares
                  :for name := (parser:node-variable-name (parser:node-let-declare-name declare))
                  :do (setf (gethash name table) (parser:node-let-declare-type declare))
                  :finally (return table))))

      (infer-bindings-type bindings dec-table subs env file))))


(defun infer-bindings-type (bindings dec-table subs env file)
  (declare (type list bindings)
           (type hash-table dec-table)
           (type tc:substitution-list subs)
           (type tc-env env)
           (type coalton-file file)
           (values tc:ty-predicate-list accessor-list (or toplevel-define-list node-let-binding-list) tc:substitution-list))
  ;;
  ;; Binding type inference has several steps.
  ;; 1. Explicit types are parsed and added to the environment
  ;; 2. Implicit bindings are grouped by scc and then each scc is type checked
  ;; 3. Explicitly typed bindings are typechecked and compared against their declared types.
  ;;

  ;; Define explicit types to the environment
  (loop :for name :being :the :hash-keys :of dec-table
        :for unparsed-ty :being :the :hash-values :of dec-table

        :for scheme := (parse-ty-scheme unparsed-ty (tc-env-env env) file)
        :do (tc-env-add-definition env name scheme))

  ;; Split apart explicit and implicit bindings
  (let* ((expl-bindings (loop :for binding :in bindings
                              :for name := (parser:node-variable-name (parser:binding-name binding))

                              :when (gethash name dec-table)
                                :collect binding))

         (impl-bindings (loop :with table := (make-hash-table :test #'eq)
                              :for binding :in bindings
                              :for name := (parser:node-variable-name (parser:binding-name binding))

                              :unless (gethash name dec-table)
                                :do (setf (gethash name table) binding)

                              :finally (return table)))

         (impl-bindings-names (alexandria:hash-table-keys impl-bindings))

         (impl-bindings-deps (loop :for name :in impl-bindings-names
                                   :for binding := (gethash name impl-bindings)
                                   :for node := (parser:binding-value binding)

                                   :for deps := (remove-duplicates
                                                 (intersection
                                                  (mapcar #'parser:node-variable-name
                                                          (parser:collect-variables node))
                                                  impl-bindings-names
                                                  :test #'eq)
                                                 :test #'eq)
                                   :collect (cons name deps)))

         (sccs (algo:tarjan-scc impl-bindings-deps))

         (preds nil)

         (impl-binding-nodes
           ;; Infer the types of implicit bindings on scc at a time
           (loop :for scc :in (reverse sccs)
                 :for bindings
                   := (loop :for name :in scc
                            :collect (gethash name impl-bindings))
                 :append (multiple-value-bind (preds_ nodes subs_)
                             (infer-impls-binding-type bindings subs env file)
                           (setf subs subs_)
                           (setf preds (append preds preds_))
                           nodes)))

         ;; Infer the type of each explicit bindings and check against the
         ;; declared type
         (expl-binding-nodes
           (loop :for binding :in expl-bindings

                 :for name := (parser:node-variable-name (parser:binding-name binding))
                 :for scheme := (gethash name (tc-env-ty-table env))

                 :collect (multiple-value-bind (preds_ node_ subs_)
                              (infer-expl-binding-type
                               binding
                               scheme
                               (parser:node-source (parser:binding-name binding))
                               subs
                               env
                               file)
                            (setf subs subs_)
                            (setf preds (append preds preds_))
                            node_))))

    (values
     preds
     nil
     (append impl-binding-nodes expl-binding-nodes)
     subs)))

(defun infer-expl-binding-type (binding declared-ty source subs env file)
  "Infer the type of BINDING and then ensure it matches DECLARED-TY."
  (declare (type (or parser:toplevel-define parser:node-let-binding parser:instance-method-definition) binding)
           (type tc:ty-scheme declared-ty)
           (type cons source)
           (type tc:substitution-list subs)
           (type tc-env env)
           (type coalton-file file)
           (values tc:ty-predicate-list (or toplevel-define node-let-binding instance-method-definition) tc:substitution-list &optional))
  
  ;; HACK: recursive scc checking on instances is too strict
  (unless (typep binding 'parser:instance-method-definition)
    (check-for-invalid-recursive-scc (list binding) (tc-env-env env) file))

  (let* ((name (parser:node-variable-name (parser:binding-name binding)))

         (bound-variables (remove name (tc-env-bound-variables env) :test #'eq))

         (fresh-qual-type (tc:fresh-inst declared-ty))
         (fresh-type (tc:qualified-ty-type fresh-qual-type))
         (fresh-preds (tc:qualified-ty-predicates fresh-qual-type)))

    (multiple-value-bind (preds accessors binding-node subs)
        (infer-binding-type
         binding
         fresh-type                     ; unify against declared type
         subs
         env
         file)

      (tc:apply-substitution subs env)

      (setf accessors (tc:apply-substitution subs accessors))
      
      (multiple-value-bind (accessors subs_)
          (solve-accessors accessors file (tc-env-env env))
        (setf subs (tc:compose-substitution-lists subs subs_))

        (when accessors
          (error 'tc-error
                 :err (coalton-error
                       :span (accessor-source (first accessors))
                       :file file
                       :message "Ambiguous accessor"
                       :primary-note "accessor is ambiguous")))

        (let* ((expr-type (tc:apply-substitution subs fresh-type))
               (expr-preds (tc:apply-substitution subs fresh-preds))

               (env-tvars (tc-env-bindings-variables env bound-variables))
               (local-tvars (set-difference (remove-duplicates
                                             (append (tc:type-variables expr-type)
                                                     (tc:type-variables expr-preds))
                                             :test #'eq)
                                            env-tvars
                                            :test #'eq))

               (output-qual-type (tc:qualify expr-preds expr-type))
               (output-scheme (tc:quantify local-tvars output-qual-type)))

          ;; Generate additional substitutions from fundeps
          (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) preds subs)))

          (let* ((expr-preds (tc:apply-substitution subs expr-preds))

                 (known-variables
                   (tc:apply-substitution
                    subs
                    (append (remove-duplicates (tc:type-variables expr-type) :test #'eq) env-tvars)))

                 (preds (tc:apply-substitution subs preds))

                 (subs (tc:compose-substitution-lists
                        subs
                        (tc:fundep-entail (tc-env-env env) expr-preds preds known-variables)))

                 (expr-preds (tc:apply-substitution subs expr-preds))

                 (preds (remove-if-not (lambda (p)
                                         (not (tc:entail (tc-env-env env) expr-preds p)))
                                       (tc:apply-substitution subs preds))))

            (setf preds (tc:apply-substitution subs preds))
            (setf local-tvars (expand-local-tvars env-tvars local-tvars preds (tc-env-env env)))
            (setf env-tvars
                  (expand-local-tvars local-tvars (tc:apply-substitution subs env-tvars) preds (tc-env-env env)))

            ;; Split predicates into retained and deferred
            (multiple-value-bind (deferred-preds retained-preds)
                (tc:split-context (tc-env-env env) env-tvars preds subs)

              (let* (;; Calculate defaultable predicates
                     (defaultable-preds
                       (handler-case
                           (tc:default-preds
                            (tc-env-env env)
                            (append env-tvars local-tvars)
                            retained-preds)

                         (tc:ambiguous-constraint (e)
                           (error-ambiguous-pred (tc:ambiguous-constraint-pred e) file))))

                     ;; Defaultable predicates are not retained
                     (retained-preds
                       (set-difference retained-preds defaultable-preds :test #'tc:type-predicate=)))

                ;; Apply defaulting to defaultable predicates
                (setf subs (tc:compose-substitution-lists
                            (tc:default-subs (tc-env-env env) nil defaultable-preds)
                            subs))

                ;; If the bindings is toplevel then attempt to default deferred-predicates
                (when (parser:binding-toplevel-p binding)
                  (setf subs (tc:compose-substitution-lists
                              (tc:default-subs (tc-env-env env) nil deferred-preds)
                              subs))
                  (setf deferred-preds (tc:reduce-context (tc-env-env env) deferred-preds subs)))

                ;; Toplevel bindings cannot defer predicates
                (when (and (parser:binding-toplevel-p binding) deferred-preds)
                  (error-unknown-pred (first deferred-preds) file))

                ;; Check that the declared and inferred schemes match
                (unless (equalp declared-ty output-scheme)
                  (error 'tc-error
                         :err (coalton-error
                               :message "Declared type is too general"
                               :span source
                               :file file
                               :primary-note (format nil "Declared type ~S is more general than inferred type ~S."
                                                     declared-ty
                                                     output-scheme))))

                ;; Check for undeclared predicates
                (when (not (null retained-preds))
                  (error 'tc-error
                         :err (coalton-error
                               :message "Explicit type is missing inferred predicate"
                               :span source
                               :file file
                               :primary-note (format nil "Declared type ~S is missing inferred predicate ~S"
                                                     output-qual-type
                                                     (first retained-preds)))))

                (values
                 deferred-preds
                 (attach-explicit-binding-type (tc:apply-substitution subs binding-node) (tc:apply-substitution subs fresh-qual-type))
                 subs)))))))))

(defun check-for-invalid-recursive-scc (bindings env file)
  (declare (type (or parser:toplevel-define-list parser:node-let-binding-list parser:instance-method-definition-list) bindings)
           (type tc:environment env)
           (type coalton-file file))

  (assert bindings)

  ;; If all bindings are functions then the scc is valid
  (when (every #'parser:binding-function-p bindings)
    (return-from check-for-invalid-recursive-scc))

  ;; If some bindings are functions and some are not then the scc is invalid
  (when (and (some (alexandria:compose #'not #'parser:binding-function-p) bindings)
             (some #'parser:binding-function-p bindings))

    (let ((first-fn (find-if #'parser:binding-function-p bindings)))
      (assert first-fn)

      (error 'tc-error
             :err (coalton-error
                   :span (parser:node-source (parser:binding-name first-fn))
                   :file file
                   :message "Invalid recursive bindings"
                   :primary-note "function can not be defined recursively with variables"
                   :notes (loop :for binding :in (remove first-fn bindings :test #'eq)
                                :collect (make-coalton-error-note
                                          :type :secondary
                                          :span (parser:node-source (parser:binding-name binding))
                                          :message "with definition"))))))

  ;; If there is a single non-recursive binding then it is valid
  (when (and (= 1 (length bindings))
             (not (member (parser:node-variable-name (parser:binding-name (first bindings)))
                          (parser:collect-variables (parser:binding-value (first bindings)))
                          :key #'parser:node-variable-name
                          :test #'eq)))
    (return-from check-for-invalid-recursive-scc))

  ;; Toplevel bindings cannot be recursive values
  (when (parser:binding-toplevel-p (first bindings))
    (error 'tc-error
           :err (coalton-error
                 :span (parser:node-source (parser:binding-name (first bindings)))
                 :file file
                 :message "Invalid recursive bindings"
                 :primary-note "invalid recursive variable bindings"
                 :notes (loop :for binding :in (rest bindings)
                              :collect (make-coalton-error-note
                                        :type :secondary
                                        :span (parser:node-source (parser:binding-name binding))
                                        :message "with definition")))))

  (let ((binding-names (mapcar (alexandria:compose #'parser:node-variable-name
                                                   #'parser:binding-name)
                               bindings)))

    (labels ((valid-recursive-constructor-call-p (node)
               "Returns t if NODE is a valid constructor call in a recursive value binding group"
               (typecase node
                 (parser:node-the
                  (valid-recursive-constructor-call-p (parser:node-the-expr node)))
                 (parser:node-application
                  (when (typep (parser:node-application-rator node) 'parser:node-variable)

                    (let* ((function-name (parser:node-variable-name (parser:node-application-rator node)))

                           (ctor (tc:lookup-constructor env function-name :no-error t)))

                      (when ctor
                        ;; The constructor must be fully applied
                        (unless (= (length (parser:node-application-rands node)) (tc:constructor-entry-arity ctor))
                          (return-from valid-recursive-constructor-call-p nil))

                        (let ((type (tc:lookup-type env (tc:constructor-entry-constructs ctor))))

                          ;; Recursive constructors are valid on types
                          ;; without reprs, types with repr lisp and
                          ;; the type "List"
                          (when (or (null (tc:type-entry-explicit-repr type))
                                    (eq :lisp (tc:type-entry-explicit-repr type))
                                    (eq 'coalton:List (tc:type-entry-name type)))
                            (return-from valid-recursive-constructor-call-p
                              (reduce
                               (lambda (a b) (and a b))
                               (parser:node-application-rands node)
                               :key #'valid-recursive-value-p
                               :initial-value t))))))))))

             (valid-recursive-value-p (node)
               "Returns t if NODE is a valid subcomponent in a recursive value binding group"
               ;; Variables are valid nodes
               (when (typep node 'parser:node-variable)
                 (return-from valid-recursive-value-p t))

               (when (valid-recursive-constructor-call-p node)
                 (return-from valid-recursive-value-p t))

               ;; Nodes are valid if they do not reference variables in the current binding group
               (not
                (intersection
                 binding-names
                 (mapcar #'parser:node-variable-name
                         (parser:collect-variables node))
                 :test #'eq))))

      (when (every (alexandria:compose #'valid-recursive-constructor-call-p #'parser:binding-value) bindings)
        (return-from check-for-invalid-recursive-scc))

      (error 'tc-error
             :err (coalton-error
                   :span (parser:node-source (parser:binding-name (first bindings)))
                   :file file
                   :message "Invalid recursive bindings"
                   :primary-note "invalid recursive variable bindings"
                   :notes (loop :for binding :in (rest bindings)
                                :collect (make-coalton-error-note
                                          :type :secondary
                                          :span (parser:node-source (parser:binding-name binding))
                                          :message "with definition")))))))

(defun infer-impls-binding-type (bindings subs env file)
  "Infer the type's of BINDINGS and then qualify those types into schemes."
  (declare (type (or parser:toplevel-define-list parser:node-let-binding-list) bindings)
           (type tc:substitution-list subs)
           (type tc-env env)
           (type coalton-file file)
           (values tc:ty-predicate-list (or toplevel-define-list node-let-binding-list) tc:substitution-list &optional))

  (check-for-invalid-recursive-scc bindings (tc-env-env env) file)

  (let* (;; track variables bound before typechecking
         (bound-variables (tc-env-bound-variables env))

         ;; Add all bindings to the environment
         (expr-tys
           (loop :for binding :in bindings
                 :for name := (parser:node-variable-name (parser:binding-name  binding))
                 :collect (tc-env-add-variable env name)))

         (preds nil)

         (accessors nil)

         ;; Derive the type of each binding
         (binding-nodes
           (loop :for binding :in bindings
                 :for ty :in expr-tys
                 :for node := (parser:binding-value binding)

                 :collect (multiple-value-bind (preds_ accessors_ node_ subs_)
                              (infer-binding-type binding ty subs env file)
                            (setf subs subs_)
                            (setf preds (append preds preds_))
                            (setf accessors (append accessors accessors_))
                            node_))))

    (tc:apply-substitution subs env)

    (setf accessors (tc:apply-substitution subs accessors))

    (multiple-value-bind (accessors subs_)
        (solve-accessors accessors file (tc-env-env env))
      (setf subs (tc:compose-substitution-lists subs subs_))

      (when accessors
        (error 'tc-error
               :err (coalton-error
                     :span (accessor-source (first accessors))
                     :file file
                     :message "Ambiguous accessor"
                     :primary-note "accessor is ambiguous")))

      (let* ((expr-tys (tc:apply-substitution subs expr-tys))

             (env-tvars (tc-env-bindings-variables env bound-variables))

             (expr-tvars (remove-duplicates (tc:type-variables expr-tys) :test #'eq))

             (local-tvars (set-difference expr-tvars env-tvars :test #'eq)))

        ;; Generate additional substitutions from fundeps
        (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) preds subs)))

        (setf preds (tc:apply-substitution subs preds))
        (setf local-tvars (expand-local-tvars env-tvars local-tvars preds (tc-env-env env)))
        (setf env-tvars (expand-local-tvars local-tvars (tc:apply-substitution subs env-tvars) preds (tc-env-env env)))

        (multiple-value-bind (deferred-preds retained-preds)
            (tc:split-context (tc-env-env env) env-tvars preds subs)

          (let* ((defaultable-preds (handler-case
                                        (tc:default-preds (tc-env-env env) (append env-tvars local-tvars) retained-preds)
                                      (error:coalton-internal-type-error (e)
                                        (error-ambiguous-pred (tc:ambiguous-constraint-pred e) file))))

                 (retained-preds (set-difference retained-preds defaultable-preds :test #'eq))

                 ;; Check if the monomorphism restriction applies
                 (restricted (some (lambda (b)
                                     (not (parser:binding-function-p b)))
                                   bindings)))


            (setf subs (tc:compose-substitution-lists
                        (tc:default-subs (tc-env-env env) nil defaultable-preds)
                        subs))

            (when (parser:binding-toplevel-p (first bindings))
              (if restricted
                  ;; Restricted bindings have all predicates defaulted
                  (setf subs (tc:compose-substitution-lists
                              (tc:default-subs (tc-env-env env) nil (append deferred-preds retained-preds))
                              subs))
                  ;; Unrestricted bindings have deferred predicates defaulted
                  (setf subs (tc:compose-substitution-lists
                              (tc:default-subs (tc-env-env env) nil deferred-preds)
                              subs)))

              (setf deferred-preds (tc:reduce-context (tc-env-env env) deferred-preds subs))
              (setf retained-preds (tc:reduce-context (tc-env-env env) retained-preds subs))
              (setf expr-tys (tc:apply-substitution subs expr-tys)))

            (if restricted
                (let* ((allowed-tvars (set-difference local-tvars (tc:type-variables retained-preds) :test #'eq))

                       (output-qual-tys
                         (loop :for ty :in expr-tys
                               :collect (tc:apply-substitution subs (tc:make-qualified-ty :predicates nil :type ty))))

                       (output-schemes
                         (loop :for ty :in output-qual-tys
                               :collect (tc:quantify allowed-tvars ty)))

                       (deferred-preds (append deferred-preds retained-preds)))

                  (loop :for scheme :in output-schemes
                        :for binding :in bindings

                        :for name := (parser:node-variable-name (parser:binding-name binding))

                        :do (tc-env-replace-type env name scheme))

                  (when (and (parser:binding-toplevel-p (first bindings)) deferred-preds)
                    (error-unknown-pred (first deferred-preds) file))

                  (values
                   deferred-preds
                   (loop :for binding :in binding-nodes
                         :for ty :in output-qual-tys
                         :collect (tc:apply-substitution subs (attach-explicit-binding-type binding ty)))
                   subs))

                (let* ((output-qual-tys
                         (loop :for ty :in expr-tys
                               :collect (tc:make-qualified-ty :predicates retained-preds :type ty)))

                       (output-schemes
                         (loop :for ty :in output-qual-tys
                               :collect (tc:quantify local-tvars ty)))

                       (rewrite-table
                         (loop :with table := (make-hash-table :test #'eq)

                               :for ty :in output-qual-tys
                               :for binding :in bindings

                               :for name := (parser:node-variable-name (parser:binding-name binding))
                               :do (setf (gethash name table) ty)

                               :finally (return table))))

                  (loop :for scheme :in output-schemes
                        :for binding :in bindings

                        :for name := (parser:node-variable-name (parser:binding-name binding))

                        :do (tc-env-replace-type env name scheme))

                  (when (and (parser:binding-toplevel-p (first bindings)) deferred-preds)
                    (error-ambiguous-pred (first deferred-preds) file))

                  (values
                   deferred-preds
                   (loop :for binding :in binding-nodes
                         :for ty :in output-qual-tys
                         :collect (rewrite-recursive-calls
                                   (tc:apply-substitution subs (attach-explicit-binding-type binding ty))
                                   rewrite-table))
                   subs)))))))))

(defun infer-binding-type (binding expected-type subs env file)
  "Infer the type of BINDING then unify against EXPECTED-TYPE. Adds BINDING's parameters to the environment."
  (declare (type (or parser:toplevel-define parser:node-let-binding parser:instance-method-definition) binding)
           (type tc:ty expected-type)
           (type tc:substitution-list subs)
           (type coalton-file file)
           (values tc:ty-predicate-list accessor-list (or toplevel-define node-let-binding instance-method-definition) tc:substitution-list))

  (check-duplicates
   (parser:pattern-variables (parser:binding-parameters binding))
   #'parser:pattern-var-name
   #'parser:pattern-source
   (lambda (first second)
     (error 'tc-error
            :err (coalton-error
                  :span (parser:node-source first)
                  :file file
                  :message "Duplicate parameters name"
                  :primary-note "first parameter here"
                  :notes
                  (list
                   (make-coalton-error-note
                    :type :primary
                    :span (parser:node-source second)
                    :message "second parameter here"))))))

  (let* ((param-tys (loop :with args := (tc:function-type-arguments expected-type)
                          :for pattern :in (parser:binding-parameters binding)

                          :if args
                            :collect (car args)
                            :and :do (setf args (cdr args))
                          :else
                            :collect (tc:make-variable)))

         (params (loop :for pattern :in (parser:binding-parameters binding)
                       :for ty :in param-tys
                       :collect (multiple-value-bind (ty_ pattern subs_)
                                    (infer-pattern-type pattern ty subs env file)
                                  (declare (ignore ty_))
                                  (setf subs subs_)
                                  pattern)))

         (ret-ty (tc:make-variable))

         (preds nil)
         (accessors nil)

         (value-node
           (if params
               ;; If the binding has parameters that setup the return state before inferring the binding's type
               (let ((*return-status* :lambda)

                     (*returns* nil))

                 (multiple-value-bind (ty_ preds_ accessors_ value-node subs_)
                     (infer-expression-type (parser:binding-value binding)
                                            ret-ty
                                            subs
                                            env
                                            file)
                   (declare (ignore ty_))
                   (setf subs subs_)
                   (setf preds preds_)
                   (setf accessors accessors_)

                   ;; Ensure that all early returns unify
                   (loop :with returns := (reverse *returns*)
                         :for (s1 . ty1) :in returns
                         :for (s2 . ty2) :in (cdr returns)
                         :do (handler-case
                                 (setf subs (tc:unify subs ty1 ty2))
                               (error:coalton-internal-type-error ()
                                 (error 'tc-error
                                        :err (coalton-error
                                              :span s1
                                              :file file
                                              :message "Return type mismatch"
                                              :primary-note (format nil "First return is of type '~S'"
                                                                    (tc:apply-substitution subs ty1))
                                              :notes
                                              (list
                                               (make-coalton-error-note
                                                :type :primary
                                                :span s2
                                                :message (format nil "Second return is of type '~S'"
                                                                 (tc:apply-substitution subs ty2)))))))))

                   ;; Unify the function's inferred type with one of the early returns.
                   (when *returns*
                     (handler-case
                         (setf subs (tc:unify subs (cdr (first *returns*)) ret-ty))
                       (error:coalton-internal-type-error ()
                         (error 'tc-error
                                :err (coalton-error
                                      :span (car (first *returns*))
                                      :file file
                                      :message "Return type mismatch"
                                      :primary-note (format nil "First return is of type '~S'"
                                                            (tc:apply-substitution subs (cdr (first *returns*))))
                                      :notes
                                      (list
                                       (make-coalton-error-note
                                        :type :primary
                                        :span (parser:node-source (parser:binding-last-node binding))
                                        :message (format nil "Second return is of type '~S'"
                                                         (tc:apply-substitution subs ret-ty)))))))))

                   value-node))

               ;; If the binding does not have parameters that just infer the binding's type
               (multiple-value-bind (ty_ preds_ accessors_ value-node subs_)
                   (infer-expression-type (parser:binding-value binding)
                                          ret-ty
                                          subs
                                          env
                                          file)
                 (declare (ignore ty_))
                 (setf subs subs_)
                 (setf preds preds_)
                 (setf accessors accessors_)
                 value-node))))

    (let ((ty (tc:make-function-type* param-tys ret-ty)))
      (handler-case
          (progn
            (setf subs (tc:unify subs ty expected-type))

            (let* ((type (tc:apply-substitution subs ty))

                   (name-node
                     (make-node-variable
                      :type (tc:qualify nil type)
                      :source (parser:node-source (parser:binding-name binding))
                      :name (parser:node-variable-name (parser:binding-name binding))))

                   (typed-binding (build-typed-binding binding name-node value-node params)))
              (values
               preds
               accessors
               typed-binding
               subs)))
        (error:coalton-internal-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:binding-source binding)
                       :file file
                       :message "Type mismatch"
                       :primary-note (format nil "Expected type '~S' but got type '~S'"
                                             (tc:apply-substitution subs expected-type)
                                             (tc:apply-substitution subs ty)))))))))

;;;
;;; Helpers
;;;

(defgeneric build-typed-binding (binding name value params)
  (:method ((binding parser:toplevel-define) name value params)
    (declare (type node-variable name)
             (type node-body value)
             (type pattern-list params)
             (values toplevel-define))

    (make-toplevel-define
     :name name
     :params params
     :body value
     :source (parser:toplevel-define-source binding)))

  (:method ((binding parser:node-let-binding) name value params)
    (declare (type node-variable name)
             (type node value)
             (type pattern-list params)
             (values node-let-binding))

    (assert (null params))

    (make-node-let-binding
     :name name
     :value value
     :source (parser:node-let-binding-source binding)))

  (:method ((binding parser:instance-method-definition) name value params)
    (declare (type node-variable name)
             (type node-body value)
             (type pattern-list params)
             (values instance-method-definition))

    (make-instance-method-definition
     :name name
     :params params
     :body value
     :source (parser:instance-method-definition-source binding))))

(defgeneric attach-explicit-binding-type (binding explicit-type)
  (:method ((binding toplevel-define) explicit-type)
    (declare (type tc:qualified-ty explicit-type)
             (values toplevel-define))

   (make-toplevel-define
    :name (make-node-variable
           :name (node-variable-name (toplevel-define-name binding))
           :type explicit-type
           :source (node-source (toplevel-define-name binding)))
    :params (toplevel-define-params binding)
    :body (toplevel-define-body binding)
    :source (toplevel-define-source binding)))

  (:method ((binding node-let-binding) explicit-type)
    (declare (type tc:qualified-ty explicit-type)
             (values node-let-binding))

   (make-node-let-binding
    :name (make-node-variable
           :name (node-variable-name (node-let-binding-name binding))
           :type explicit-type
           :source (node-source (node-let-binding-name binding)))
    :value (node-let-binding-value binding)
    :source (node-let-binding-source binding)))

  (:method ((binding instance-method-definition) explicit-type)
    (declare (type tc:qualified-ty explicit-type)
             (values instance-method-definition))

    (make-instance-method-definition
     :name (make-node-variable
            :name (node-variable-name (instance-method-definition-name binding))
            :type explicit-type
            :source (node-source (instance-method-definition-name binding)))
     :params (instance-method-definition-params binding)
     :body (instance-method-definition-body binding)
     :source (instance-method-definition-source binding))))

;;; When inferring the types of bindings in a recursive binding group,
;;; references to those bindings will not yet have predicates. If
;;; these incorrectly typed references remain, the compiler will
;;; produce invalid code. `rewrite-recursive-calls' rewrites the type
;;; of recursive references once the predicates on each binding are
;;; known.

(defun rewrite-recursive-calls (binding table)
  (declare (type hash-table table))

  (labels ((rewrite-variable-ref (node)
             (declare (type node-variable node)
                      (values node-variable))

             (if (gethash (node-variable-name node) table)

                 (make-node-variable
                  :type (gethash (node-variable-name node) table)
                  :source (node-source node)
                  :name (node-variable-name node))

                 node)))

    (etypecase binding
      (toplevel-define
       (make-toplevel-define
        :name (toplevel-define-name binding)
        :params (toplevel-define-params binding)
        :body (traverse
               (toplevel-define-body binding)
               (make-traverse-block
                :variable #'rewrite-variable-ref))
        :source (toplevel-define-source binding)))

      (node-let-binding
       (make-node-let-binding
        :name (node-let-binding-name binding)
        :value (traverse
                (node-let-binding-value binding)
                (make-traverse-block
                 :variable #'rewrite-variable-ref))
        :source (node-let-binding-source binding))))))

;;; When type checking bindings, Coalton computes the set of type
;;; variables that can be qualified over. Predicates that contain type
;;; variables not in this set are rejected as ambiguous.
;;;
;;;   An example rejected definition:
;;;   Into :a :b => :a -> Integer
;;;
;;; Without fundeps this set of type variables is simply the type
;;; variables in the inferred type, minus the type variables in the
;;; environment. With fundeps, type variables can appear in the
;;; predicates but not the type without being ambiguous.
;;;
;;;   A valid definition for "C :a :b (:a -> :b)"
;;;   C :a :b => :a -> Integer
;;;
;;; `expand-local-tvars' uses fundeps to compute an extended set of
;;; unambiguous type variables given a previous such list and the
;;; binding's predicates.

(defun expand-local-tvars (env-tvars local-tvars preds env)
  (let ((old-local-tvars nil))
    (loop :until (null (set-exclusive-or old-local-tvars local-tvars)) :do
      (setf old-local-tvars local-tvars)
      (loop :for pred :in preds
            :for class := (tc:lookup-class env (tc:ty-predicate-class pred))
            :when (tc:ty-class-fundeps class) :do
              (let* ((idx (loop :for i :from 0
                                :for ty :in (tc:ty-predicate-types pred)
                                :when (subsetp (tc:type-variables ty) local-tvars :test #'eq)
                                  :collect i))

                     (fundep-vars (util:project-indicies idx (tc:ty-class-class-variables class)))
                     (closure-vars (tc:closure fundep-vars (tc:ty-class-fundeps class)))

                     (new-vars (set-difference closure-vars fundep-vars :test #'eq))

                     (new-tys (util:project-map
                               new-vars
                               (tc:ty-class-class-variable-map class)
                               (tc:ty-predicate-types pred))))

                (loop :for var :in (set-difference
                                    (remove-duplicates
                                     (tc:type-variables new-tys)
                                     :test #'eq)
                                    env-tvars)
                      :do (push var local-tvars)))))

    (remove-duplicates local-tvars :test #'eq)))
