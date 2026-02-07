;;;
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
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker/stage-1)
   (#:types #:coalton-impl/typechecker/types)
   (#:redef #:coalton-impl/redef-detection)
   (#:settings #:coalton-impl/settings))
  (:export
   #:infer-expression-type              ; FUNCTION
   #:infer-expl-binging-type            ; FUNCTION
   #:attach-explicit-binding-type       ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/define)

(declaim (type (member :toplevel :lambda :do) *return-status*))
(defparameter *return-status* :toplevel)

(deftype node-return-info ()
  '(cons source:location tc:ty))

(defun node-return-info-p (x)
  (typep x 'node-return-info))

(defun node-return-info-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-return-info-p x)))

(deftype node-return-info-list ()
  '(satisfies node-return-info-list-p))

(declaim (type node-return-info-list *returns*))
(defparameter *returns* nil)

(defun error-ambiguous-pred (pred)
  (declare (type tc:ty-predicate pred))

  (unless (source:location pred)
    (util:coalton-bug "Predicate ~S does not have source information" pred))

  (tc-error "Ambiguous predicate" (tc-note pred "Ambiguous predicate ~S" pred)))

(defun error-unknown-pred (pred)
  (declare (type tc:ty-predicate pred))

  (unless (source:location pred)
    (util:coalton-bug "Predicate ~S does not have source information" pred))

  (tc-error "Unknown instance" (tc-note pred "Unknown instance ~S" pred)))

(defun standard-expression-type-mismatch-error (node subs expected-type ty)
  "Utility for signalling a type-mismatch error in INFER-EXPRESSION-TYPE"
  (tc-error "Type mismatch"
            (tc-note node "Expected type '~S' but got '~S'"
                     (tc:apply-substitution subs expected-type)
                     (tc:apply-substitution subs ty))))

;;;
;;; Entrypoint
;;;

(defun toplevel-define (defines declares env)
  "Entrypoint for typechecking a group of parsed defines and declares."
  (declare (type parser:toplevel-define-list defines)
           (type parser:toplevel-declare-list declares)
           (type tc:environment env)
           (values toplevel-define-list tc:environment))

  ;; Ensure that all defines are in the current package
  (check-package defines
                 (alexandria:compose #'parser:node-variable-name
                                     #'parser:toplevel-define-name)
                 (alexandria:compose #'source:location
                                     #'parser:toplevel-define-name))

  ;; Ensure that there are no duplicate definitions
  (check-duplicates
   defines
   (alexandria:compose #'parser:node-variable-name #'parser:toplevel-define-name)
   (lambda (first second)
     (tc-error "Duplicate definition"
               (tc-note (parser:toplevel-define-name first)
                        "first definition here")
               (tc-note (parser:toplevel-define-name second)
                        "second definition here"))))

  ;; Ensure that there are no duplicate declarations
  (check-duplicates
   declares
   (alexandria:compose #'parser:identifier-src-name #'parser:toplevel-declare-name)
   (lambda (first second)
     (tc-error "Duplicate declaration"
               (tc-note (parser:toplevel-declare-name first)
                        "first declaration here")
               (tc-note (parser:toplevel-declare-name second)
                        "second declaration here"))))

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
          :do (tc-error "Orphan declaration"
                        (tc-note (parser:toplevel-declare-name declare)
                                 "declaration does not have an associated definition")))

  (let ((dec-table (make-hash-table :test #'eq))

        (tc-env (make-tc-env :env env)))

    (loop :for declare :in declares
          :for name := (parser:identifier-src-name (parser:toplevel-declare-name declare))
          :for ty := (parser:toplevel-declare-type declare)
          :do (setf (gethash name dec-table) ty))

    ;; Infer binding types, returning the typed nodes.
    (multiple-value-bind (preds accessors binding-nodes subs)
        (infer-bindings-type defines dec-table nil tc-env)
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

        ;; Check types and update environment
        (when (catch 'redef:abort-redef
                (handler-bind
                    ((redef:incompatible-redefinition
                       (lambda (c)
                         (declare (ignore c))
                         (when settings:*auto-continue-redefinition*
                           (let ((restart (find-restart 'redef:continue-anyway)))
                             (when restart
                               (invoke-restart restart)))))))
                  (loop :for define :in defines
                        :for name := (parser:node-variable-name (parser:binding-name define))
                        :for scheme := (tc:remove-source-info (gethash name (tc-env-ty-table tc-env)))

                        :when (tc:type-variables scheme)
                          :do (util:coalton-bug "Scheme ~S should not have any free type variables." scheme)

                        ;; Check for incompatible redefinition before updating environment
                        :do (let ((old-type (tc:lookup-value-type env name :no-error t)))
                              (when old-type
                                (unless (redef:types-compatible-p old-type scheme)
                                  (let ((affected (redef:find-affected-functions name)))
                                    (when affected
                                      (redef:raise-redefinition-error
                                       :function-name name
                                       :old-type old-type
                                       :new-type scheme
                                       :affected-functions affected
                                       :environment env))))))

                            (setf env (tc:set-value-type env name scheme))

                            (setf env (tc:set-name env name (tc:make-name-entry
                                                             :name name
                                                             :type :value
                                                             :docstring (source:docstring define)
                                                             :location (source:location define))))

                        :if (parser:toplevel-define-orig-params define)
                          :do (setf env (tc:set-function-source-parameter-names
                                         env
                                         name
                                         (parser:toplevel-define-orig-params define)))
                        :else
                          :if (tc:lookup-function-source-parameter-names env name)
                            :do (setf env (tc:unset-function-source-parameter-names env name))))
                nil)
          (return-from toplevel-define (values nil env)))

        ;; Record dependencies
        (loop :for define :in defines
              :for name := (parser:node-variable-name (parser:binding-name define))
              :for code := (parser:binding-value define)
              :do (redef:record-dependencies name code env))

        (values
         (tc:apply-substitution subs binding-nodes)
         env)))))

;;;
;;; Expression Type Inference
;;;

(defun exception-type-p (inferred-ty env)
  (alexandria:when-let* ((ty-name (if (types:tycon-p inferred-ty)
                                      (types:tycon-name inferred-ty)
                                      nil))
                         (entry   (tc:lookup-type (tc-env-env env) ty-name)))
    (tc:type-entry-exception-p entry)))

(defun resumption-type-p (inferred-ty env)
  (alexandria:when-let* ((ty-name (if (types:tycon-p inferred-ty)
                                      (types:tycon-name inferred-ty)
                                      nil))
                         (entry   (tc:lookup-type (tc-env-env env) ty-name)))
    (tc:type-entry-resumption-p entry)))

(defgeneric infer-expression-type (node expected-type subs env)
  (:documentation "Infer the type of NODE and then unify against EXPECTED-TYPE

Returns (VALUES INFERRED-TYPE PREDICATES NODE SUBSTITUTIONS)")
  (:method ((node parser:node-literal) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-literal tc:substitution-list &optional))

    (let ((ty (etypecase (parser:node-literal-value node)
                (ratio        tc:*fraction-type*)
                (single-float tc:*single-float-type*)
                (double-float tc:*double-float-type*)
                (string       tc:*string-type*)
                (character    tc:*char-type*))))

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
                :location (source:location node)
                :value (parser:node-literal-value node))
               subs)))

        (tc:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node subs expected-type ty)))))

  (:method ((node parser:node-accessor) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-accessor tc:substitution-list))

    (let* ((from-ty
             (tc:make-variable))
           (to-ty
             (tc:make-variable))
           (ty
             (tc:make-function-type from-ty to-ty)))
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
                 :location (source:location node)))
               (make-node-accessor
                :type (tc:qualify nil type)
                :location (source:location node)
                :name (parser:node-accessor-name node))
               subs))))))

  (:method ((node parser:node-integer-literal) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-integer-literal tc:substitution-list &optional))
    
    (let* ((num
             (util:find-symbol "NUM" "COALTON/CLASSES"))
           (tvar
             (tc:make-variable))
           (pred
             (tc:make-ty-predicate :class num :types (list tvar) :location (source:location node))))
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
                :location (source:location node)
                :value (parser:node-integer-literal-value node))
               subs)))
        (tc:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node subs expected-type tvar)))))

  (:method ((node parser:node-variable) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-variable tc:substitution-list &optional))

    (multiple-value-bind (ty preds)
        (tc-env-lookup-value env node)
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
                :location (source:location node)
                :name (parser:node-variable-name node))
               subs)))
        (tc:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node subs expected-type ty)))))

  (:method ((node parser:node-application) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-application tc:substitution-list &optional))

    (multiple-value-bind (fun-ty preds accessors rator-node subs)
        (infer-expression-type (parser:node-application-rator node)
                               (tc:make-variable)
                               subs
                               env)

      (let* ((rands (or (parser:node-application-rands node)
                        (list
                         (parser:make-node-variable
                          :location (source:location node)
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
                                                            env)
                                   (declare (ignore ty_))
                                   (setf preds (append preds preds_))
                                   (setf accessors (append accessors accessors_))
                                   (setf subs subs_)
                                   (setf fun-ty_ (tc:apply-substitution subs (tc:function-type-to fun-ty_)))

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
                                                              env)
                                     (declare (ignore ty_))
                                     (setf preds (append preds preds_))
                                     (setf accessors (append accessors accessors_))
                                     (setf subs subs_)
                                     (setf fun-ty_ new-to)

                                     node_)))

                                ;; Otherwise signal an error
                                (t
                                 (setf fun-ty (tc:apply-substitution subs fun-ty))
                                 (tc-error "Argument error"
                                           (if (null (tc:function-type-arguments fun-ty))
                                               (tc-note node "Unable to call value of type '~S': it is not a function"
                                                        fun-ty)
                                               (tc-note node "Function call has ~D arguments but inferred type '~S' only takes ~D"
                                                        (length rands)
                                                        fun-ty
                                                        (length (tc:function-type-arguments fun-ty))))))))))

        (handler-case
            (progn
              (setf subs (tc:unify subs fun-ty_ expected-type))
              (let ((type (tc:apply-substitution subs fun-ty_)))
                (values type
                        preds
                        accessors
                        (make-node-application :type (tc:qualify nil type)
                                               :location (source:location node)
                                               :rator rator-node
                                               :rands rand-nodes)
                        subs)))
          (tc:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node subs expected-type fun-ty_))))))

  (:method ((node parser:node-bind) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values null tc:ty-predicate-list accessor-list node-bind tc:substitution-list))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-bind-expr node)
                               (tc:make-variable)
                               subs
                               env)

      (multiple-value-bind (pat-ty pat-node subs)
          (infer-pattern-type (parser:node-bind-pattern node)
                              expr-ty   ; unify against expr-ty
                              subs
                              env)
        (declare (ignore pat-ty))

        (values nil         ; return nil as this is always thrown away
                preds
                accessors
                (make-node-bind
                 ;; NOTE: We don't attach type here because NODE-BIND has no
                 ;; meaningful type.
                 :location (source:location node)
                 :pattern pat-node
                 :expr expr-node)
                subs))))

  (:method ((node parser:node-body) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-body tc:substitution-list))

    (let* ((preds nil)
           (accessors nil)

           ;; Infer the type of each node
           (body-nodes
             (loop :for node_ :in (parser:node-body-nodes node)
                   :collect (multiple-value-bind (node_ty_ preds_ accessors_ node_ subs_)
                                (infer-expression-type node_ (tc:make-variable) subs env)
                              (declare (ignore node_ty_))
                              (setf subs subs_)
                              (setf preds (append preds preds_))
                              (setf accessors (append accessors accessors_))
                              node_))))

      (multiple-value-bind (ty preds_ accessors_ last-node subs)
          (infer-expression-type (parser:node-body-last-node node) expected-type subs env)
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

  (:method ((node parser:node-abstraction) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-abstraction tc:substitution-list &optional))

    (check-duplicates
     (parser:pattern-variables (parser:node-abstraction-params node))
     #'parser:pattern-var-name
     (lambda (first second)
       (tc-error "Duplicate parameters name"
                 (tc-note first "first parameter here")
                 (tc-note second "second parameter here"))))

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
                   :location (source:location node)))
                 (loop :for pattern :in (parser:node-abstraction-params node)
                       :for ty :in arg-tys
                       :collect (multiple-value-bind (ty_ pattern subs_)
                                    (infer-pattern-type pattern ty subs env)
                                  (declare (ignore ty_))
                                  (setf subs subs_)
                                  pattern)))))

      (multiple-value-bind (body-ty preds accessors body-node subs)
          (infer-expression-type (parser:node-abstraction-body node)
                                 (tc:make-variable)
                                 subs
                                 env)

        ;; Ensure that all early returns unify
        (loop :with returns := (reverse *returns*)
              :for (s1 . ty1) :in returns
              :for (s2 . ty2) :in (cdr returns)
              :do (handler-case
                      (setf subs (tc:unify subs ty1 ty2))
                    (tc:coalton-internal-type-error ()
                      (tc-error "Return type mismatch"
                                (tc-note s1
                                         "First return is of type '~S'"
                                         (tc:apply-substitution subs ty1))
                                (tc-note s2
                                         "Second return is of type '~S'"
                                         (tc:apply-substitution subs ty2))))))

        ;; Unify the function's inferred type with one of the early returns.
        (when *returns*
          (handler-case
              (setf subs (tc:unify subs (cdr (first *returns*)) body-ty))
            (tc:coalton-internal-type-error ()
              (tc-error "Return type mismatch"
                        (tc-note (car (first *returns*))
                                 "First return is of type '~S'"
                                 (tc:apply-substitution subs (cdr (first *returns*))))
                        (tc-note (parser:node-body-last-node (parser:node-abstraction-body node))
                                 "Second return is of type '~S'"
                                 (tc:apply-substitution subs body-ty))))))

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
                    :location (source:location node)
                    :params params
                    :body body-node)
                   subs)))
            (tc:coalton-internal-type-error ()
              (standard-expression-type-mismatch-error node subs expected-type ty)))))))

  (:method ((node parser:node-let) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-let tc:substitution-list))

    ;; Ensure that there are no duplicate let bindings
    (check-duplicates
     (parser:node-let-bindings node)
     (alexandria:compose #'parser:node-variable-name #'parser:node-let-binding-name)
     (lambda (first second)
       (tc-error "Duplicate definition in let"
                 (tc-note first "first definition here")
                 (tc-note second "second definition here"))))

    (multiple-value-bind (preds accessors binding-nodes subs)
        (infer-let-bindings (parser:node-let-bindings node) (parser:node-let-declares node) subs env)

      (multiple-value-bind (ty preds_ accessors_ body-node subs)
          (infer-expression-type (parser:node-let-body node)
                                 expected-type ; pass through expected type
                                 subs
                                 env)
        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (values
         ty
         preds
         accessors
         (make-node-let
          :type (tc:qualify nil ty)
          :location (source:location node)
          :bindings binding-nodes
          :body body-node)
         subs))))

  (:method ((node parser:node-lisp) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-lisp tc:substitution-list &optional))

    (let ((declared-ty (parse-type (parser:node-lisp-type node) (tc-env-env env))))

      (handler-case
          (progn
            (setf subs (tc:unify subs declared-ty expected-type))
            (let ((type (tc:apply-substitution subs declared-ty))

                  (var-nodes
                    (mapcar (lambda (var)
                              (make-node-variable
                               :type (tc:qualify nil (tc-env-lookup-value env var))
                               :location (source:location var)
                               :name (parser:node-variable-name var)))
                            (parser:node-lisp-vars node))))
              (when (eq ':values (parser:node-lisp-return-convention node))
                (unless (types:tuple-component-types type)
                  (tc-error "Invalid lisp multiple-values return type"
                            (tc-note (parser:node-lisp-type node)
                                     "`(lisp multiple-values ...)` requires a Tuple return type, but got '~S'"
                                     type))))
              (values
               type
               nil
               nil
               (make-node-lisp
                :type (tc:qualify nil type)
                :location (source:location node)
                :vars var-nodes
                :var-names (parser:node-lisp-var-names node)
                :return-convention (parser:node-lisp-return-convention node)
                :body (parser:node-lisp-body node))
               subs)))
        (tc:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node subs expected-type declared-ty)))))

  (:method ((node parser:node-match) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-match tc:substitution-list &optional))

    ;; Infer the type of the expression being cased on
    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-match-expr node)
                               (tc:make-variable)
                               subs
                               env)

      (let* (;; Infer the type of each pattern, unifying against expr-ty
             (pat-nodes
               (loop :for branch :in (parser:node-match-branches node)
                     :for pattern := (parser:node-match-branch-pattern branch)
                     :collect (multiple-value-bind (pat-ty pat-node subs_)
                                  (infer-pattern-type pattern expr-ty subs env)
                                (declare (ignore pat-ty))
                                (setf subs subs_)
                                pat-node)))

             (ret-ty (tc:make-variable))

             ;; Infer the type of each branch, unifying against ret-ty
             (branch-body-nodes
               (loop :for branch :in (parser:node-match-branches node)
                     :for body := (parser:node-match-branch-body branch)
                     :collect (multiple-value-bind (body-ty preds_ accessors_ body-node subs_)
                                  (infer-expression-type body ret-ty subs env)
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
                               :location (source:location branch)))))

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
                  :location (source:location node)
                  :expr expr-node
                  :branches branch-nodes)
                 subs)))
          (tc:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node subs expected-type ret-ty))))))

  (:method ((node parser:node-catch) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-catch tc:substitution-list &optional))
    ;; Infer type of the expression that may throw an exception
    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-catch-expr node)
                               (tc:make-variable)
                               subs
                               env)

      (let* (;; Infer type of each pattern, ensuring it is an exception type
             (branch-pat-nodes
               (loop
                 :for branch :in (parser:node-catch-branches node)
                 :for pattern := (parser:node-catch-branch-pattern branch)
                 :for (pat-ty pat-node subs_)
                   := (multiple-value-list (infer-pattern-type pattern (tc:make-variable) subs env))
                 :unless (or (exception-type-p pat-ty env)
                             (typep pattern 'parser:pattern-wildcard))
                   :do (tc-error
                        "Invalid catch case"
                        (tc-note
                         pat-node
                         "Catch branch pattern must be an exception constructor pattern or a wildcard."))
                 :else 
                   :do (setf subs subs_)
                   :and :collect pat-node))
             ;; Infer type of each branch body, unifying against expr-ty
             (branch-body-nodes
               (loop
                 :for branch :in (parser:node-catch-branches node)
                 :for body := (parser:node-catch-branch-body branch)
                 :collect (multiple-value-bind (body-ty preds_ accessors_ body-node subs_)
                              (infer-expression-type body expr-ty subs env)
                            (declare (ignore body-ty))
                            (setf subs subs_)
                            (setf preds (append preds preds_))
                            (setf accessors (append accessors accessors_))
                            body-node)))

             (branch-nodes
               (loop
                 :for branch :in (parser:node-catch-branches node)
                 :for pat-node :in branch-pat-nodes
                 :for branch-body-node :in branch-body-nodes
                 :collect (make-node-catch-branch
                           :pattern pat-node
                           :body branch-body-node
                           :location (source:location branch)))))
        (handler-case
            (progn
              (setf subs (tc:unify subs expr-ty expected-type))
              (let ((type (tc:apply-substitution subs expr-ty)))
                (values
                 type
                 preds
                 accessors
                 (make-node-catch
                  :type (tc:qualify nil type)
                  :location (source:location node)
                  :expr expr-node
                  :branches branch-nodes)
                 subs)))
          (tc:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node subs expected-type expr-ty))))))

  (:method ((node parser:node-resumable) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-resumable tc:substitution-list &optional))
    
    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-resumable-expr node)
                               (tc:make-variable)
                               subs
                               env)
      (let* (;; infer type of each pattern, ensuring it is a resumption type
             (branch-pat-nodes
               (loop
                 :for branch :in (parser:node-resumable-branches node)
                 :for pattern := (parser:node-resumable-branch-pattern branch)
                 :for (pat-ty pat-node subs_)
                   := (multiple-value-list (infer-pattern-type pattern (tc:make-variable) subs env))
                 :unless (resumption-type-p pat-ty env)
                   :do (tc-error "Invalid resumable case"
                                 (tc-note pat-node "case pattern must construct a resumption type."))
                 :else 
                   :do (setf subs subs_)
                   :and :collect pat-node))
             ;; Infer type of each branch body, it should unify with the expr/expected type
             (branch-body-nodes
               (loop
                 :for branch :in (parser:node-resumable-branches node)
                 :for body := (parser:node-resumable-branch-body branch)
                 :collect (multiple-value-bind (body-ty preds_ accessors_ body-node subs_)
                              (infer-expression-type body expr-ty subs env)
                            (declare (ignore body-ty))
                            (setf subs subs_)
                            (setf preds (append preds preds_))
                            (setf accessors (append accessors accessors_))
                            body-node)))
             (branch-nodes
               (loop
                 :for branch :in (parser:node-resumable-branches node)
                 :for pat-node :in branch-pat-nodes
                 :for branch-body-node :in branch-body-nodes
                 :collect (make-node-resumable-branch
                           :pattern pat-node
                           :body branch-body-node
                           :location (source:location branch)))))
        (handler-case
            (progn
              (setf subs (tc:unify subs expr-ty expected-type))
              (let ((type (tc:apply-substitution subs expr-ty)))
                (values
                 type
                 preds
                 accessors
                 (make-node-resumable
                  :type (tc:qualify nil type)
                  :location (source:location node)
                  :expr expr-node
                  :branches branch-nodes)
                 subs)))
          (tc:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node subs expected-type expr-ty))))))

  (:method ((node parser:node-progn) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-progn tc:substitution-list))

    (multiple-value-bind (body-ty preds accessors body-node subs)
        (infer-expression-type (parser:node-progn-body node)
                               expected-type
                               subs
                               env)
      (values
       body-ty
       preds
       accessors
       (make-node-progn
        :type (tc:qualify nil body-ty)
        :location (source:location node)
        :body body-node)
       subs)))

  (:method ((node parser:node-the) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node tc:substitution-list &optional))

    (let ((declared-ty (parse-type (parser:node-the-type node) (tc-env-env env))))

      (multiple-value-bind (expr-ty preds accessors expr-node subs)
          (infer-expression-type (parser:node-the-expr node)
                                 (tc:make-variable)
                                 subs
                                 env)

        ;; Ensure subs are applied
        (setf expr-ty (tc:apply-substitution subs expr-ty))

        ;; Check that declared-ty and expr-ty unify
        (handler-case
            (setf subs (tc:unify subs declared-ty expr-ty))
          (tc:coalton-internal-type-error ()
            (tc-error "Type mismatch"
                      (tc-note node "Declared type '~S' does not match inferred type '~S'"
                               (tc:apply-substitution subs declared-ty)
                               (tc:apply-substitution subs expr-ty)))))

        ;; Check that declared-ty is not more specific than expr-ty
        (handler-case
            (tc:match expr-ty declared-ty)
          (tc:coalton-internal-type-error ()
            (tc-error "Declared type too general"
                      (tc-note node "Declared type '~S' is more general than inferred type '~S'"
                               (tc:apply-substitution subs declared-ty)
                               (tc:apply-substitution subs expr-ty)))))

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
          (tc:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node subs expected-type expr-ty))))))

  (:method ((node parser:node-return) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-return tc:substitution-list))

    ;; Returns must be inside a lambda
    (when (eq *return-status* :toplevel)
      (tc-error "Unexpected return"
                (tc-note node "returns must be inside a lambda")))

    ;; Returns cannot be in a do expression
    (when (eq *return-status* :do)
      (tc-error "Invalid return"
                (tc-note node "returns cannot be in a do expression")))

    (multiple-value-bind (ty preds accessors expr-node subs)
        (infer-expression-type (or (parser:node-return-expr node)
                                   ;; If the return looks like (return) then it returns unit
                                   (parser:make-node-variable
                                    :location (source:location node)
                                    :name 'coalton:Unit))
                               (tc:make-variable)
                               subs
                               env)

      ;; Add node the the list of returns
      (push (cons (source:location node) ty) *returns*)

      (values
       expected-type
       preds
       accessors
       (make-node-return
        :type (tc:qualify nil expected-type)
        :location (source:location node)
        :expr expr-node)
       subs)))

  (:method ((node parser:node-throw) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-throw tc:substitution-list))

    (multiple-value-bind (exception-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-throw-expr node)
                               (tc:make-variable)
                               subs
                               env)

      (unless (exception-type-p exception-ty env)
        (tc-error
         "Invalid throw"
         (tc-note
          expr-node
          "Argument to `throw` must be a known exception.")
         (tc-note
          expr-node
          "Not Yet Supported: throw polymorphism.")))
      
      (values
       expected-type
       preds
       accessors
       (make-node-throw
        :type (tc:qualify nil expected-type)
        :location (source:location node)
        :expr expr-node)
       subs)))

  (:method ((node parser:node-resume-to) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-resume-to tc:substitution-list))

    (multiple-value-bind (resumption-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-resume-to-expr node)
                               (tc:make-variable)
                               subs
                               env)

      (unless (resumption-type-p resumption-ty env)
        (tc-error "Invalid resume-to"
                  (tc-note node "Argument to `resume-to` be a known resumption.")
                  (tc-note node "Not Yet Supported: resume-to polymorphism.")))
      
      (values
       expected-type
       preds
       accessors
       (make-node-resume-to
        :type (tc:qualify nil expected-type)
        :location (source:location node)
        :expr expr-node)
       subs)))

  (:method ((node parser:node-or) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-or tc:substitution-list))

    (let* ((preds nil)
           (accessors nil)

           (body-nodes
             (loop :for node_ :in (parser:node-or-nodes node)
                   :collect (multiple-value-bind (node_ty_ preds_ accessors_ node_ subs_)
                                (infer-expression-type node_
                                                       tc:*boolean-type*
                                                       subs
                                                       env)
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
              :location (source:location node)
              :nodes body-nodes)
             subs))
        (tc:coalton-internal-type-error ()
          (tc-error "Type mismatch"
                    (tc-note node "Expected type '~S' but 'or' evaluates to '~S'"
                             (tc:apply-substitution subs expected-type)
                             tc:*boolean-type*))))))
  
  (:method ((node parser:node-and) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-and tc:substitution-list))

    (let* ((preds nil)
           (accessors nil)

           (body-nodes
             (loop :for node_ :in (parser:node-and-nodes node)
                   :collect (multiple-value-bind (node_ty_ preds_ accessors_ node_ subs_)
                                (infer-expression-type node_
                                                       tc:*boolean-type*
                                                       subs
                                                       env)
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
              :location (source:location node)
              :nodes body-nodes)
             subs))
        (tc:coalton-internal-type-error ()
          (tc-error "Type mismatch"
                    (tc-note node "Expected type '~S' but 'and' evaluates to '~S'"
                             (tc:apply-substitution subs expected-type)
                             tc:*boolean-type*))))))

  (:method ((node parser:node-if) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-if tc:substitution-list &optional))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-if-expr node)
                               tc:*boolean-type* ; unify predicate against boolean
                               subs
                               env)
      (declare (ignore expr-ty))

      (multiple-value-bind (then-ty preds_ accessors_ then-node subs)
          (infer-expression-type (parser:node-if-then node)
                                 expected-type
                                 subs
                                 env)
        (declare (ignore then-ty))
        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (multiple-value-bind (else-ty preds_ accessors_ else-node subs)
            (infer-expression-type (parser:node-if-else node)
                                   expected-type
                                   subs
                                   env)
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
                  :location (source:location node)
                  :expr expr-node
                  :then then-node
                  :else else-node)
                 subs))
            (tc:coalton-internal-type-error ()
              (standard-expression-type-mismatch-error node subs expr-node else-ty)))))))

  (:method ((node parser:node-when) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-when tc:substitution-list &optional))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-when-expr node)
                               tc:*boolean-type*
                               subs
                               env)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
          (infer-expression-type (parser:node-when-body node)
                                 tc:*unit-type*
                                 subs
                                 env)
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
                :location (source:location node)
                :expr expr-node
                :body body-node)
               subs))
          (tc:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node subs expected-type body-ty))))))

  (:method ((node parser:node-unless) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-unless tc:substitution-list &optional))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-unless-expr node)
                               tc:*boolean-type*
                               subs
                               env)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
          (infer-expression-type (parser:node-unless-body node)
                                 tc:*unit-type*
                                 subs
                                 env)
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
                :location (source:location node)
                :expr expr-node
                :body body-node)
               subs))
          (tc:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node subs expected-type body-ty))))))


  (:method ((node parser:node-while) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-while tc:substitution-list &optional))
    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-while-expr node)
                               tc:*boolean-type*
                               subs
                               env)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
          (infer-expression-type (parser:node-while-body node)
                                 (tc:make-variable)
                                 subs
                                 env)
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
                :location (source:location node)
                :label (parser:node-while-label node)
                :expr expr-node
                :body body-node)
               subs))
          (tc:coalton-internal-type-error ()
            (standard-expression-type-mismatch-error node subs expected-type tc:*unit-type*))))))

  (:method ((node parser:node-while-let) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-while-let tc:substitution-list &optional))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-while-let-expr node)
                               (tc:make-variable) 
                               subs
                               env)

      (multiple-value-bind (pat-ty pat-node subs)
          (infer-pattern-type (parser:node-while-let-pattern node) expr-ty subs env)
        (declare (ignore pat-ty))
        
        (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
            (infer-expression-type (parser:node-while-let-body node)
                                   (tc:make-variable)
                                   subs
                                   env)
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
                  :location (source:location node)
                  :label (parser:node-while-let-label node)
                  :pattern pat-node
                  :expr expr-node
                  :body body-node)
                 subs))
            (tc:coalton-internal-type-error ()
              (standard-expression-type-mismatch-error node subs expected-type tc:*unit-type*)))))))


  (:method ((node parser:node-for) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-for tc:substitution-list &optional))

    (let ((intoiter-symbol
            (util:find-symbol "INTOITERATOR" "COALTON/ITERATOR")))

      (multiple-value-bind (pat-ty pat-node subs)
          (infer-pattern-type (parser:node-for-pattern node) (tc:make-variable) subs env)

        (multiple-value-bind (expr-ty preds accessors expr-node subs)
            (infer-expression-type (parser:node-for-expr node) (tc:make-variable) subs env)

          (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
              (infer-expression-type (parser:node-for-body node) (tc:make-variable) subs env)

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
                     :location (source:location node))
                    preds)
                   accessors
                   (make-node-for
                    :type (tc:qualify nil tc:*unit-type*)
                    :location (source:location node)
                    :label (parser:node-for-label node)
                    :pattern pat-node
                    :expr expr-node
                    :body body-node)
                   subs))
              (tc:coalton-internal-type-error ()
                (standard-expression-type-mismatch-error node subs expected-type tc:*unit-type*))))))))

  (:method ((node parser:node-loop) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-loop tc:substitution-list &optional))

    (multiple-value-bind (body-ty preds accessors body-node subs)
        (infer-expression-type (parser:node-loop-body node)
                               (tc:make-variable)
                               subs
                               env)
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
              :location (source:location node)
              :label (parser:node-loop-label node)
              :body body-node)
             subs))
        (tc:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node subs expected-type tc:*unit-type* )))))

  (:method ((node parser:node-break) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-break tc:substitution-list &optional))
    (handler-case
        (progn
          (setf subs (tc:unify subs tc:*unit-type* expected-type)) 
          (values
           tc:*unit-type*
           nil
           nil
           (make-node-break
            :type (tc:qualify nil tc:*unit-type*)
            :location (source:location node)
            :label (parser:node-break-label node))
           subs))
      (tc:coalton-internal-type-error ()
        (standard-expression-type-mismatch-error node subs expected-type tc:*unit-type*))))

  (:method ((node parser:node-continue) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-continue tc:substitution-list &optional))
    (handler-case
        (progn
          (setf subs (tc:unify subs tc:*unit-type* expected-type))
          (values
           tc:*unit-type*
           nil
           nil
           (make-node-continue
            :type (tc:qualify nil tc:*unit-type*)
            :location (source:location node)
            :label (parser:node-continue-label node))
           subs))
      (tc:coalton-internal-type-error ()
        (standard-expression-type-mismatch-error node subs expected-type tc:*unit-type*))))


  (:method ((node parser:node-cond-clause) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-cond-clause tc:substitution-list))

    (multiple-value-bind (expr-ty preds accessors expr-node subs)
        (infer-expression-type (parser:node-cond-clause-expr node)
                               tc:*boolean-type*
                               subs
                               env)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ accessors_ body-node subs)
          (infer-expression-type (parser:node-cond-clause-body node)
                                 expected-type ; unify against expected-type
                                 subs
                                 env)
        (setf preds (append preds preds_))
        (setf accessors (append accessors accessors_))

        (let ((type (tc:apply-substitution subs body-ty)))
          (values
           type
           preds
           accessors
           (make-node-cond-clause
            :location (source:location node)
            :expr expr-node
            :body body-node)
           subs)))))

  (:method ((node parser:node-cond) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-cond tc:substitution-list &optional))

    (let* ((preds nil)
           (accessors nil)

           (ret-ty (tc:make-variable))

           (clause-nodes
             (loop :for clause :in (parser:node-cond-clauses node)
                   :collect (multiple-value-bind (clause-ty preds_ accessors_ clause-node subs_)
                                (infer-expression-type clause
                                                       ret-ty
                                                       subs
                                                       env)
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
                :location (source:location node)
                :clauses clause-nodes)
               subs)))
        (tc:coalton-internal-type-error ()
          (standard-expression-type-mismatch-error node subs expected-type ret-ty)))))

  (:method ((node parser:node-do-bind) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-do-bind tc:substitution-list))

    (let ((*return-status* :do))

      (multiple-value-bind (expr-ty preds accessors expr-node subs)
          (infer-expression-type (parser:node-do-bind-expr node)
                                 expected-type ; unify here so that expr-ty is in the form "m a"
                                 subs
                                 env)

        (multiple-value-bind (ty_ pattern subs)
            (infer-pattern-type (parser:node-do-bind-pattern node)
                                (tc:tapp-to (tc:apply-substitution subs expr-ty)) ; this should never fail
                                subs
                                env)
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
                  :location (source:location node))
                 subs))
            (tc:coalton-internal-type-error ()
              (tc-error "Type mismatch"
                        (tc-note node "Expected type '~S' but got '~S'"
                                 (tc:apply-substitution subs expected-type)
                                 (tc:apply-substitution subs expr-ty)))))))))

  (:method ((node parser:node-do) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty tc:ty-predicate-list accessor-list node-do tc:substitution-list))

    (let* (;; m-type is the type of the monad and has kind "* -> *"
           (m-type (tc:make-variable (tc:make-kfun :from tc:+kstar+ :to tc:+kstar+)))

           (monad-symbol (util:find-symbol "MONAD" "COALTON/CLASSES"))

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
                                                          env)
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
                                                          env)
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
                                                          env)
                                 (declare (ignore ty_))
                                 (setf preds (append preds preds_))
                                 (setf accessors (append accessors accessors_))
                                 (setf subs subs_)
                                 node_))))))

      (multiple-value-bind (ty preds_ accessors_ last-node subs)
          (infer-expression-type (parser:node-do-last-node node)
                                 (tc:make-tapp :from m-type
                                               :to (tc:make-variable))
                                 subs
                                 env)

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
                 :location (source:location node))
                preds)
               accessors
               (make-node-do
                :type (tc:qualify nil ty)
                :location (source:location node)
                :nodes nodes
                :last-node last-node) 
               subs))
          (tc:coalton-internal-type-error ()
            (tc-error "Type mismatch"
                      (tc-note node "Expected type '~S' but do expression has type '~S'"
                               (tc:apply-substitution subs expected-type)
                               (tc:apply-substitution subs ty)))))))))

;;;
;;; Pattern Type Inference
;;;

(defgeneric infer-pattern-type (pat expected-typ subs env)
  (:documentation "Infer the type of pattern PAT and then unify against EXPECTED-TYPE.

Returns (VALUES INFERRED-TYPE NODE SUBSTITUTIONS)")
  (:method ((pat parser:pattern-binding) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty pattern-binding tc:substitution-list))

    (check-duplicates (parser:pattern-variables pat)
                      #'parser:pattern-var-name
                      (lambda (first second)
                        (tc-error "Duplicate pattern variable"
                                  (tc-note first "first definition here")
                                  (tc-note second "second definition here"))))

    (multiple-value-bind (pat-ty bound subs)
        (infer-pattern-type (parser:pattern-binding-pattern pat) expected-type subs env)
      (multiple-value-bind (pat-ty var subs)
          (infer-pattern-type (parser:pattern-binding-var pat) pat-ty subs env)

        (values
         pat-ty
         (make-pattern-binding
          :type (tc:qualify nil pat-ty)
          :var var
          :pattern bound)
         subs))))

  (:method ((pat parser:pattern-var) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty pattern-var tc:substitution-list))

    (let ((ty (tc-env-add-variable env (parser:pattern-var-name pat))))

      ;; SAFETY: unification against a variable will never fail
      (setf subs (tc:unify subs ty expected-type))

      (let ((type (tc:apply-substitution subs ty)))
        (values
         type
         (make-pattern-var
          :type (tc:qualify nil type)
          :location (source:location pat)
          :name (parser:pattern-var-name pat)
          :orig-name (parser:pattern-var-orig-name pat))
         subs))))

  (:method ((pat parser:pattern-literal) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty pattern-literal tc:substitution-list))

    (let ((ty (etypecase (parser:pattern-literal-value pat)
                (integer (let* ((num
                                  (util:find-symbol "NUM" "COALTON/CLASSES"))
                                (tvar
                                  (tc:make-variable))
                                (pred
                                  (tc:make-ty-predicate :class num :types (list tvar) :location (source:location pat))))
                           (setf subs (tc:compose-substitution-lists (tc:default-subs (tc-env-env env) (list tvar) (list pred)) subs))
                           tvar))
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
                :location (source:location pat)
                :value (parser:pattern-literal-value pat))
               subs)))
        (tc:coalton-internal-type-error ()
          (tc-error "Type mismatch"
                    (tc-note pat "Expected type '~S' but pattern literal has type '~S'"
                             (tc:apply-substitution subs expected-type)
                             (tc:apply-substitution subs ty)))))))

  (:method ((pat parser:pattern-wildcard) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty pattern-wildcard tc:substitution-list))

    (values
     expected-type
     (make-pattern-wildcard
      :type (tc:qualify nil expected-type)
      :location (source:location pat))
     subs))

  (:method ((pat parser:pattern-constructor) expected-type subs env)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (values tc:ty pattern-constructor tc:substitution-list))

    (let ((ctor (tc:lookup-constructor (tc-env-env env) (parser:pattern-constructor-name pat) :no-error t)))

      (check-duplicates (parser:pattern-variables pat)
                        #'parser:pattern-var-name
                        (lambda (first second)
                          (tc-error "Duplicate pattern variable"
                                    (tc-note first "first definition here")
                                    (tc-note second "second definition here"))))

      (unless ctor
        (tc-error "Unknown constructor"
                  (tc-note pat "constructor is not known")))

      (let ((arity
              (tc:constructor-entry-arity ctor))
            (num-args
              (length (parser:pattern-constructor-patterns pat))))
        (unless (= arity num-args)
          (tc-error "Argument mismatch"
                    (tc-note pat "Constructor ~A takes ~D arguments but is given ~D"
                             (parser:pattern-constructor-name pat)
                             arity
                             num-args)))

        (let* ((ctor-ty (tc:qualified-ty-type ;; NOTE: Constructors cannot have predicates
                         (tc:fresh-inst
                          (tc:lookup-value-type (tc-env-env env) (parser:pattern-constructor-name pat)))))

               (pat-ty (tc:function-return-type ctor-ty))

               (pattern-nodes
                 (loop :for arg :in (parser:pattern-constructor-patterns pat)
                       :for arg-ty :in (tc:function-type-arguments ctor-ty)
                       :collect (multiple-value-bind (ty_ node_ subs_)
                                    (infer-pattern-type arg arg-ty subs env)
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
                    :location (source:location pat)
                    :name (parser:pattern-constructor-name pat)
                    :patterns pattern-nodes)
                   subs)))
            (tc:coalton-internal-type-error ()
              (tc-error "Type mismatch"
                        (tc-note pat "Expected type '~S' but pattern has type '~S'"
                                 (tc:apply-substitution subs expected-type)
                                 (tc:apply-substitution subs pat-ty))))))))))

;;;
;;; Binding Group Type Inference
;;;

;;; ---------------------------------------------------------------------------
;;; Value restriction and weak type variables
;;;
;;; Coalton follows the same safety strategy used by ML-family languages for
;;; mutable state: only non-expansive bindings are generalized.
;;;
;;; - Non-expansive expressions are "values" (variables, literals, lambdas, and
;;;   constructor applications over non-expansive arguments).
;;; - Expansive expressions are everything else: they may allocate, mutate, or
;;;   observe effects, so they introduce weak type variables.
;;;
;;; Any type variables that come from expansive bindings are treated as weak:
;;; they can be solved by unification, and are generalized only when they
;;; appear in covariant positions (relaxed value restriction).
;;;
;;; The expansive/non-expansive check here is intentionally syntactic (as in
;;; OCaml's value-restriction implementation): constructor applications over
;;; non-expansive arguments are non-expansive; ordinary function applications
;;; are treated as expansive.
;;;
;;; References:
;;;   - Andrew K. Wright, "Simple Imperative Polymorphism" (1995)
;;;   - Jacques Garrigue, "Relaxing the Value Restriction" (2004)
;;;   - OCaml manual section "Polymorphism and its limitations"
;;; ---------------------------------------------------------------------------

(defun binding-init-expression (binding)
  "Return BINDING's initializer expression used for value-restriction checks.

For normal `let` bindings this is the right-hand side expression.
For function-shorthand bindings and explicit multi-form bodies there is no
single initializer expression, and this function returns NIL."
  (declare (type (or parser:toplevel-define parser:node-let-binding parser:instance-method-definition) binding)
           (values (or null parser:node) &optional))
  (etypecase binding
    (parser:node-let-binding
      (parser:node-let-binding-value binding))
    (parser:toplevel-define
      (let ((body (parser:toplevel-define-body binding)))
        (when (null (parser:node-body-nodes body))
          (parser:node-body-last-node body))))
    (parser:instance-method-definition
      (let ((body (parser:instance-method-definition-body binding)))
        (when (null (parser:node-body-nodes body))
          (parser:node-body-last-node body))))))

(defun nonexpansive-expression-p (node env)
  "Return T when NODE is syntactically non-expansive.

In this context:
  - non-expansive means an expression that is value-like and can be
    safely generalized;
  - expansive means an expression that may allocate/effect and therefore
    should not be generalized.

Constructor applications are treated as non-expansive only when:
  - the rator resolves to a data constructor;
  - the application is not over-applied; and
  - every argument is itself non-expansive.

All other applications are considered expansive. This is conservative but
matches the value-restriction safety argument."
  (declare (type parser:node node)
           (type tc:environment env)
           (values boolean &optional))
  (typecase node
    ((or parser:node-variable
         parser:node-literal
         parser:node-integer-literal
         parser:node-abstraction)
     t)
    (parser:node-the
      (nonexpansive-expression-p (parser:node-the-expr node) env))
    (parser:node-application
      (and (typep (parser:node-application-rator node) 'parser:node-variable)
           (let* ((name (parser:node-variable-name (parser:node-application-rator node)))
                  (ctor (tc:lookup-constructor env name :no-error t))
                  (rands (parser:node-application-rands node)))
             (and ctor
                  (<= (length rands) (tc:constructor-entry-arity ctor))
                  (every (lambda (rand)
                           (nonexpansive-expression-p rand env))
                         rands)))))
    (t nil)))

(defun binding-nonexpansive-p (binding env)
  "Return T when BINDING is eligible for generalization.

Function bindings are always non-expansive because they denote lambdas.
Non-function bindings are eligible only when their initializer is
non-expansive."
  (declare (type (or parser:toplevel-define parser:node-let-binding parser:instance-method-definition) binding)
           (type tc:environment env)
           (values boolean &optional))
  (or (parser:binding-function-p binding)
      (let ((init-expr (binding-init-expression binding)))
        (and init-expr
             (nonexpansive-expression-p init-expr env)))))

(defun weak-binding-type-variables (bindings expr-tys env)
  "Collect weak-variable candidates for this binding group.

Each expansive binding contributes its inferred expression type variables.
Those variables may unify with concrete types later. Covariant occurrences may
still be generalized by the relaxed value restriction."
  (declare (type list bindings)
           (type tc:ty-list expr-tys)
           (type tc:environment env)
           (values tc:tyvar-list &optional))
  (remove-duplicates
   (loop :for binding :in bindings
         :for expr-ty :in expr-tys
         :unless (binding-nonexpansive-p binding env)
           :append (tc:type-variables expr-ty))
   :test #'tc:ty=))

(defun blocked-weak-type-variables (weak-tvars expr-tys retained-preds env)
  "Return weak variables that must remain monomorphic.

Weak variables are blocked from generalization when they are not covariant in
the inferred expression types, or when they appear in retained predicates.

Variance comes from constructor metadata in ENV. Unknown or opaque constructors
fall back to invariant, which is conservative."
  (declare (type tc:tyvar-list weak-tvars)
           (type tc:ty-list expr-tys)
           (type tc:ty-predicate-list retained-preds)
           (type tc:environment env)
           (values tc:tyvar-list &optional))
  (let* ((resolver (tc:make-env-variance-resolver env))
         (variance-table
           (tc:collect-tyvar-variances expr-tys resolver))
         (retained-tvars
           (tc:type-variables retained-preds)))
    (remove-duplicates
     (loop :for weak-var :in weak-tvars
           :for observed-variance := (tc:tyvar-variance variance-table weak-var)
           :when (or (find weak-var retained-tvars :test #'tc:ty=)
                     (not (tc:variance-covariant-p observed-variance)))
             :collect weak-var)
     :test #'tc:ty=)))

(defun error-non-generalizable-binding (binding scheme)
  "Signal a user-facing error for a top-level weak (non-generalizable) type.

At top level we reject unresolved weak variables in inferred schemes instead of
printing an implicit weak variable notation."
  (declare (type (or parser:toplevel-define parser:node-let-binding) binding)
           (type tc:ty-scheme scheme))
  (tc-error "Type is not generalizable"
            (tc-note (parser:binding-name binding)
                     "Inferred type ~S cannot be generalized because this binding is expansive."
                     scheme)
            (tc-note (parser:binding-name binding)
                     "Hint: move this allocation into a function body (eta-expand) for fresh state per call, or add an explicit type declaration if this binding should be monomorphic.")))

(defun infer-let-bindings (bindings declares subs env)
  (declare (type parser:node-let-binding-list bindings)
           (type parser:node-let-declare-list declares)
           (type tc:substitution-list subs)
           (type tc-env env)
           (values tc:ty-predicate-list accessor-list (or toplevel-define-list node-let-binding-list) tc:substitution-list &optional))

  (let ((def-table
          (make-hash-table :test #'eq))
        (dec-table
          (make-hash-table :test #'eq)))
    ;; Ensure that there are no duplicate definitions
    (loop :for binding :in bindings
          :for name := (parser:node-variable-name (parser:node-let-binding-name binding))

          :if (gethash name def-table)
            :do (tc-error "Duplicate binding in let"
                          (tc-note (parser:node-let-binding-name binding)
                                   "second definition here")
                          (tc-note (parser:node-let-binding-name
                                    (gethash name def-table))
                                   "first definition here"))
          :else
            :do (setf (gethash name def-table) binding))

    ;; Ensure that there are no duplicate declarations
    (loop :for declare :in declares
          :for name := (parser:node-variable-name (parser:node-let-declare-name declare))

          :if (gethash name dec-table)
            :do (tc-error "Duplicate declaration in let"
                          (tc-note (parser:node-let-declare-name declare)
                                   "second declaration here")
                          (tc-note (parser:node-let-declare-name
                                    (gethash name dec-table))
                                   "first declaration here"))
          :else
            :do (setf (gethash name dec-table) declare))

    ;; Ensure that each declaration has an associated definition
    (loop :for declare :in declares
          :for name := (parser:node-variable-name (parser:node-let-declare-name declare))

          :unless (gethash name def-table)
            :do (tc-error "Orphan declare in let"
                          (tc-note (parser:node-let-declare-name declare)
                                   "declaration does not have an associated definition")))

    (let ((dec-table
            (loop :with table := (make-hash-table :test #'eq)
                  :for declare :in declares
                  :for name := (parser:node-variable-name (parser:node-let-declare-name declare))
                  :do (setf (gethash name table) (parser:node-let-declare-type declare))
                  :finally (return table))))

      (infer-bindings-type bindings dec-table subs env))))


(defun infer-bindings-type (bindings dec-table subs env)
  (declare (type list bindings)
           (type hash-table dec-table)
           (type tc:substitution-list subs)
           (type tc-env env)
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

        :for scheme := (parse-ty-scheme unparsed-ty (tc-env-env env))
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
           (loop :for scc :in sccs
                 :for bindings
                   := (loop :for name :in scc
                            :collect (gethash name impl-bindings))
                 :append (multiple-value-bind (preds_ nodes subs_)
                             (infer-impls-binding-type bindings subs env)
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
                              (infer-expl-binding-type binding
                                                       scheme
                                                       (source:location
                                                        (parser:binding-name binding))
                                                       subs
                                                       env)
                            (setf subs subs_)
                            (setf preds (append preds preds_))
                            node_))))
    (values preds
            nil
            (append impl-binding-nodes expl-binding-nodes)
            subs)))

(defun infer-expl-binding-type (binding declared-ty location subs env)
  "Infer the type of BINDING and then ensure it matches DECLARED-TY."
  (declare (type (or parser:toplevel-define parser:node-let-binding parser:instance-method-definition) binding)
           (type tc:ty-scheme declared-ty)
           (type source:location location)
           (type tc:substitution-list subs)
           (type tc-env env)
           (values tc:ty-predicate-list
                   (or toplevel-define node-let-binding instance-method-definition)
                   tc:substitution-list
                   &optional))
  
  ;; HACK: recursive scc checking on instances is too strict
  (unless (typep binding 'parser:instance-method-definition)
    (check-for-invalid-recursive-scc (list binding) (tc-env-env env)))

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
         env)

      (tc:apply-substitution subs env)

      (setf accessors (tc:apply-substitution subs accessors))

      (multiple-value-bind (accessors subs_)
          (solve-accessors accessors (tc-env-env env))
        (setf subs (tc:compose-substitution-lists subs subs_))

        (when accessors
          (tc-error "Ambiguous accessor"
                    (tc-note (first accessors)
                             "accessor is ambiguous")))

        ;; Generate additional substitutions from fundeps.
        ;; The purpose of this block is primarily to effect
        ;; the inheritance of functional dependencies.
        ;; For example, if we have
        ;;   (define-class (C :a :b (:a -> :b)))
        ;;   (define-class (C :a :b => D :a :b))
        ;; and a list of predicates ((D #T1 #T2) (C #T1 #T3)), then
        ;; the following lines will ensure that #T2 and #T3 are unified.
        (setf fresh-preds (tc:apply-substitution subs fresh-preds))
        (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) fresh-preds subs)))
        (setf preds (tc:apply-substitution subs preds))
        (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) preds subs)))

        (let* ((expr-type (tc:apply-substitution subs fresh-type))
               (expr-preds (tc:apply-substitution subs fresh-preds))

               (env-tvars (tc-env-bindings-variables env bound-variables))
               (local-tvars (set-difference (remove-duplicates
                                             (append (tc:type-variables expr-type)
                                                     (tc:type-variables expr-preds))
                                             :test #'tc:ty=)
                                            env-tvars
                                            :test #'tc:ty=))

               (output-qual-type (tc:qualify expr-preds expr-type))
               (output-scheme (tc:quantify local-tvars output-qual-type)))

          (let* ((expr-preds (tc:apply-substitution subs expr-preds))
                 (preds (tc:apply-substitution subs preds))
                 (subs subs)

                 ;; Known tvars for fundep-entail:
                 ;; 1. Tvars that appear in the binding's type signature
                 ;; 2. Tvars that have been fixed by the surrounding environment
                 (known-variables
                   (remove-if-not
                    #'tc:tyvar-p
                    (tc:apply-substitution
                     subs
                     (append
                      (remove-duplicates (tc:type-variables expr-type) :test #'tc:ty=)
                      env-tvars)))))

            ;; This loop repeats fundep-entail until no new substitutions are found
            ;; (or it hits the configured max-fundep depth). This allows the typechecker
            ;; to follow chained fundeps, where:
            ;;   (define-class (A :b :a (:b -> :a)))
            ;;   (define-class (B :c :b (:c -> :b)))
            ;;   ...
            ;; This is necessary for cases where dependent tvars only occur in the constraint:
            ;; (declare function ((B :c :b) (A :b :a) => Unit -> :c))
            (loop :for i :below tc:+fundep-max-depth+
                  :for new-subs := (tc:fundep-entail (tc-env-env env)
                                                     expr-preds
                                                     preds
                                                     known-variables)
                  :do (when (endp new-subs)
                        (return))

                      (setf subs (tc:compose-substitution-lists new-subs subs))
                      (setf expr-preds (tc:apply-substitution new-subs expr-preds))
                      (setf preds (tc:apply-substitution new-subs preds))

                      ;; Promote newly-determined variables for downstream fundep-entail calls,
                      ;; so that tvars resulting from new substitutions become 'known' on the next pass.
                      (let ((range-tvars
                              (remove-duplicates
                               (alexandria:mappend #'tc:type-variables
                                                   (mapcar #'tc:substitution-to new-subs))
                               :test #'tc:ty=)))
                        (setf known-variables
                              (remove-duplicates
                               (append
                                (remove-if-not #'tc:tyvar-p
                                               (tc:apply-substitution new-subs known-variables))
                                range-tvars)
                               :test #'tc:ty=)))
                  :finally (util:coalton-bug "Fundep chain failed to converge"))

            (setf preds
                  (remove-if
                   (lambda (p) (tc:entail (tc-env-env env) expr-preds p))
                   preds))

            (setf local-tvars
                  (expand-local-tvars env-tvars
                                      local-tvars
                                      preds
                                      (tc-env-env env)))
            (setf env-tvars
                  (expand-local-tvars local-tvars
                                      (tc:type-variables
                                       (tc:apply-substitution subs env-tvars))
                                      preds
                                      (tc-env-env env)))

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
                           (error-ambiguous-pred (tc:ambiguous-constraint-pred e)))))

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
                  (error-unknown-pred (first deferred-preds)))

                ;; Check that the declared and inferred schemes match
                (unless (equalp declared-ty output-scheme)
                  (tc-error "Declared type is too general"
                            (tc-location location
                                         "Declared type ~S is more general than inferred type ~S."
                                         declared-ty
                                         output-scheme)))

                ;; Check for undeclared predicates
                (when (not (null retained-preds))
                  (tc-error "Explicit type is missing inferred predicate"
                            (tc-location location
                                         "Declared type ~S is missing inferred predicate ~S"
                                         output-qual-type
                                         (first retained-preds))))

                (values deferred-preds
                        (attach-explicit-binding-type
                         (tc:apply-substitution subs binding-node)
                         (tc:apply-substitution subs fresh-qual-type))
                        subs)))))))))

(defun check-for-invalid-recursive-scc (bindings env)
  (declare (type (or parser:toplevel-define-list
                     parser:node-let-binding-list
                     parser:instance-method-definition-list)
                 bindings)
           (type tc:environment env))

  (assert bindings)

  ;; If all bindings are functions then the scc is valid
  (when (every #'parser:binding-function-p bindings)
    (return-from check-for-invalid-recursive-scc))

  ;; If some bindings are functions and some are not then the scc is invalid
  (when (and (some (alexandria:compose #'not #'parser:binding-function-p) bindings)
             (some #'parser:binding-function-p bindings))

    (let ((first-fn (find-if #'parser:binding-function-p bindings)))
      (assert first-fn)

      (apply #'tc-error
             "Invalid recursive bindings"
             (tc-note (parser:binding-name first-fn)
                      "function can not be defined recursively with variables")
             (loop :for binding :in (remove first-fn bindings :test #'eq)
                   :collect (tc-secondary-note (parser:binding-name binding)
                                               "with definition")))))

  ;; If there is a single non-recursive binding then it is valid
  (when (and (= 1 (length bindings))
             (not (member (parser:node-variable-name (parser:binding-name (first bindings)))
                          (parser:collect-variables (parser:binding-value (first bindings)))
                          :key #'parser:node-variable-name
                          :test #'eq)))
    (return-from check-for-invalid-recursive-scc))

  ;; Toplevel bindings cannot be recursive values
  (when (parser:binding-toplevel-p (first bindings))
    (apply #'tc-error
           "Invalid recursive bindings"
           (tc-note (parser:binding-name (first bindings))
                    "invalid recursive variable bindings")
           (loop :for binding :in (rest bindings)
                 :collect (tc-secondary-note (parser:binding-name binding)
                                             "with definition"))))

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

      (apply #'tc-error "Invalid recursive bindings"
             (tc-note (parser:binding-name (first bindings))
                      "invalid recursive variable bindings")
             (loop :for binding :in (rest bindings)
                   :collect (tc-note (parser:binding-name binding) "with definition"))))))

(defun infer-impls-binding-type (bindings subs env)
  "Infer the type's of BINDINGS and then qualify those types into schemes."
  (declare (type (or parser:toplevel-define-list parser:node-let-binding-list) bindings)
           (type tc:substitution-list subs)
           (type tc-env env)
           (values tc:ty-predicate-list (or toplevel-define-list node-let-binding-list) tc:substitution-list &optional))

  (check-for-invalid-recursive-scc bindings (tc-env-env env))

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
                              (infer-binding-type binding ty subs env)
                            (setf subs subs_)
                            (setf preds (append preds preds_))
                            (setf accessors (append accessors accessors_))
                            node_))))

    (tc:apply-substitution subs env)

    (setf accessors (tc:apply-substitution subs accessors))

    (multiple-value-bind (accessors subs_)
        (solve-accessors accessors (tc-env-env env))
      (setf subs (tc:compose-substitution-lists subs subs_))

      (when accessors
        (tc-error "Ambiguous accessor"
                  (tc-note (first accessors) "accessor is ambiguous")))

      (let* ((expr-tys
               (tc:apply-substitution subs expr-tys))
             (env-tvars
               (tc-env-bindings-variables env bound-variables))
             (expr-tvars
               (remove-duplicates (tc:type-variables expr-tys) :test #'eq))
             (local-tvars
               (set-difference expr-tvars env-tvars :test #'eq))
             (weak-tvars
               (intersection
                (weak-binding-type-variables bindings expr-tys (tc-env-env env))
                local-tvars
                :test #'tc:ty=)))

        (setf preds (tc:apply-substitution subs preds))

        ;; Generate additional substitutions from fundeps
        ;; This effects the inheritance of functional dependencies
        ;; from superclasses and reduces generality with respect to
        ;; instances defined in the environment.
        (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) preds subs)))

        (setf preds (tc:apply-substitution subs preds))
        (setf local-tvars
              (expand-local-tvars env-tvars
                                  local-tvars
                                  preds
                                  (tc-env-env env)))

        (setf env-tvars
              (expand-local-tvars local-tvars
                                  (tc:type-variables
                                   (tc:apply-substitution subs env-tvars))
                                  preds
                                  (tc-env-env env)))

        (multiple-value-bind (deferred-preds retained-preds)
            (tc:split-context (tc-env-env env) env-tvars preds subs)

          (let* ((defaultable-preds (handler-case
                                        (tc:default-preds (tc-env-env env) (append env-tvars local-tvars) retained-preds)
                                      (tc:coalton-internal-type-error (e)
                                        (error-ambiguous-pred (tc:ambiguous-constraint-pred e)))))

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

            (let* ((generalizable-candidates
                     (remove-if-not
                      #'tc:tyvar-p
                      (tc:type-variables (tc:apply-substitution subs local-tvars))))
                   (blocked-weak-tvars
                     (intersection
                      (blocked-weak-type-variables
                       (remove-if-not #'tc:tyvar-p
                                      (tc:apply-substitution subs weak-tvars))
                       (tc:apply-substitution subs expr-tys)
                       (tc:apply-substitution subs retained-preds)
                       (tc-env-env env))
                      generalizable-candidates
                      :test #'tc:ty=))
                   (generalizable-tvars
                     (set-difference
                      generalizable-candidates
                      ;; Weak variables with non-covariant occurrences remain
                      ;; monomorphic placeholders until solved.
                      blocked-weak-tvars
                      :test #'tc:ty=)))

              (if restricted
                  (let* ((allowed-tvars (set-difference generalizable-tvars
                                                        (tc:type-variables retained-preds)
                                                        :test #'tc:ty=))

                         (output-qual-tys
                           (loop :for ty :in expr-tys
                                 :collect (tc:apply-substitution subs (tc:make-qualified-ty :predicates nil :type ty))))

                         (output-schemes
                           (loop :for ty :in output-qual-tys
                                 :collect (tc:quantify allowed-tvars ty)))

                         (deferred-preds (append deferred-preds retained-preds)))

                    (when (parser:binding-toplevel-p (first bindings))
                      (loop :for scheme :in output-schemes
                            :for binding :in bindings
                            :when (tc:type-variables scheme)
                              :do (error-non-generalizable-binding binding scheme)))

                    (loop :for scheme :in output-schemes
                          :for binding :in bindings

                          :for name := (parser:node-variable-name (parser:binding-name binding))

                          :do (tc-env-replace-type env name scheme))

                    (when (and (parser:binding-toplevel-p (first bindings)) deferred-preds)
                      (error-unknown-pred (first deferred-preds)))

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
                                 :collect (tc:quantify generalizable-tvars ty)))

                         (rewrite-table
                           (loop :with table := (make-hash-table :test #'eq)

                                 :for ty :in output-qual-tys
                                 :for binding :in bindings

                                 :for name := (parser:node-variable-name (parser:binding-name binding))
                                 :do (setf (gethash name table) ty)

                                 :finally (return table))))

                    (when (parser:binding-toplevel-p (first bindings))
                      (loop :for scheme :in output-schemes
                            :for binding :in bindings
                            :when (tc:type-variables scheme)
                              :do (error-non-generalizable-binding binding scheme)))

                    (loop :for scheme :in output-schemes
                          :for binding :in bindings

                          :for name := (parser:node-variable-name (parser:binding-name binding))

                          :do (tc-env-replace-type env name scheme))

                    (when (and (parser:binding-toplevel-p (first bindings)) deferred-preds)
                      (error-ambiguous-pred (first deferred-preds)))

                    (values
                     deferred-preds
                     (loop :for binding :in binding-nodes
                           :for ty :in output-qual-tys
                           :collect (rewrite-recursive-calls
                                     (tc:apply-substitution subs (attach-explicit-binding-type binding ty))
                                     rewrite-table))
                     subs))))))))))

(defun infer-binding-type (binding expected-type subs env)
  "Infer the type of BINDING then unify against EXPECTED-TYPE. Adds BINDING's parameters to the environment."
  (declare (type (or parser:toplevel-define parser:node-let-binding parser:instance-method-definition) binding)
           (type tc:ty expected-type)
           (type tc:substitution-list subs)
           (values tc:ty-predicate-list accessor-list (or toplevel-define node-let-binding instance-method-definition) tc:substitution-list))

  (check-duplicates
   (parser:pattern-variables (parser:binding-parameters binding))
   #'parser:pattern-var-name
   (lambda (first second)
     (tc-error "Duplicate parameters name"
               (tc-note first "first parameter here")
               (tc-note second "second parameter here"))))

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
                                    (infer-pattern-type pattern ty subs env)
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
                                            env)
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
                               (tc:coalton-internal-type-error ()
                                 (tc-error "Return type mismatch"
                                           (tc-location s1
                                                        "First return is of type '~S'"
                                                        (tc:apply-substitution subs ty1))
                                           (tc-location s2
                                                        "Second return is of type '~S'"
                                                        (tc:apply-substitution subs ty2))))))

                   ;; Unify the function's inferred type with one of the early returns.
                   (when *returns*
                     (handler-case
                         (setf subs (tc:unify subs (cdr (first *returns*)) ret-ty))
                       (tc:coalton-internal-type-error ()
                         (tc-error "Return type mismatch"
                                   (tc-location (car (first *returns*))
                                                "First return is of type '~S'"
                                                (tc:apply-substitution subs (cdr (first *returns*))))
                                   (tc-note (parser:binding-last-node binding)
                                            "Second return is of type '~S'"
                                            (tc:apply-substitution subs ret-ty))))))

                   value-node))

               ;; If the binding does not have parameters that just infer the binding's type
               (multiple-value-bind (ty_ preds_ accessors_ value-node subs_)
                   (infer-expression-type (parser:binding-value binding)
                                          ret-ty
                                          subs
                                          env)
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
                      :location (source:location (parser:binding-name binding))
                      :name (parser:node-variable-name (parser:binding-name binding))))

                   (typed-binding (build-typed-binding binding name-node value-node params)))
              (values
               preds
               accessors
               typed-binding
               subs)))
        (tc:coalton-internal-type-error ()
          (tc-error "Type mismatch"
                    (tc-note binding "Expected type '~S' but got type '~S'"
                             (tc:apply-substitution subs expected-type)
                             (tc:apply-substitution subs ty))))))))

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
     :location (source:location binding)))

  (:method ((binding parser:node-let-binding) name value params)
    (declare (type node-variable name)
             (type node value)
             (type pattern-list params)
             (values node-let-binding))

    (assert (null params))

    (make-node-let-binding
     :name name
     :value value
     :location (source:location binding)))

  (:method ((binding parser:instance-method-definition) name value params)
    (declare (type node-variable name)
             (type node-body value)
             (type pattern-list params)
             (values instance-method-definition))

    (make-instance-method-definition
     :name name
     :params params
     :body value
     :location (source:location binding))))

(defgeneric attach-explicit-binding-type (binding explicit-type)
  (:method ((binding toplevel-define) explicit-type)
    (declare (type tc:qualified-ty explicit-type)
             (values toplevel-define))

   (make-toplevel-define
    :name (make-node-variable
           :name (node-variable-name (toplevel-define-name binding))
           :type explicit-type
           :location (source:location (toplevel-define-name binding)))
    :params (toplevel-define-params binding)
    :body (toplevel-define-body binding)
    :location (source:location binding)))

  (:method ((binding node-let-binding) explicit-type)
    (declare (type tc:qualified-ty explicit-type)
             (values node-let-binding))

   (make-node-let-binding
    :name (make-node-variable
           :name (node-variable-name (node-let-binding-name binding))
           :type explicit-type
           :location (source:location (node-let-binding-name binding)))
    :value (node-let-binding-value binding)
    :location (source:location binding)))

  (:method ((binding instance-method-definition) explicit-type)
    (declare (type tc:qualified-ty explicit-type)
             (values instance-method-definition))

    (make-instance-method-definition
     :name (make-node-variable
            :name (node-variable-name (instance-method-definition-name binding))
            :type explicit-type
            :location (source:location (instance-method-definition-name binding)))
     :params (instance-method-definition-params binding)
     :body (instance-method-definition-body binding)
     :location (source:location binding))))

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
                  :location (source:location node)
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
        :location (source:location binding)))

      (node-let-binding
       (make-node-let-binding
        :name (node-let-binding-name binding)
        :value (traverse
                (node-let-binding-value binding)
                (make-traverse-block
                 :variable #'rewrite-variable-ref))
        :location (source:location binding))))))

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
  "Expand LOCAL-TVARS over the functional dependencies taken from PREDS
and return the set difference of the expansion and ENV-TVARS."
  (let* ((fundeps (tc:collect-fundep-vars env preds))
         (expansion (tc:generic-closure local-tvars fundeps :test #'tc:ty=))
         (expansion (remove-duplicates expansion :test #'tc:ty=)))
    (set-difference expansion env-tvars :test #'tc:ty=)))
