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
   #:coalton-impl/typechecker/ast
   #:coalton-impl/typechecker/tc-env)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:parser #:coalton-impl/parser)
   (#:error #:coalton-impl/error)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:toplevel-define                    ; STRUCT
   #:make-toplevel-define               ; CONSTRUCTOR
   #:toplevel-define-name               ; ACCESSOR
   #:toplevel-define-vars               ; ACCESSOR
   #:toplevel-define-body               ; ACCESSOR
   #:toplevel-define-explicit-type      ; ACCESSOR
   #:toplevel-define-source             ; ACCESSOR
   #:toplevel-define-monomorphize       ; ACCESSOR
   #:toplevel-define-list               ; TYPE

   #:toplevel-define                    ; FUNCTION
   ))

;; TODO: ensure patterns don't bind the same variables multiple times

(in-package #:coalton-impl/typechecker/define)

(declaim (type (member :toplevel :lambda) *return-status*))
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

(defun error-ambigious-pred (pred file)
  (declare (type tc:ty-predicate pred)
           (type coalton-file file))

  (unless (tc:ty-predicate-source pred)
    (util:coalton-bug "Predicate ~A does not have source information" pred))

  (error 'tc-error
         :err (coalton-error
               :span (tc:ty-predicate-source pred)
               :file file
               :message "Ambigious predicate"
               :primary-note (format nil "Ambigious predicate ~A" pred))))


(defstruct (toplevel-define
            (:copier nil))
  (name          (util:required 'name)          :type node-variable             :read-only t)
  (vars          (util:required 'vars)          :type node-variable-list        :read-only t)
  (body          (util:required 'body)          :type node-body                 :read-only t)
  (explicit-type (util:required 'explicit-type) :type (or null tc:qualified-ty) :read-only t)
  (source        (util:required 'source)        :type cons                      :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-define-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-define-p x))))

(deftype toplevel-define-list ()
  '(satisfies toplevel-define-list-p))

(defmethod tc:apply-substitution (subs (node toplevel-define))
  (declare (type tc:substitution-list subs)
           (values toplevel-define &optional))
  (make-toplevel-define
   :name (tc:apply-substitution subs (toplevel-define-name node))
   :vars (tc:apply-substitution subs (toplevel-define-vars node))
   :body (tc:apply-substitution subs (toplevel-define-body node))
   :explicit-type (tc:apply-substitution subs (toplevel-define-explicit-type node))
   :source (toplevel-define-source node)))

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
                    :message "second defintion here"))))))

  ;; Ensure that there are no duplicate declerations
  (check-duplicates
   declares
   (alexandria:compose #'parser:identifier-src-name #'parser:toplevel-declare-name)
   (lambda (first second)
     (error 'tc-error
            :err (coalton-error
                  :span (parser:identifier-src-source (parser:toplevel-declare-name first))
                  :file file
                  :message "Duplicate decleration"
                  :primary-note "first decleration here"
                  :notes
                  (list
                   (make-coalton-error-note
                    :type :primary
                    :span (parser:identifier-src-source (parser:toplevel-declare-name second))
                    :message "second decleration here"))))))

  ;; Ensure that each decleration has an associated definition
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
                           :message "Orphan decleration"
                           :primary-note "decleration does not have an associated definition")))

  (let ((dec-table (make-hash-table :test #'eq))

        (tc-env (make-tc-env :env env)))

    (loop :for declare :in declares
          :for name := (parser:identifier-src-name (parser:toplevel-declare-name declare))
          :for ty := (parser:toplevel-declare-type declare)
          :do (setf (gethash name dec-table) ty))

    ;; Infer binding types, returning the typed nodes.
    (multiple-value-bind (preds binding-nodes subs)
        (infer-bindings-type defines dec-table nil tc-env file)
      (assert (null preds))
      
      (let (;; Attach explicit types to any explicit bindings

            ;; TODO: Do we need to unify these so that type variables line up for codegen?
            (binding-nodes
              (loop :for node :in binding-nodes
                    :for explicit-type := (gethash (toplevel-define-name node) dec-table)
                    :if explicit-type
                      :collect (attach-explicit-binding-type node explicit-type)
                    :else
                      :collect node)))

        (loop :for define :in defines
              :for name := (parser:node-variable-name (parser:name define))
              :for scheme := (tc:remove-source-info (gethash name (tc-env-ty-table tc-env)))

              :when (tc:type-variables scheme)
                :do (util:coalton-bug "Scheme ~A should not have any free type variables." scheme)

              :do (setf env (tc:set-value-type env name scheme))

                  ;; TODO: set function source parameter names

                  (setf env (tc:set-name env name (tc:make-name-entry
                                                   :name name
                                                   :type :value
                                                   ;; TOOD: add docstring here
                                                   :docstring nil
                                                   :location (parser:coalton-file-name file)))))

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
             (values tc:ty tc:ty-predicate-list node-literal tc:substitution-list))

    (let ((ty (etypecase (parser:node-literal-value node)
                (ratio tc:*fraction-type*)
                (single-float tc:*single-float-type*)
                (double-float tc:*double-float-type*)
                (string tc:*string-type*)
                (character tc:*char-type*))))

      ;; TODO: this error message isn't very helpful
      (handler-case
          (progn
            (setf subs (tc:unify subs ty expected-type))
            (let ((type (tc:apply-substitution subs ty)))
              (values
               type
               nil
               (make-node-literal
                :type (tc:qualify nil type)
                :source (parser:node-source node)
                :value (parser:node-literal-value node))
               subs)))
        (error:coalton-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:node-source node)
                       :file file
                       :message "Type mismatch"
                       :primary-note (format nil "Expected type '~A' but got type '~A'"
                                             (tc:apply-substitution subs expected-type)
                                             (tc:apply-substitution subs ty))))))))

  (:method ((node parser:node-integer-literal) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list node-integer-literal tc:substitution-list))

    (let* ((classes-package (find-package "COALTON-LIBRARY/CLASSES"))

           (num (find-symbol "NUM" classes-package))

           (tvar (tc:make-variable))

           (pred (tc:make-ty-predicate :class num :types (list tvar) :source (parser:node-source node))))

      ;; TODO: error out here better if NUM is not defined

      (setf (gethash tvar (tc-env-var-table env)) node)

      (handler-case
          (progn
            (setf subs (tc:unify subs tvar expected-type))
            (let ((type (tc:apply-substitution subs tvar)))
              (values
               type
               (list pred)
               (make-node-integer-literal
                :type (tc:qualify (list pred) type)
                :source (parser:node-source node)
                :value (parser:node-integer-literal-value node))
               subs)))
        (error:coalton-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:node-source node)
                       :file file
                       :message "Type mismatch"
                       :primary-note (format nil "Expected type '~A' but got type '~A'"
                                             (tc:apply-substitution subs expected-type)
                                             (tc:apply-substitution subs tvar))))))))

  (:method ((node parser:node-variable) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list node-variable tc:substitution-list))

    (multiple-value-bind (ty preds)
        (tc-env-lookup-value env node file)

      (handler-case
          (progn
            (setf subs (tc:unify subs ty expected-type))
            (let ((type (tc:apply-substitution subs ty)))
              (values
               type
               preds
               (make-node-variable
                :type (tc:qualify preds type)
                :source (parser:node-source node)
                :name (parser:node-variable-name node))
               subs)))
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
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list node-application tc:substitution-list))

    ;; TODO: lol lmao
    (when (null (parser:node-application-rands node))
      (error ""))

    (multiple-value-bind (fun-ty preds rator-node subs)
        (infer-expression-type (parser:node-application-rator node)
                               (tc:make-variable)
                               subs
                               env
                               file)

      (let* ((fun-ty_ fun-ty)
             (rand-nodes
               ;; Apply arguments one at a time for better error messages
               (loop :for rand :in (parser:node-application-rands node)
                     :collect (cond
                                ;; If the rator is a function then unify against its argument
                                ((tc:function-type-p fun-ty_)
                                 (multiple-value-bind (ty_ preds_ node_ subs_)
                                     (infer-expression-type rand
                                                            (tc:function-type-from fun-ty_)
                                                            subs
                                                            env
                                                            file)
                                   (declare (ignore ty_))
                                   (setf preds (append preds preds_))
                                   (setf subs subs_)
                                   (setf fun-ty_ (tc:function-type-to fun-ty_))

                                   node_))

                                ;; If the rator is variable then unify against a new function type
                                ((tc:tyvar-p fun-ty_)
                                 (let* ((new-from (tc:make-variable))

                                        (new-to (tc:make-variable))

                                        (new-ty (tc:make-function-type new-from new-to)))

                                   (setf subs (tc:unify subs fun-ty_ new-ty))
                                   (multiple-value-bind (ty_ preds_ node_ subs_)
                                       (infer-expression-type rand
                                                              new-from
                                                              subs
                                                              env
                                                              file)
                                     (declare (ignore ty_))
                                     (setf preds (append preds preds_))
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
                                              :primary-note (format nil "Function has ~D arguments but inferred type '~A' only takes ~D"
                                                                    (length (parser:node-application-rands node))
                                                                    fun-ty
                                                                    (length (tc:function-type-arguments fun-ty))))))))))

        (handler-case
            (progn
              (setf subs (tc:unify subs fun-ty_ expected-type))
              (let ((type (tc:apply-substitution subs fun-ty_)))
                (values
                 type
                 preds
                 (make-node-application
                  :type (tc:qualify nil type)
                  :source (parser:node-source node)
                  :rator rator-node
                  :rands rand-nodes)
                 subs)))
          (error:coalton-type-error ()
            (error 'tc-error
                   :err (coalton-error
                         :span (parser:node-source node)
                         :file file
                         :message "Type mismatch"
                         :primary-note (format nil "Expected type '~A' but got type '~A'"
                                               (tc:apply-substitution subs expected-type)
                                               (tc:apply-substitution subs fun-ty_)))))))))

  (:method ((node parser:node-bind) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values null tc:ty-predicate-list node-bind tc:substitution-list))

    (multiple-value-bind (expr-ty preds expr-node subs)
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
             (values tc:ty tc:ty-predicate-list node-body tc:substitution-list))

    (let* ((preds nil)

           ;; Infer the type of each node
           (body-nodes
             (loop :for node_ :in (parser:node-body-nodes node)
                   :collect (multiple-value-bind (node_ty_ preds_ node_ subs_)
                                (infer-expression-type node_ (tc:make-variable) subs env file)
                              (declare (ignore node_ty_))
                              (setf subs subs_)
                              (setf preds (append preds preds_))
                              node_))))

      (multiple-value-bind (ty preds_ last-node subs)
          (infer-expression-type (parser:node-body-last-node node) expected-type subs env file)
        (setf preds (append preds preds_))

        (values
         ty
         preds
         (make-node-body
          :nodes body-nodes
          :last-node last-node)
         subs))))

  (:method ((node parser:node-abstraction) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list node-abstraction tc:substitution-list))

    (let (;; Setup return environment
          (*return-status* :lambda)
          (*returns* nil)

          ;; Add parameters to the environment
          (arg-tys
            (loop :for var :in (parser:node-abstraction-vars node)
                  :collect (tc-env-add-variable env (parser:node-variable-name var)))))

      (multiple-value-bind (body-ty preds body-node subs)
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
                    (error:coalton-type-error ()
                      (error 'tc-error
                             :err (coalton-error
                                   :span s1
                                   :file file
                                   :message "Return type mismatch"
                                   :primary-note (format nil "First return is of type '~A'"
                                                         (tc:apply-substitution subs ty1))
                                   :notes
                                   (list
                                    (make-coalton-error-note
                                     :type :primary
                                     :span s2
                                     :message (format nil "Second return is of type '~A'"
                                                      (tc:apply-substitution subs ty2)))))))))

        ;; Unify the function's inferered type with one of the early returns.
        (when *returns*
          (handler-case
              (setf subs (tc:unify subs (cdr (first *returns*)) body-ty))
            (error:coalton-type-error ()
              (error 'tc-error
                     :err (coalton-error
                           :span (car (first *returns*))
                           :file file
                           :message "Return type mismatch"
                           :primary-note (format nil "First return is of type '~A'"
                                                 (tc:apply-substitution subs (cdr (first *returns*))))
                           :notes
                           (list
                            (make-coalton-error-note
                             :type :primary
                             :span (parser:node-source (parser:node-body-last-node (parser:node-abstraction-body node)))
                             :message (format nil "Second return is of type '~A'"
                                              (tc:apply-substitution subs body-ty)))))))))

        (let ((ty (tc:make-function-type* arg-tys body-ty)))
          (handler-case
              (progn
                (setf subs (tc:unify subs ty expected-type))
                (let ((type (tc:apply-substitution subs ty))
                      
                      (var-nodes
                        (loop :for var :in (parser:node-abstraction-vars node)
                              :for arg-ty :in arg-tys
                              :collect (make-node-variable
                                        :type (tc:qualify nil (tc:apply-substitution subs arg-ty))
                                        :source (parser:node-source var)
                                        :name (parser:node-variable-name var)))))
                  (values
                   type
                   preds
                   (make-node-abstraction
                    :type (tc:qualify nil type)
                    :source (parser:node-source node)
                    :vars var-nodes
                    :body body-node)
                   subs)))
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
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list node-let tc:substitution-list))

    ;; Ensure that there are no duplicate let bindings
    (check-duplicates
     (parser:node-let-bindings node)
     (alexandria:compose #'parser:node-variable-name #'parser:node-let-binding-name)
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

    (multiple-value-bind (preds binding-nodes subs)
        (infer-let-bindings (parser:node-let-bindings node) (parser:node-let-declares node) subs env file)

      (multiple-value-bind (ty preds_ body-node subs)
          (infer-expression-type (parser:node-let-body node)
                                 expected-type ; pass through expected type
                                 subs
                                 env
                                 file)
        (setf preds (append preds preds_))

        (values
         ty
         preds
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
             (values tc:ty tc:ty-predicate-list node-lisp tc:substitution-list))

    (let ((declared-ty (parse-type (parser:node-lisp-type node) (tc-env-env env) file)))

      (handler-case
          (progn
            (setf subs (tc:unify subs declared-ty expected-type))
            (let ((type (tc:apply-substitution subs declared-ty))
                  
                  (var-nodes
                    (mapcar (lambda (var)
                              (tc-env-lookup-value env var file))
                            (parser:node-lisp-vars node))))
              (values
               type
               nil
               (make-node-lisp
                :type (tc:qualify nil type)
                :source (parser:node-source node)
                :vars var-nodes
                :body (parser:node-lisp-body node))
               subs)))
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
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list node-match tc:substitution-list))

    ;; Infer the type of the expression being cased on
    (multiple-value-bind (expr-ty preds expr-node subs)
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
                     :collect (multiple-value-bind (body-ty preds_ body-node subs_)
                                  (infer-expression-type body ret-ty subs env file)
                                (declare (ignore body-ty))
                                (setf subs subs_)
                                (setf preds (append preds preds_))
                                body-node)))

             (branch-nodes
               (loop :for branch :in (parser:node-match-branches node)
                     :for pat-node :in pat-nodes
                     :for branch-body-node :in branch-body-nodes
                     :collect (make-node-match-branch
                               :pattern pat-node
                               :body branch-body-node
                               :source (parser:node-match-branch-source branch))))

             (type (tc:apply-substitution subs ret-ty)))

        (values
         type
         preds
         (make-node-match
          :type (tc:qualify nil type)
          :source (parser:node-source node)
          :expr expr-node
          :branches branch-nodes)
         subs))))

  (:method ((node parser:node-progn) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list node-progn tc:substitution-list &optional))

    (multiple-value-bind (body-ty preds body-node subs)
        (infer-expression-type (parser:node-progn-body node)
                               expected-type
                               subs
                               env
                               file)
      (values
       body-ty
       preds
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
             (values tc:ty tc:ty-predicate-list node tc:substitution-list))

    (let ((declared-ty (parse-type (parser:node-the-type node) (tc-env-env env) file)))

      (multiple-value-bind (expr-ty preds expr-node subs)
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
        (setf subs (tc:compose-substitution-lists subs (tc:match expr-ty declared-ty)))

        (handler-case
            (progn
              (setf subs (tc:unify subs expr-ty expected-type))
              (values
               (tc:apply-substitution subs expr-ty)
               preds
               expr-node
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

  (:method ((node parser:node-return) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list node-return tc:substitution-list))

    ;; Returns must be inside a lambda
    (when (eq *return-status* :toplevel)
      (error 'tc-error
             :err (coalton-error
                   :span (parser:node-source node)
                   :file file
                   :message "Unexpected return"
                   :primary-note "returns must be inside a lambda")))

    ;; TODO: Handle missing return expr
    (assert (parser:node-return-expr node))

    (multiple-value-bind (ty preds expr-node subs)
        (infer-expression-type (parser:node-return-expr node)
                               (tc:make-variable)
                               subs
                               env
                               file)

      ;; Add node the the list of returns
      (push (cons (parser:node-source node) ty) *returns*)

      (values
       expected-type
       preds
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
             (values tc:ty tc:ty-predicate-list node-or tc:substitution-list))

    (let* ((preds nil)

           (body-nodes
             (loop :for node_ :in (parser:node-or-nodes node)
                   :collect (multiple-value-bind (node_ty_ preds_ node_ subs_)
                                (infer-expression-type node_
                                                       tc:*boolean-type*
                                                       subs
                                                       env
                                                       file)
                              (declare (ignore node_ty_))
                              (setf subs subs_)
                              (setf preds (append preds preds_))
                              node_))))

      (handler-case
          (progn
            (setf subs (tc:unify subs tc:*boolean-type* expected-type))
            (values
             tc:*boolean-type*
             preds
             (make-node-or
              :type (tc:qualify nil tc:*boolean-type*)
              :source (parser:node-source node)
              :nodes body-nodes)
             subs))
        (error:coalton-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:node-source node)
                       :file file
                       :message "Type mismatch"
                       :primary-note (format nil "Expected type '~A' but 'or' evaluates to '~A'"
                                             (tc:apply-substitution subs expected-type)
                                             tc:*boolean-type*)))))))

  (:method ((node parser:node-and) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list node-and tc:substitution-list))

    (let* ((preds nil)

           (body-nodes
             (loop :for node_ :in (parser:node-and-nodes node)
                   :collect (multiple-value-bind (node_ty_ preds_ node_ subs_)
                                (infer-expression-type node_
                                                       tc:*boolean-type*
                                                       subs
                                                       env
                                                       file)
                              (declare (ignore node_ty_))
                              (setf subs subs_)
                              (setf preds (append preds preds_))
                              node_))))

      (handler-case
          (progn
            (setf subs (tc:unify subs tc:*boolean-type* expected-type))
            (values
             tc:*boolean-type*
             preds
             (make-node-and
              :type (tc:qualify nil tc:*boolean-type*)
              :source (parser:node-source node)
              :nodes body-nodes)
             subs))
        (error:coalton-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:node-source node)
                       :file file
                       :message "Type mismatch"
                       :primary-note (format nil "Expected type '~A' but 'and' evaluates to '~A'"
                                             (tc:apply-substitution subs expected-type)
                                             tc:*boolean-type*)))))))

  (:method ((node parser:node-if) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list node-if tc:substitution-list))

    (multiple-value-bind (expr-ty preds expr-node subs)
        (infer-expression-type (parser:node-if-expr node)
                               tc:*boolean-type* ; unify predicate against boolean
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (then-ty preds_ then-node subs)
          (infer-expression-type (parser:node-if-then node)
                                 (tc:make-variable)
                                 subs
                                 env
                                 file)
        (setf preds (append preds preds_))

        (multiple-value-bind (else-ty preds_ else-node subs)
            (infer-expression-type (parser:node-if-else node)
                                   then-ty ; unify against then-ty
                                   subs
                                   env
                                   file)
          (setf preds (append preds preds_))

          (handler-case
              (progn
                (setf subs (tc:unify subs else-ty expected-type))
                (let ((type (tc:apply-substitution subs else-ty)))
                  (values
                   type
                   preds
                   (make-node-if
                    :type (tc:qualify nil type)
                    :source (parser:node-source node)
                    :expr expr-node
                    :then then-node
                    :else else-node)
                   subs)))
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
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list node-when tc:substitution-list))

    (multiple-value-bind (expr-ty preds expr-node subs)
        (infer-expression-type (parser:node-when-expr node)
                               tc:*boolean-type*
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ body-node subs)
          (infer-expression-type (parser:node-when-expr node)
                                 tc:*unit-type*
                                 subs
                                 env
                                 file)
        (setf preds (append preds preds_))

        (handler-case
            (progn
              (setf subs (tc:unify subs body-ty expected-type))
              (values
               tc:*unit-type*
               preds
               (make-node-when
                :type (tc:qualify nil tc:*unit-type*)
                :source (parser:node-source node)
                :expr expr-node
                :body body-node)
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
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list node-unless tc:substitution-list))

    (multiple-value-bind (expr-ty preds expr-node subs)
        (infer-expression-type (parser:node-unless-expr node)
                               tc:*boolean-type*
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ body-node subs)
          (infer-expression-type (parser:node-unless-expr node)
                                 tc:*unit-type*
                                 subs
                                 env
                                 file)
        (setf preds (append preds preds_))

        (handler-case
            (progn
              (setf subs (tc:unify subs body-ty expected-type))
              (values
               tc:*unit-type*
               preds
               (make-node-unless
                :type (tc:qualify nil tc:*unit-type*)
                :source (parser:node-source node)
                :expr expr-node
                :body body-node)
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
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list node-cond-clause tc:substitution-list))

    (multiple-value-bind (expr-ty preds expr-node subs)
        (infer-expression-type (parser:node-cond-clause-expr node)
                               tc:*boolean-type*
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ body-node subs)
          (infer-expression-type (parser:node-cond-clause-body node)
                                 expected-type ; unify against expected-type
                                 subs
                                 env
                                 file)
        (setf preds (append preds preds_))

        (let ((type (tc:apply-substitution subs body-ty)))
          (values
           type
           preds
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
             (values tc:ty tc:ty-predicate-list node-cond tc:substitution-list))

    (let* ((preds nil)

           (ret-ty (tc:make-variable))

           (clause-nodes
             (loop :for clause :in (parser:node-cond-clauses node)
                   :collect (multiple-value-bind (clause-ty preds_ clause-node subs_)
                                (infer-expression-type clause
                                                       ret-ty
                                                       subs
                                                       env
                                                       file)
                              (declare (ignore clause-ty))
                              (setf subs subs_)
                              (setf preds (append preds preds_))
                              clause-node))))

      (handler-case
          (progn
            (setf subs (tc:unify subs ret-ty expected-type))
            (let ((type (tc:apply-substitution subs ret-ty)))
              (values
               type
               preds
               (make-node-cond
                :type (tc:qualify nil type)
                :source (parser:node-source node)
                :clauses clause-nodes)
               subs)))
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
          :name (parser:pattern-var-name pat))
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
     nil))

  (:method ((pat parser:pattern-constructor) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty pattern-constructor tc:substitution-list))

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
           (type coalton-file file)
           (values tc:ty-predicate-list (or toplevel-define-list node-let-binding-list) tc:substitution-list &optional))

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
           (values tc:ty-predicate-list (or toplevel-define-list node-let-binding-list) tc:substitution-list))
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
                              :for name := (parser:node-variable-name (parser:name binding))

                              :when (gethash name dec-table)
                                :collect binding))

         (impl-bindings (loop :with table := (make-hash-table :test #'eq)
                              :for binding :in bindings
                              :for name := (parser:node-variable-name (parser:name binding))

                              :unless (gethash name dec-table)
                                :do (setf (gethash name table) binding)

                              :finally (return table)))

         (impl-bindings-names (alexandria:hash-table-keys impl-bindings))

         (impl-bindings-deps (loop :for name :in impl-bindings-names
                                   :for binding := (gethash name impl-bindings)
                                   :for node := (parser:value binding)

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

                 :for name := (parser:node-variable-name (parser:name binding))
                 :for scheme := (gethash name (tc-env-ty-table env))

                 :collect (multiple-value-bind (preds_ node_ subs_)
                              (infer-expl-binding-type
                               binding
                               scheme
                               (parser:node-source (parser:name binding))
                               subs
                               env
                               file)
                            (setf subs subs_)
                            (setf preds (append preds preds_))
                            node_))))

    (values
     preds
     (append impl-binding-nodes expl-binding-nodes)
     subs)))

(defun infer-expl-binding-type (binding declared-ty source subs env file)
  "Infer the type of BINDING and then ensure it matches DECLARED-TY."
  (declare (type (or parser:toplevel-define parser:node-let-binding) binding)
           (type tc:ty-scheme declared-ty)
           (type cons source)
           (type tc:substitution-list subs)
           (type tc-env env)
           (type coalton-file file)
           (values tc:ty-predicate-list (or toplevel-define node-let-binding) tc:substitution-list &optional))

  (let* ((name (parser:node-variable-name (parser:name binding)))

         (bound-variables (remove name (tc-env-bound-variables env) :test #'eq))

         (fresh-qual-type (tc:fresh-inst declared-ty))
         (fresh-type (tc:qualified-ty-type fresh-qual-type))
         (fresh-preds (tc:qualified-ty-predicates fresh-qual-type)))

    (multiple-value-bind (preds binding-node subs)
        (infer-binding-type
         binding
         fresh-type                     ; unify against declared type
         subs
         env
         file)

      (tc:apply-substitution subs env)

      (let* ((expr-type (tc:apply-substitution subs fresh-type))
             (expr-preds (tc:apply-substitution subs fresh-preds))

             (env-tvars (tc-env-bindings-variables env bound-variables))
             (local-tvars (set-difference (tc:type-variables expr-type) env-tvars :test #'eq))

             (output-qual-type (tc:qualify expr-preds expr-type))
             (output-scheme (tc:quantify local-tvars output-qual-type))

             (reduced-preds (remove-if-not (lambda (p)
                                             (not (tc:entail (tc-env-env env) expr-preds p)))
                                           (tc:apply-substitution subs preds))))

        ;; Generate additional substitutions from fundeps
        (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) reduced-preds subs)))

        ;; Split predicates into retained and deferred
        (multiple-value-bind (deferred-preds retained-preds)
            (tc:split-context (tc-env-env env) env-tvars reduced-preds subs)

          (let* (;; Calculate defaultable predicates
                 (defaultable-preds
                   (handler-case
                       (tc:default-preds (tc-env-env env) (append env-tvars local-tvars) retained-preds)
                     (error:coalton-type-error (e)
                       (error-ambigious-pred (tc:ambigious-constraint-pred e) file))))

                 ;; Defaultable predicates are not retained
                 (retained-preds
                   (set-difference retained-preds defaultable-preds :test #'eq)))

            ;; Apply defaulting to defaultable predicates
            (setf subs (tc:compose-substitution-lists
                        (tc:default-subs (tc-env-env env) nil defaultable-preds)
                        subs))

            ;; If the bindings is toplevel then attempt to default deferred-predicates
            (when (parser:toplevel binding)
              (setf subs (tc:compose-substitution-lists
                          (tc:default-subs (tc-env-env env) nil deferred-preds)
                          subs))
              (setf deferred-preds (tc:reduce-context (tc-env-env env) deferred-preds subs)))

            ;; Toplevel bindings cannot defer predicates
            (when (and (parser:toplevel binding) deferred-preds)
              (error-ambigious-pred (first deferred-preds) file))

            ;; Check that the declared and inferred schemes match
            (unless (equalp declared-ty output-scheme)
              (error 'tc-error
                     :err (coalton-error
                           :message "Declared type is too general"
                           :span source
                           :file file
                           :primary-note (format nil "Declared type ~A is more general than inferred type ~A."
                                                 declared-ty
                                                 output-scheme))))

            ;; Check for undeclared predicates
            (when (not (null retained-preds))
              (error 'tc-error
                     :err (coalton-error
                           :message "Explicit type is missing inferred predicate"
                           :span source
                           :file file
                           :primary-note (format nil "Declared type ~A is missing inferred predicate ~A"
                                                 output-qual-type
                                                 (first retained-preds)))))

            (values
             deferred-preds
             (attach-explicit-binding-type binding-node fresh-qual-type)
             subs)))))))

(defun infer-impls-binding-type (bindings subs env file)
  "Infer the type's of BINDINGS and then qualify those types into schemes."
  (declare (type (or parser:toplevel-define-list parser:node-let-binding-list) bindings)
           (type tc:substitution-list subs)
           (type tc-env env)
           (type coalton-file file)
           (values tc:ty-predicate-list (or toplevel-define-list node-let-binding-list) tc:substitution-list &optional))

  (let* (;; track variables bound before typechecking
         (bound-variables (tc-env-bound-variables env))

         ;; Add all bindings to the environment
         (expr-tys
           (loop :for binding :in bindings
                 :for name := (parser:node-variable-name (parser:name  binding))
                 :collect (tc-env-add-variable env name)))

         (preds nil)

         ;; Derive the type of each binding
         (binding-nodes
           (loop :for binding :in bindings
                 :for ty :in expr-tys
                 :for node := (parser:value binding)

                 :collect (multiple-value-bind (preds_ node_ subs_)
                              (infer-binding-type binding ty subs env file)
                            (setf subs subs_)
                            (setf preds (append preds preds_))
                            node_))))

    (let* ((expr-tys (tc:apply-substitution subs expr-tys))

           (env-tvars (tc:apply-substitution subs (tc-env-bindings-variables env bound-variables)))

           (expr-tvars (remove-duplicates (tc:type-variables expr-tys) :test #'eq))

           (local-tvars (set-difference expr-tvars env-tvars :test #'eq)))

      (setf subs (nth-value 1 (tc:solve-fundeps (tc-env-env env) preds subs)))

      (multiple-value-bind (deferred-preds retained-preds)
          (tc:split-context (tc-env-env env) env-tvars preds subs)

        (let* ((defaultable-preds (handler-case
                                      (tc:default-preds (tc-env-env env) (append env-tvars local-tvars) retained-preds)
                                    (error:coalton-type-error (e)
                                      (error-ambigious-pred (tc:ambigious-constraint-pred e) file))))

               (retained-preds (set-difference retained-preds defaultable-preds :test #'eq))

               ;; Check if the monomorphism restriction applies
               (restricted (some (lambda (b)
                                   (not (parser:restricted b)))
                                 bindings)))


          (setf subs (tc:compose-substitution-lists
                      (tc:default-subs (tc-env-env env) nil defaultable-preds)
                      subs))

          (when (parser:toplevel (first bindings))
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

                     (output-schemes
                       (loop :for ty :in expr-tys
                             :collect (tc:quantify
                                       allowed-tvars
                                       (tc:make-qualified-ty :predicates nil :type ty))))

                     (deferred-preds (append deferred-preds retained-preds)))

                (loop :for scheme :in output-schemes
                      :for binding :in bindings

                      :for name := (parser:node-variable-name (parser:name binding))
                      :do (tc-env-replace-type env name scheme))

                (when (and (parser:toplevel (first bindings)) deferred-preds)
                  (error-ambigious-pred (first deferred-preds) file))

                (values
                 deferred-preds
                 binding-nodes
                 subs))

              (let* ((output-schemes
                       (loop :for ty :in expr-tys
                             :collect (tc:quantify
                                       local-tvars
                                       (tc:make-qualified-ty :predicates retained-preds :type ty)))))

                (loop :for scheme :in output-schemes
                      :for binding :in bindings

                      :for name := (parser:node-variable-name (parser:name binding))
                      :do (tc-env-replace-type env name scheme))

                (when (and (parser:toplevel (first bindings)) deferred-preds)
                  (error-ambigious-pred (first deferred-preds) file))

                (values
                 deferred-preds
                 binding-nodes
                 subs))))))))

(defun infer-binding-type (binding expected-type subs env file)
  "Infer the type of BINDING then unify against EXPECTED-TYPE. Adds BINDING's paramaters to the environment."
  (declare (type (or parser:toplevel-define parser:node-let-binding) binding)
           (type tc:ty expected-type)
           (type tc:substitution-list subs)
           (type coalton-file file)
           (values tc:ty-predicate-list (or toplevel-define node-let-binding) tc:substitution-list))

  (let* ((vars (loop :for var :in (parser:parameters binding)
                     :for name := (parser:node-variable-name var)
                     :collect (tc-env-add-variable env name)))

         (ret-ty (tc:make-variable))

         (preds nil)

         (value-node
           (if vars
               ;; If the binding has parameters that setup the return state before infering the binding's type
               (let ((*return-status* :lambda)

                     (*returns* nil))

                 (multiple-value-bind (ty_ preds_ value-node subs_)
                     (infer-expression-type (parser:value binding)
                                            ret-ty
                                            subs
                                            env
                                            file)
                   (declare (ignore ty_))
                   (setf subs subs_)
                   (setf preds preds_)

                   ;; Ensure that all early returns unify
                   (loop :with returns := (reverse *returns*)
                         :for (s1 . ty1) :in returns
                         :for (s2 . ty2) :in (cdr returns)
                         :do (handler-case
                                 (setf subs (tc:unify subs ty1 ty2))
                               (error:coalton-type-error ()
                                 (error 'tc-error
                                        :err (coalton-error
                                              :span s1
                                              :file file
                                              :message "Return type mismatch"
                                              :primary-note (format nil "First return is of type '~A'"
                                                                    (tc:apply-substitution subs ty1))
                                              :notes
                                              (list
                                               (make-coalton-error-note
                                                :type :primary
                                                :span s2
                                                :message (format nil "Second return is of type '~A'"
                                                                 (tc:apply-substitution subs ty2)))))))))

                   ;; Unify the function's inferered type with one of the early returns.
                   (when *returns*
                     (handler-case
                         (setf subs (tc:unify subs (cdr (first *returns*)) ret-ty))
                       (error:coalton-type-error ()
                         (error 'tc-error
                                :err (coalton-error
                                      :span (car (first *returns*))
                                      :file file
                                      :message "Return type mismatch"
                                      :primary-note (format nil "First return is of type '~A'"
                                                            (tc:apply-substitution subs (cdr (first *returns*))))
                                      :notes
                                      (list
                                       (make-coalton-error-note
                                        :type :primary
                                        :span (parser:node-source (parser:last-node binding))
                                        :message (format nil "Second return is of type '~A'"
                                                         (tc:apply-substitution subs ret-ty)))))))))

                   value-node))

               ;; If the binding does not have parameters that just infer the binding's type
               (multiple-value-bind (ty_ preds_ value-node subs_)
                   (infer-expression-type (parser:value binding)
                                          ret-ty
                                          subs
                                          env
                                          file)
                 (declare (ignore ty_))
                 (setf subs subs_)
                 (setf preds preds_)
                 value-node))))

    (let ((ty (tc:make-function-type* vars ret-ty)))
      (handler-case
          (progn
            (setf subs (tc:unify subs ty expected-type))

            (let* ((type (tc:apply-substitution subs ty))

                   (name-node
                     (make-node-variable
                      :type (tc:qualify nil type)
                      :source (parser:node-source (parser:name binding))
                      :name (parser:node-variable-name (parser:name binding))))

                   (var-nodes
                     (mapcar (lambda (var ty)
                               (make-node-variable
                                :type (tc:qualify nil ty)
                                :source (parser:node-source var)
                                :name (parser:node-variable-name var)))
                             (parser:toplevel-define-vars binding)
                             vars))

                   (typed-binding (build-typed-binding binding name-node value-node var-nodes)))
              (values
               preds
               typed-binding
               subs)))
        (error:coalton-type-error ()
          (error 'tc-error
                 :err (coalton-error
                       :span (parser:source binding)
                       :file file
                       :message "Type mismatch"
                       :primary-note (format nil "Expected type '~A' but got type '~A'"
                                             (tc:apply-substitution subs expected-type)
                                             (tc:apply-substitution subs ty)))))))))

(defgeneric build-typed-binding (binding name value vars)
  (:method ((binding parser:toplevel-define) name value vars)
    (declare (type node-variable name)
             (type node-body value)
             (type node-variable-list vars))
    
   (make-toplevel-define
    :source (parser:toplevel-define-source binding)
    :name name
    :body value
    :vars vars
    :explicit-type nil))
  
  (:method ((binding parser:node-let-binding) name value vars)
    (declare (type node-variable name)
             (type node value)
             (type node-variable-list vars))
    
    (unless (null vars)
      (util:coalton-bug "Unexpected parameters on let binding"))
    
   (make-node-let-binding
    :source (parser:node-let-binding-source binding)
    :name name
    :value value
    :explicit-type nil)))

(defgeneric attach-explicit-binding-type (binding explicit-type)
  (:method ((binding toplevel-define) explicit-type)
    (declare (type tc:qualified-ty explicit-type))
    
   (make-toplevel-define
    :source (toplevel-define-source binding)
    :name (toplevel-define-name binding)
    :body (toplevel-define-body binding)
    :vars (toplevel-define-vars binding)
    :explicit-type explicit-type))
  
  (:method ((binding node-let-binding) explicit-type)
    (declare (type tc:qualified-ty explicit-type))
    
   (make-node-let-binding
    :source (node-let-binding-source binding)
    :name (node-let-binding-name binding)
    :value (node-let-binding-value binding)
    :explicit-type explicit-type)))
