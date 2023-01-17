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

(defpackage #:coalton-impl/typechecker2/define
  (:use
   #:cl
   #:coalton-impl/typechecker2/base
   #:coalton-impl/typechecker2/parse-type)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:parser #:coalton-impl/parser)
   (#:error #:coalton-impl/error)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:toplevel-define                    ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker2/define)

;;;
;;; Typechecking Environment
;;;

(defstruct (tc-env
            (:predicate nil))

  ;; The main copiler env
  (env      (util:required 'env)          :type tc:environment :read-only t)

  ;; Hash table mappinig variables bound in the current translation unit to types
  (ty-table (make-hash-table :test #'eq)  :type hash-table     :read-only t)

  ;; Hash table mapping type variables in predicates to the variable
  ;; that introduced them. This is only used for error messages.
  (var-table (make-hash-table :test #'eq) :type hash-table     :read-only t))

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
           (type coalton-file file)
           (values tc:ty tc:ty-predicate-list))

  (let* ((scheme (or (gethash (parser:node-variable-name var) (tc-env-ty-table env))

                 (tc:lookup-value-type (tc-env-env env) (parser:node-variable-name var) :no-error t)

                 (error 'tc-error
                        :err (coalton-error
                              :span (parser:node-source var)
                              :file file
                              :message "Unknown variable"
                              :primary-note "unknown variable"))))

         (qual-ty (tc:fresh-inst scheme))

         (ty (tc:qualified-ty-type qual-ty))

         (preds (tc:qualified-ty-predicates qual-ty)))

    (loop :for tvar :in (tc:type-variables preds)
          :do (setf (gethash tvar (tc-env-var-table env)) var))

    (values
     ty
     preds)))

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

(defun tc-env-bound-variables (env)
  (declare (type tc-env env)
           (values util:symbol-list &optional))

  (alexandria:hash-table-keys (tc-env-ty-table env)))

(defun tc-env-bindings-variables (env names)
  (declare (type tc-env env)
           (type util:symbol-list names)
           (values tc:tyvar-list))

  (remove-duplicates
   (loop :with table := (tc-env-ty-table env)
         :for name :in names
         :for ty := (gethash name table)

         :unless ty
           :do (util:coalton-bug "Unknown binding ~A" name)

         :append (tc:type-variables ty))
   :test #'eq))

(defun tc-env-replace-type (env name scheme)
  (declare (type tc-env env)
           (type symbol name)
           (type tc:ty-scheme scheme)
           (values null))

  (unless (gethash name (tc-env-ty-table env))
    (util:coalton-bug "Attempt to replace unknown type ~A" name))

  (setf (gethash name (tc-env-ty-table env)) scheme)

  nil)

(defun tc-env-ambigious-pred (env pred file subs)
  (declare (type tc-env env)
           (type tc:ty-predicate pred)
           (type coalton-file file)
           (type tc:substitution-list subs))

  (tc:apply-substitution subs env)

  (let ((tvars (tc:type-variables (tc:apply-substitution subs pred))))
    (unless tvars
      (util:coalton-bug "Ambigious predicate ~A does not have any variables." pred))

    (let ((node (gethash (first tvars) (tc-env-var-table env))))
      (unless node
        (util:coalton-bug "Unable to find source form for ambigious predicate ~A." pred))

      (error 'tc-error
             :err (coalton-error
                   :span (parser:node-source node)
                   :file file
                   :message "Ambigious predicate"
                   :primary-note (format nil "Ambigious predicate ~A" pred))))))

(defmethod tc:apply-substitution (subs (env tc-env))
  "Applies SUBS to the types currently being checked in ENV. Does not update the types in the inner main environment because there should not be substitutions for them."
  (maphash
   (lambda (key value)
     (setf (gethash key (tc-env-ty-table env)) (tc:apply-substitution subs value)))
   (tc-env-ty-table env))

  (loop :with var-table := (tc-env-var-table env)
        :with keys := (alexandria:hash-table-keys var-table)

        :for key :in keys
        :for ty := (tc:apply-substitution subs key)

        :when (tc:tyvar-p ty)
          :do (setf (gethash ty var-table) (gethash key var-table))))

(defmethod tc:type-variables ((env tc-env))
  "Returns all of the type variables of the types being checked in ENV. Does not return type variables from the inner main environment because it should not contain any free type variables."
  (loop :for ty :being :the :hash-values :of (tc-env-ty-table env)
        :append (tc:type-variables ty)))

;;;
;;; Entrypoint
;;;

;; TODO: validate that all definitinos are defined in the current package

(defun toplevel-define (defines declares file env)
  "Entrypoint for typechecking a group of parsed defines and declares."
  (declare (type parser:toplevel-define-list defines)
           (type parser:toplevel-declare-list declares)
           (type coalton-file file)
           (type tc:environment env))

  (let ((def-table (make-hash-table :test #'eq))

        (dec-table (make-hash-table :test #'eq)))

    ;; Ensure that there are no duplicate definitions
    (loop :for define :in defines
          :for name := (parser:node-variable-name (parser:name define))

          :if (gethash name def-table)
            :do (error 'tc-error
                       :err (coalton-error
                             :span (parser:node-source (parser:name define))
                             :file file
                             :message "Duplicate definition"
                             :primary-note "second definition here"
                             :notes
                             (list
                              (make-coalton-error-note
                               :type :primary
                               :span (parser:node-source
                                      (parser:name
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

    (let ((dec-table (make-hash-table :test #'eq))

          (tc-env (make-tc-env :env env)))

      (loop :for declare :in declares
            :for name := (parser:identifier-src-name (parser:toplevel-declare-name declare))
            :for ty := (parser:toplevel-declare-type declare)
            :do (setf (gethash name dec-table) ty))

      (infer-bindings-type defines dec-table nil tc-env file)

      (loop :for define :in defines
            :for name := (parser:node-variable-name (parser:name define))
            :do (format t "~A :: ~A~%" name (gethash name (tc-env-ty-table tc-env))))

      tc-env)))

;;;
;;; Expression Type Inference
;;;

(defgeneric infer-expression-type (node expected-type subs env file)
  (:documentation "Infer the type of NODE and then unify against EXPECTED-TYPE")
  (:method ((node parser:node-literal) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

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
             nil
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
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    (multiple-value-bind (ty preds)
        (tc-env-lookup-value env node file)

      (handler-case
          (progn
            (setf subs (tc:unify subs ty expected-type))
            (values
             (tc:apply-substitution subs ty)
             preds
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
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    (when (null (parser:node-application-rands node))
      (error ""))

    (multiple-value-bind (fun-ty preds subs)
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
              :do (multiple-value-bind (rand-ty preds_ subs_)
                      (infer-expression-type rand
                                             (tc:apply-substitution subs arg-ty)
                                             subs
                                             env
                                             file)
                    (declare (ignore rand-ty))
                    (setf subs subs_)
                    (setf preds (append preds preds_))))

        (let* ((ty (tc:make-function-type*
                    (subseq arg-tys (length (parser:node-application-rands node)))
                    (tc:function-return-type fun-ty)))

               (ty (tc:apply-substitution subs ty)))

          (handler-case
              (progn
                (setf subs (tc:unify subs ty expected-type))
                (values
                 (tc:apply-substitution subs ty)
                 preds
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
             (type coalton-file file)
             (values null tc:ty-predicate-list tc:substitution-list))

    (multiple-value-bind (expr-ty preds subs)
        (infer-expression-type (parser:node-bind-expr node)
                               (tc:make-variable)
                               subs
                               env
                               file)

      (multiple-value-bind (pat-ty preds_ subs)
          (infer-pattern-type (parser:node-bind-pattern node)
                              expr-ty   ; unify against expr-ty
                              subs
                              env
                              file)
        (declare (ignore pat-ty))
        (setf preds (append preds preds_))

        (values
         nil                ; return nil as this is always thrown away
         preds
         subs))))

  (:method ((node parser:node-body) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    (let ((preds nil))

      ;; Infer the type of each node
      (loop :for node_ :in (parser:node-body-nodes node)
            :do (multiple-value-bind (node_ty_ preds_ subs_)
                    (infer-expression-type node_ (tc:make-variable) subs env file)
                  (declare (ignore node_ty_))
                  (setf subs subs_)
                  (setf preds (append preds preds_))))

      (multiple-value-bind (ty preds_ subs)
            (infer-expression-type (parser:node-body-last-node node) expected-type subs env file)
        (setf preds (append preds preds_))

        (values
         ty
         preds
         subs))))

  (:method ((node parser:node-abstraction) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    (let (;; Setup return environment
          (*return-status* :lambda)
          (*returns* nil)

          ;; Add parameters to the environment
          (arg-tys
            (loop :for var :in (parser:node-abstraction-vars node)
                  :collect (tc-env-add-variable env (parser:node-variable-name var)))))

      (multiple-value-bind (body-ty preds subs)
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
                (values
                 (tc:apply-substitution subs ty)
                 preds
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
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    (multiple-value-bind (subs preds)
        (infer-let-bindings (parser:node-let-bindings node) (parser:node-let-declares node) subs env file)

      (multiple-value-bind (ty preds_ subs)
          (infer-expression-type (parser:node-let-body node)
                                 expected-type ; pass through expected type
                                 subs
                                 env
                                 file)
        (setf preds (append preds preds_))

        (values
         ty
         subs))))

  (:method ((node parser:node-lisp) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    (let ((declared-ty (parse-type (parser:node-lisp-type node) (tc-env-env env) file)))

      (handler-case
          (progn
            (setf subs (tc:unify subs declared-ty expected-type))
            (values
             (tc:apply-substitution subs declared-ty)
             nil
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
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    ;; Infer the type of the expression being cased on
    (multiple-value-bind (expr-ty preds subs)
        (infer-expression-type (parser:node-match-expr node)
                               (tc:make-variable)
                               subs
                               env
                               file)

      ;; Infer the type of each pattern, unifying against expr-ty
      (loop :for branch :in (parser:node-match-branches node)
            :for pattern := (parser:node-match-branch-pattern branch)
            :do (multiple-value-bind (pat-ty preds_ subs_)
                    (infer-pattern-type pattern expr-ty subs env file)
                  (declare (ignore pat-ty))
                  (setf subs subs_)
                  (setf preds (append preds preds_))))

      (let ((ret-ty (tc:make-variable)))

        ;; Infer the type of each branch, unifying against ret-ty
        (loop :for branch :in (parser:node-match-branches node)
              :for body := (parser:node-match-branch-body branch)
              :do (multiple-value-bind (body-ty preds_ subs_)
                      (infer-expression-type body ret-ty subs env file)
                    (declare (ignore body-ty))
                    (setf subs subs_)
                    (setf preds preds_)))

        (values
         (tc:apply-substitution subs ret-ty)
         preds
         subs))))

  (:method ((node parser:node-progn) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list tc:substitution-list &optional))

    (infer-expression-type (parser:node-progn-body node)
                           expected-type
                           subs
                           env
                           file))


  (:method ((node parser:node-the) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    (let ((declared-ty (parse-type (parser:node-the-type node) (tc-env-env env) file)))

      (multiple-value-bind (expr-ty preds subs)
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
               preds
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
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    ;; Returns must be inside a lambda
    (when (eq *return-status* :toplevel)
      (error 'tc-error
             :err (coalton-error
                   :span (parser:node-source node)
                   :file file
                   :message "Unexpected return"
                   :primary-note "returns must be inside a lambda")))

    (multiple-value-bind (ty preds subs)
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
       subs)))

  (:method ((node parser:node-or) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    (let ((preds nil))

      (loop :for node_ :in (parser:node-or-nodes node)
            :do (multiple-value-bind (node_ty_ preds_ subs_)
                    (infer-expression-type node_
                                           tc:*boolean-type*
                                           subs
                                           env
                                           file)
                  (declare (ignore node_ty_))
                  (setf subs subs_)
                  (setf preds (append preds preds_))))

      (handler-case
          (progn
            (setf subs (tc:unify subs tc:*boolean-type* expected-type))
            (values
             tc:*boolean-type*
             preds
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
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    (let ((preds))

      (loop :for node_ :in (parser:node-and-nodes node)
            :do (multiple-value-bind (node_ty_ preds_ subs_)
                    (infer-expression-type node_
                                           tc:*boolean-type*
                                           subs
                                           env
                                           file)
                  (declare (ignore node_ty_))
                  (setf subs subs_)
                  (setf preds (append preds preds_))))

      (handler-case
          (progn
            (setf subs (tc:unify subs tc:*boolean-type* expected-type))
            (values
             tc:*boolean-type*
             preds
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
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    (multiple-value-bind (expr-ty preds subs)
        (infer-expression-type (parser:node-if-expr node)
                               tc:*boolean-type* ; unify predicate against boolean
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (then-ty preds_ subs)
          (infer-expression-type (parser:node-if-then node)
                                 (tc:make-variable)
                                 subs
                                 env
                                 file)
        (setf preds (append preds preds_))

        (multiple-value-bind (else-ty preds_ subs)
            (infer-expression-type (parser:node-if-else node)
                                   then-ty ; unify against then-ty
                                   subs
                                   env
                                   file)
          (setf preds (append preds preds_))

          (handler-case
              (progn
                (setf subs (tc:unify subs else-ty expected-type))
                (values
                 (tc:apply-substitution subs else-ty)
                 preds
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
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    (multiple-value-bind (expr-ty preds subs)
        (infer-expression-type (parser:node-when-expr node)
                               tc:*boolean-type*
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ subs)
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
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    (multiple-value-bind (expr-ty preds subs)
        (infer-expression-type (parser:node-unless-expr node)
                               tc:*boolean-type*
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ subs)
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
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    (multiple-value-bind (expr-ty preds subs)
        (infer-expression-type (parser:node-cond-clause-expr node)
                               tc:*boolean-type*
                               subs
                               env
                               file)
      (declare (ignore expr-ty))

      (multiple-value-bind (body-ty preds_ subs)
          (infer-expression-type (parser:node-cond-clause-body node)
                                 expected-type ; unify against expected-type
                                 subs
                                 env
                                 file)
        (setf preds (append preds preds_))

        (values
         (tc:apply-substitution subs body-ty)
         preds
         subs))))

  (:method ((node parser:node-cond) expected-type subs env file)
    (declare (type tc:ty expected-type)
             (type tc:substitution-list subs)
             (type tc-env env)
             (type coalton-file file)
             (values tc:ty tc:ty-predicate-list tc:substitution-list))

    (let ((preds))

      (let ((ret-ty (tc:make-variable)))

        (loop :for clause :in (parser:node-cond-clauses node)
              :do (multiple-value-bind (clause-ty preds_ subs_)
                      (infer-expression-type clause
                                             ret-ty
                                             subs
                                             env
                                             file)
                    (declare (ignore clause-ty))
                    (setf subs subs_)
                    (setf preds (append preds preds_))))

        (handler-case
            (progn
              (setf subs (tc:unify subs ret-ty expected-type))
              (values
               (tc:apply-substitution subs ret-ty)
               preds
               subs))
          (error:coalton-type-error ()
            (error 'tc-error
                   :err (coalton-error
                         :span (parser:node-source node)
                         :file file
                         :message "Type mismatch"
                         :primary-note (format nil "Expected type '~A' but got '~A'"
                                               (tc:apply-substitution subs expected-type)
                                               (tc:apply-substitution subs ret-ty)))))))))

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
             (type coalton-file file)
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
             (type coalton-file file)
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
             (type coalton-file file)
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
           (type coalton-file file))

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
           (values tc:ty-predicate-list tc:substitution-list))
  ;;
  ;; Binding type inference has several steps.
  ;; 1. Explicit types are parsed and added to the environment
  ;; 2. Implicit bindings are grouped by scc and then each scc is type checked
  ;; 3. Explicitly typed bindings are typechecked and compared against their declared types.
  ;;

  ;; Define explicit types to the environment
  (loop :for name :being :the :hash-keys :of dec-table
        :for unparsed-ty :being :the :hash-values :of dec-table

        :for qual-ty := (parse-qualified-type unparsed-ty (tc-env-env env) file)
        :do (tc-env-add-definition env name (tc:quantify (tc:type-variables qual-ty) qual-ty)))

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
                                                          (collect-variables node))
                                                  impl-bindings-names
                                                  :test #'eq)
                                                 :test #'eq)
                                   :collect (cons name deps)))

         (sccs (algo:tarjan-scc impl-bindings-deps))

         (preds nil))

    ;; Infer the types of implicit bindings on scc at a time
    (loop :for scc :in (reverse sccs)
          :for bindings
            := (loop :for name :in scc
                     :collect (gethash name impl-bindings))
          :do (multiple-value-bind (preds_ subs_)
                  (infer-impls-binding-type bindings subs env file)
                (setf subs subs_)
                (setf preds (append preds preds_))))

    ;; Infer the type of each explicit bindings and check against the
    ;; declared type
    (loop :for binding :in expl-bindings

          :for name := (parser:node-variable-name (parser:name binding))
          :for scheme := (gethash name (tc-env-ty-table env))

          :do (multiple-value-bind (preds_ subs_)
                  (infer-expl-binding-type
                   binding
                   scheme
                   (parser:node-source (parser:name binding))
                   subs
                   env
                   file)
                (setf subs subs_)
                (setf preds (append preds preds_))))

    (values
     preds
     subs)))

(defun infer-expl-binding-type (binding declared-ty source subs env file)
  "Infer the type of BINDING and then ensure it matches DECLARED-TY."
  (declare (type (or parser:toplevel-define parser:node-let-binding) binding)
           (type tc:ty-scheme declared-ty)
           (type cons source)
           (type tc:substitution-list subs)
           (type tc-env env)
           (type coalton-file file)
           (values tc:ty-predicate-list tc:substitution-list))

  (let* ((name (parser:node-variable-name (parser:name binding)))

         (bound-variables (remove name (tc-env-bound-variables env) :test #'eq))

         (fresh-qual-type (tc:fresh-inst declared-ty))
         (fresh-type (tc:qualified-ty-type fresh-qual-type))
         (fresh-preds (tc:qualified-ty-predicates fresh-qual-type)))

    (multiple-value-bind (preds subs)
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
        (setf subs (tc:solve-fundeps (tc-env-env env) reduced-preds subs))

        ;; Split predicates into retained and deferred
        (multiple-value-bind (deferred-preds retained-preds)
            (tc:split-context (tc-env-env env) env-tvars reduced-preds subs)

          (let* (;; Calculate defaultable predicates
                 (defaultable-preds
                   (handler-case
                       (tc:default-preds (tc-env-env env) (append env-tvars local-tvars) retained-preds)
                     (error:coalton-type-error (e)
                       (tc-env-ambigious-pred env (tc:ambigious-constraint-pred e) file subs))))

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
              (tc-env-ambigious-pred env (first deferred-preds) file subs))

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
             subs)))))))

(defun infer-impls-binding-type (bindings subs env file)
  "Infer the type's of BINDINGS and then qualify those types into schemes."
  (declare (type (or parser:toplevel-define-list parser:node-let-binding-list) bindings)
           (type tc:substitution-list subs)
           (type tc-env env)
           (type coalton-file file)
           (values tc:ty-predicate-list tc:substitution-list))

  (let* (;; track variables bound before typechecking
         (bound-variables (tc-env-bound-variables env))

         ;; Add all bindings to the environment
         (expr-tys
           (loop :for binding :in bindings
                 :for name := (parser:node-variable-name (parser:name  binding))
                 :collect (tc-env-add-variable env name)))

         (preds nil))

    ;; Derive the type of each binding
    (loop :for binding :in bindings
          :for ty :in expr-tys
          :for node := (parser:value binding)

          :do (multiple-value-bind (preds_ subs_)
                  (infer-binding-type binding ty subs env file)
                (setf subs subs_)
                (setf preds (append preds preds_))))

    ;; Update the environment
    (tc:apply-substitution subs env)

    (let* ((expr-tys (tc:apply-substitution subs expr-tys))

           (env-tvars (tc-env-bindings-variables env bound-variables))

           (expr-tvars (remove-duplicates (tc:type-variables expr-tys) :test #'eq))

           (local-tvars (set-difference expr-tvars env-tvars :test #'eq)))

      (setf subs (tc:solve-fundeps (tc-env-env env) preds subs))

      (multiple-value-bind (deferred-preds retained-preds)
          (tc:split-context (tc-env-env env) env-tvars preds subs)

        (let* ((defaultable-preds (tc:default-preds (tc-env-env env) (append env-tvars local-tvars) retained-preds))

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
                  (tc-env-ambigious-pred env (first deferred-preds) file subs))

                
                (values
                 deferred-preds
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
                  (tc-env-ambigious-pred env (first deferred-preds) file subs))

                (values
                 deferred-preds
                 subs))))))))

(defun infer-binding-type (binding expected-type subs env file)
  "Infer the type of BINDING then unify against EXPECTED-TYPE. Adds BINDING's paramaters to the environment."
  (declare (type (or parser:toplevel-define parser:node-let-binding) binding)
           (type tc:ty expected-type)
           (type tc:substitution-list subs)
           (type coalton-file file)
           (values tc:ty-predicate-list tc:substitution-list))

  (let ((vars (loop :for var :in (parser:parameters binding)
                    :for name := (parser:node-variable-name var)
                    :collect (tc-env-add-variable env name))))

    (multiple-value-bind (ret-ty preds subs)
        (infer-expression-type (parser:value binding)
                               (tc:make-variable)
                               subs
                               env
                               file)

      (let ((ty (tc:make-function-type* vars ret-ty)))
        (handler-case
            (progn
              (setf subs (tc:unify subs ty expected-type))
              (values
               preds
               subs))
          (error:coalton-type-error ()
            (error 'tc-error
                   :err (coalton-error
                         :span (parser:source binding)
                         :file file
                         :message "Type mismatch"
                         :primary-note (format nil "Expected type '~A' but got type '~A'"
                                               (tc:apply-substitution subs expected-type)
                                               (tc:apply-substitution subs ty))))))))))
