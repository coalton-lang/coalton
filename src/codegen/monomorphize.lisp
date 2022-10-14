;;;; src/codegen/monomorphize.lisp
;;;;
;;;; Recompile specialized versions of type class generic functions across a call graph.
;;;;
;;;; The monomorphizer works by traversing a callgraph, it starts at a
;;;; given entrypoint and looks up the optimized AST associated that
;;;; entrypoint. The function CANDIDATE-SELECTION then finds function
;;;; applications within that entrypoint that are being passed
;;;; statically known typeclass dictionaries. These application sites
;;;; and their arguments are referred to as "compile candidates".
;;;;
;;;; (declare (Num :a => :a -> :a))
;;;; (define (f x)
;;;;   (+ 1 2)               <- Addition on "Integer"
;;;;   (+ (Some x) (Some x)) <- Addition on "Optional :a"
;;;;   (+ x x)               <- Addition on ":a"
;;;;
;;;; In the above example only the first function application is a
;;;; valid candidate. The later calls to + do not have statically
;;;; known dictionaries. SOME has no typeclass dictionaries and thus
;;;; is also not a valid candidate.
;;;;
;;;; Candidates are comprised of a function name, which currently must
;;;; be defined at the top level. And a list of arguments which can be
;;;; nodes or the placeholder value '@@unpropagated. Unpropagated
;;;; values will remain as function arguments in the recompiled
;;;; version of a function. Valid candidates have at least two
;;;; arguments, at least one argument must not be '@@unpropagated, and
;;;; they must have at least one unpropagated argument.
;;;; 
;;;; Valid candidates can have unknown typeclass dictionaries as long
;;;; as at least one argument is a valid dictionary. Candidates are
;;;; each given a unique name, used as the name of their recompiled
;;;; function. Candidates are only recompiled once per callgraph, but
;;;; the same function may be recompiled many times with different
;;;; static dictionaries as arguments. Candidate recompilation can
;;;; also be deduplicated across multiple monomorphized callgraphs in
;;;; the same compilation unit.

(defpackage #:coalton-impl/codegen/monomorphize
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/transformations
   #:traverse
   #:traverse-bindings)
  (:import-from
   #:coalton-impl/codegen/ast-substitutions
   #:ast-substitution
   #:make-ast-substitution
   #:ast-substitution-from
   #:ast-substitution-to
   #:apply-ast-substitution)
  (:import-from
   #:coalton-impl/codegen/typecheck-node
   #:typecheck-node)
  (:import-from
   #:coalton-impl/codegen/hoister
   #:make-hoister
   #:pop-final-hoist-point)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:make-candidate-manager
   #:monomorphize))

(in-package #:coalton-impl/codegen/monomorphize)

(deftype argument ()
  "An ARGUMENT is either a NODE or the marker value '@@unpropagated"
  `(or (member @@unpropagated) node))

(defun argument-p (x)
  (typep x 'argument))

(defun argument-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'argument-p x)))

(deftype argument-list ()
  '(satisfies argument-list-p))

(defstruct compile-candidate
  "A COMPILE-CANDIDATE is a compilation of a NAME and a list of
ARGUMENTS some of which are statically known dictionaries."
  (name      (util:required 'name) :type symbol        :read-only t)
  (args      (util:required 'args) :type argument-list :read-only t))

(defun compile-candidate-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'compile-candidate-p x)))

(deftype compile-candidate-list ()
  '(satisfies compile-candidate-list-p))

(defun propagatable-p (x)
  (not (eq x '@@unpropagated)))

(defun node-can-be-propidated (node env)
  "Returns true if a given node should be const propidated to a callee.
Currently only typeclass dictionaries will be propidated, although
constant values could be."
  (cond
    ((and
      (node-variable-p node)
      (tc:lookup-instance-by-codegen-sym env (node-variable-value node) :no-error t))
     t)

    ((and
      (node-direct-application-p node)
      (tc:lookup-instance-by-codegen-sym env (node-direct-application-rator node) :no-error t))
     t) 

    ((and
      (node-application-p node)
      (node-variable-p (node-application-rator node))
      (tc:lookup-instance-by-codegen-sym env (node-variable-value (node-application-rator node)) :no-error t))
     t)

    (t
     nil)))

(defstruct candidate-manager
  "A CANDIDATE-MANAGER is used to deduplicate COMPILE-CANDIDATE
recompilation, and also maintains a stack of uncompiled candidates."
  (map   (make-hash-table :test #'equalp) :type hash-table            :read-only t)
  (stack nil                              :type compile-candidate-list            ))

(defun candidate-manager-get (candidate-manager candidate)
  "Returns name the given to a compile-candidate"
  (declare (type candidate-manager candidate-manager)
           (type compile-candidate candidate)
           (values symbol))
  (let ((name (gethash candidate (candidate-manager-map candidate-manager))))
    name))

(defun candidate-manager-push (candidate-manager candidate package)
  "Push a new candidate to be compiled. This function is idempotent."
  (declare (type candidate-manager candidate-manager)
           (type compile-candidate candidate)
           (values))

  (when (candidate-manager-get candidate-manager candidate)
    (return-from candidate-manager-push nil))

  (let ((new-name
          (gentemp (concatenate 'string (symbol-name (compile-candidate-name candidate)) "#")
                   package)))

    (setf (gethash candidate (candidate-manager-map candidate-manager)) new-name))

  (push candidate (candidate-manager-stack candidate-manager))

  nil)

(defun candidate-manager-pop (candidate-manager)
  "Returns a candidate that has been queued for compilation"
  (declare (type candidate-manager candidate-manager)
           (values (or null compile-candidate)))
  (pop (candidate-manager-stack candidate-manager)))

(defun valid-candidate-p (name args bound-variables env)
  "Returns the candidate for a given function and arguments. Returns nil if a given application cannot be a candidate."
  (declare (type symbol name)
           (type node-list args)
           (type util:symbol-list bound-variables)
           (type tc:environment env)
           (values (or compile-candidate null) &optional))

  ;; Only functions with known code are valid candidates
  (unless (tc:lookup-code env name :no-error t)
    (return-from valid-candidate-p nil))

  (let ((new-args (loop :for node :in args
                    :if (and (node-can-be-propidated node env) (node-free-p node bound-variables))
                      :collect node
                    :else
                      :collect '@@unpropagated)))

    ;; Candidates must have at least one propagatable argument
    (unless (some #'propagatable-p new-args)
      (return-from valid-candidate-p nil))

    ;; Fully propigating arguments could change the ordering of side effects
    ;; Exit early to avoid this
    ;;
    ;; Unless the function is known to have more arguments than are being applied
    (when (every #'propagatable-p new-args)
      (let ((function (tc:lookup-function env name :no-error t)))
        (unless function
          (return-from valid-candidate-p nil))
        (unless (> (tc:function-env-entry-arity function) (length new-args))
          (return-from valid-candidate-p nil))))

    (make-compile-candidate
     :name name
     :args new-args)))

(defun compile-candidate (candidate node env)
  "Recompile a given CANDIDATE replacing propigatable arguments with
their known values."
  (declare (type compile-candidate candidate)
           (type node-abstraction node)
           (type tc:environment env)
           (values node &optional))

  (let* ((ast-subs nil)

         (new-type (node-type node))

         (arg-tys nil)

         (subs nil)

         (new-vars (loop :for var :in (node-abstraction-vars node)
                         :for arg :in (compile-candidate-args candidate)

                         :if (eq arg '@@unpropagated)
                           :collect (progn
                                      (push (tc:function-type-from new-type) arg-tys)
                                      var)
                         :else
                           :do (progn
                                 (setf subs (tc:unify subs (tc:function-type-from new-type) (node-type arg)))
                                 (push (make-ast-substitution :from var :to arg) ast-subs))

                         :do (setf new-type (tc:function-type-to new-type))))

         (retained-args
           (if (< #1=(length (compile-candidate-args candidate)) #2=(length (node-abstraction-vars node)))
               (subseq (node-abstraction-vars node) #1# #2#)
               nil))

         (new-node
           (let* ((candidate-args (compile-candidate-args candidate))

                  (abstraction-vars (node-abstraction-vars node)))

             (cond
               ;; Function is over applied
               ((and (null new-vars)
                     (> (length candidate-args) (length abstraction-vars)))
                (let* ((over-args (- (length (compile-candidate-args candidate)) (length (node-abstraction-vars node))))
                       (arg-tys (subseq (tc:function-type-arguments (node-type node))
                                        (1- over-args)
                                        (length (compile-candidate-args candidate))))
                       (args (mapcar (lambda (_) (declare (ignore _)) (gensym)) arg-tys))

                       (node (tc:apply-substitution
                              subs
                              (apply-ast-substitution
                               ast-subs
                               (make-node-abstraction
                                :type (node-type (node-abstraction-subexpr node))
                                :vars args
                                :subexpr (make-node-application
                                          :type (tc:make-function-type*
                                                 (subseq (tc:function-type-arguments (node-type (node-abstraction-subexpr node))) (length args))
                                                 (tc:function-return-type (node-type (node-abstraction-subexpr node))))
                                          :rator (node-abstraction-subexpr node)
                                          :rands (loop :for ty :in arg-tys
                                                       :for arg :in args
                                                       :collect (make-node-variable :type ty :value arg))))))))
                  node))

               ;; Function is under applied
               ((and (null new-vars)
                     (< (length candidate-args) (length abstraction-vars)))
                (let* ((remaining-args (subseq abstraction-vars (length candidate-args)))

                       (node-ty (node-type node))

                       (arg-tys (subseq (tc:function-type-arguments node-ty) (length candidate-args)))

                       (ret-ty (tc:function-return-type node-ty)))
                  (tc:apply-substitution
                   subs
                   (make-node-abstraction
                    :type (tc:make-function-type* arg-tys ret-ty)
                    :vars remaining-args
                    :subexpr (apply-ast-substitution ast-subs (node-abstraction-subexpr node))))))

               ;; Function is applied
               (t
                (tc:apply-substitution
                 subs
                 (make-node-abstraction
                  :type (tc:make-function-type*
                         (reverse arg-tys)
                         new-type)
                  :vars (append new-vars retained-args)
                  :subexpr (apply-ast-substitution ast-subs (node-abstraction-subexpr node)))))))))

    (typecheck-node new-node env)

    new-node))

(defun resolve-var (node resolve-table)
  "Lookup a given node in the RESOLVE-TABLE, returning it if it exists.
Otherwise returns the origional node. This function is used to
propigate dictionaries that have been moved by the hoister."
  (declare (type node node)
           (type hash-table resolve-table)
           (values node))

  (unless (node-variable-p node)
    (return-from resolve-var node))

  (let ((resolved (gethash (node-variable-value node) resolve-table)))
    (if resolved
        resolved
        node)))

(defun candidate-selection (node candidate-manager resolve-table package env)
  (declare (type node node)
           (type candidate-manager candidate-manager)
           (type hash-table resolve-table)
           (type package package)
           (type tc:environment env))
  (labels ((validate-candidate (node &key bound-variables &allow-other-keys)
             (let ((name (node-rator-name node))
                   (rands (node-rands node)))

               (unless name
                 (return-from validate-candidate nil))

               (let ((candidate
                       (valid-candidate-p
                        name
                        (loop :for rand :in rands
                              :collect (resolve-var rand resolve-table))
                        bound-variables
                        env)))

                 (unless candidate
                   (return-from validate-candidate nil))

                 (candidate-manager-push candidate-manager candidate package)

                 nil))))

    (traverse
     node
     (list
      (cons :direct-application #'validate-candidate)
      (cons :application #'validate-candidate))
     nil))
  (values))


(defun rewrite-callsites (node candidate-manager resolve-table env)
  (declare (type node node)
           (type candidate-manager candidate-manager)
           (type hash-table resolve-table)
           (type tc:environment env)
           (values node &optional))

  (labels ((apply-candidate (node &key bound-variables &allow-other-keys)
             (let ((name (node-rator-name node))
                   (rands (node-rands node)))

               (unless name
                 (return-from apply-candidate nil))

               (let ((candidate (valid-candidate-p
                                 name
                                 (loop :for rand :in rands
                                       :collect (resolve-var rand resolve-table))
                                 bound-variables
                                 env)))

                 (unless candidate
                   (return-from apply-candidate nil))

                 (let* ((function-name (candidate-manager-get candidate-manager candidate))

                        (new-type (node-rator-type node))

                        (arg-tys nil) 

                        (args (loop
                                :for arg :in (node-rands node)
                                :for candidate-arg :in (compile-candidate-args candidate)

                                :when (eq candidate-arg '@@unpropagated)
                                  :collect (progn
                                             (push (node-type arg) arg-tys)
                                             arg)

                                :do (setf new-type (tc:function-type-to new-type)))))

                   (if (null args)
                       (make-node-variable
                        :type new-type
                        :value function-name)
                       (make-node-application
                        :type (node-type node)
                        :rator (make-node-variable
                                :type (tc:make-function-type*
                                       (reverse arg-tys)
                                       new-type)
                                :value function-name)
                        :rands args)))))))

    (traverse
     node
     (list
      (cons :application #'apply-candidate)
      (cons :direct-application #'apply-candidate))
     nil)))

(defun monomorphize (name manager package resolve-table optimize-node env)
  (declare (type symbol name)
           (type candidate-manager manager)
           (type package package)
           (type hash-table resolve-table)
           (type function optimize-node)
           (type tc:environment env)
           (values binding-list &optional))

  (let* ((initial-code (tc:lookup-code env name))

         (binding-group nil))

    (candidate-selection initial-code manager resolve-table package env)
    (push (cons name (rewrite-callsites initial-code manager resolve-table env)) binding-group)

    (loop :for candidate := (candidate-manager-pop manager)
          :while candidate

          :for name := (compile-candidate-name candidate)
          :for code := (tc:lookup-code env name)
          :for new-code := (funcall optimize-node (compile-candidate candidate code env) env)

          :for new-code_ := (progn
                              (candidate-selection new-code manager resolve-table package env)
                              (rewrite-callsites new-code manager resolve-table env))

          :for new-code__ := (funcall optimize-node new-code_ env)

          :do (candidate-selection new-code__ manager resolve-table package env)
          :do (push (cons (candidate-manager-get manager candidate) (rewrite-callsites new-code__ manager resolve-table env))
                    binding-group))

    binding-group))
