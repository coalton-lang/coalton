(defpackage #:coalton-impl/codegen/resolve-instance
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/transformations
   #:pred-type)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:pred-context                       ; TYPE
   #:pred-type                          ; FUNCTION
   #:register-static-dict-resolver      ; FUNCTION
   #:resolve-dict                       ; FUNCTION
   #:resolve-static-dict                ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/resolve-instance)

(defun pred-context-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons tc:ty-predicate symbol))) x)))

(deftype pred-context ()
  '(satisfies pred-context-p))

(defvar *static-dict-resolvers* nil
  "Symbols naming additional static dictionary resolvers.")

(defun register-static-dict-resolver (resolver)
  (declare (type symbol resolver)
           (values symbol &optional))
  (pushnew resolver *static-dict-resolvers* :test #'eq)
  resolver)

(defun resolve-extra-static-dict (pred context env)
  (declare (type tc:ty-predicate pred)
           (type pred-context context)
           (values (or null node) boolean &optional))
  (dolist (resolver *static-dict-resolvers* (values nil nil))
    (multiple-value-bind (dict foundp)
        (funcall resolver pred context env)
      (when foundp
        (return (values dict t))))))

(defvar *static-dict-resolution-stack* nil
  "Active (INSTANCE . PRED) pairs during static dictionary construction.")

(defvar *static-dict-fixed-vars* nil
  "Type variables fixed by the root dictionary request and its given context.")

(defvar *static-dict-max-given-height* 0
  "Maximum predicate height in the root dictionary request and its given context.")

(defun resolve-static-dict (pred context env)
  "Attempt to resolve PRED from static instances"
  (declare (type tc:ty-predicate pred)
           (type pred-context context)
           (values node &optional))

  ;; Lookup the predicate as a static instance
  (let* ((context-preds (when (null *static-dict-resolution-stack*)
                          (loop :for (ctx-pred . nil) :in context
                                :append (tc:superclass-closure env ctx-pred))))
         (fixed-vars (if *static-dict-resolution-stack*
                         *static-dict-fixed-vars*
                         (tc:type-variables (cons pred context-preds))))
         (max-given-height (if *static-dict-resolution-stack*
                               *static-dict-max-given-height*
                               (loop :for p :in (cons pred context-preds)
                                     :maximize (tc:predicate-height p))))
         (*static-dict-fixed-vars* fixed-vars)
         (*static-dict-max-given-height* max-given-height)
         (instance (tc:lookup-class-instance env pred :no-error t)))
    (cond
      ((and instance
            (not (tc:instance-recursion-blocked-p
                  env instance pred *static-dict-resolution-stack* fixed-vars max-given-height)))
       ;; The provider fixes the parameters; the caller supplies evidence.
       (let* ((*static-dict-resolution-stack* (acons instance pred *static-dict-resolution-stack*))
              (constraints (tc:instantiate-instance-context instance pred))
              (subs (nth-value 1 (tc:solve-fundeps env constraints nil)))
              (parameters (tc:apply-substitution subs constraints)))
         (if (null parameters)
             (make-node-global-variable
              :type (pred-type pred env)
              :value (tc:ty-class-instance-codegen-sym instance))

             (make-node-application
              :type (pred-type pred env)
              :properties '()
              :rator (make-node-global-variable
                      :type (tc:make-function-type*
                             (mapcar (lambda (p) (pred-type p env)) parameters)
                             (pred-type pred env))
                      :value (tc:ty-class-instance-codegen-sym instance))
              :rands (mapcar (lambda (p) (resolve-dict p context env)) parameters)))))
      (t
       ;; No explicit instance matched. Give compiler-registered resolvers a
       ;; chance to synthesize a dictionary for internal type shapes, then
       ;; fall back to the ordinary instance lookup to signal the usual error.
       (multiple-value-bind (dict foundp)
           (resolve-extra-static-dict pred context env)
         (cond
           (foundp
            dict)
           (t
            (if (tc:lookup-class-instances
                 env
                 (tc:ty-predicate-class pred)
                 :no-error t)
                (error "Unknown instance for predicate ~S" pred)
                (error "Undefined key ~a" (tc:ty-predicate-class pred))))))))))


(defun superclass-accessors (pred ctx-pred env equations)
  (let ((class (tc:lookup-class env (tc:ty-predicate-class ctx-pred))))
    (loop :for super :in (tc:context-superclasses class ctx-pred)
          :for (nil . field) :in (tc:ty-class-superclass-dict class)
          :for accessor := (alexandria:format-symbol (symbol-package field) "CLASS/~A-~A"
                                                     (tc:ty-class-name class) field)
          :thereis (lookup-pred pred super ctx-pred accessor env equations))))

(defun lookup-pred (pred ctx-pred sub-pred method-accessor env equations)
  "Search for PRED in CTX-PRED recursing through superclasses if necessary"
  (declare (type tc:ty-predicate pred)
           (type tc:ty-predicate ctx-pred)
           (type tc:ty-predicate sub-pred)
           (type (or null symbol) method-accessor)
           (type tc:environment env)
           (values list &optional))

  (when (tc:given-predicate= pred ctx-pred equations)
    (return-from lookup-pred (list (make-node-global-variable
                                    :type (tc:make-function-type
                                           (pred-type sub-pred env)
                                           (pred-type pred env))
                                    :value method-accessor))))

  (let ((superclass-ret (superclass-accessors pred ctx-pred env equations)))

    (when superclass-ret
      (cons
       (make-node-global-variable
        :type (tc:make-function-type
               (pred-type sub-pred env)
               (pred-type ctx-pred env))
        :value method-accessor)
       superclass-ret))))

(defun lookup-pred-base (pred ctx-pred ctx-name env equations)
  (when (tc:given-predicate= pred ctx-pred equations)
    (return-from lookup-pred-base
      (list (make-node-local-variable
             :type (pred-type pred env)
             :value ctx-name))))

  (let ((superclass-ret (superclass-accessors pred ctx-pred env equations)))
    (when superclass-ret
      (cons
       (make-node-local-variable
        :type (pred-type ctx-pred env)
        :value ctx-name)
       superclass-ret))))

(defun pred-from-context (name context)
  "Lookup the predicate called NAME in CONTEXT"
  (declare (type symbol name)
           (type pred-context context)
           (values tc:ty-predicate &optional))
  (or (car (rassoc name context :test #'equalp))
      (error "Unable to find pred with name ~A~%" name)))

(defun build-call (args)
  (if (= 1 (length args))
      (car args)
      (make-node-field
       :type (tc:function-type-to (node-type (car args)))
       :name (node-variable-value (car args))
       :dict (build-call (cdr args)))))

(defun resolve-context-super (pred context env)
  "Search for PRED in all CONTEXT preds and return a node"
  (declare (type tc:ty-predicate pred)
           (type pred-context context)
           (values (or null node) &optional))

  (flet ((lookup (equations)
           (loop :for (ctx-pred . ctx-sym) :in context
                 :for path := (lookup-pred-base pred ctx-pred ctx-sym env equations)
                 :when path :return (build-call (reverse path)))))
    (or (lookup nil)
        (when (find-if #'tc:pattern-given-p context :key #'car)
          (let ((equations (tc:pattern-given-equations env (mapcar #'car context))))
            (and equations (lookup equations)))))))

(defun resolve-dict (pred context env)
  "Resolve PRED to a node"
  (declare (type tc:ty-predicate pred)
           (type pred-context context)
           (type tc:environment env)
           (values t &optional))
  (or
   (resolve-context-super pred context env)
   (resolve-static-dict pred context env)))
