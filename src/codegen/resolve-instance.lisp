(cl:defpackage #:coalton-impl/codegen/resolve-instance
  (:use
   #:cl
   #:coalton-impl/util
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:pred-context                       ; TYPE
   #:pred-type                          ; FUNCTION
   #:resolve-dict                       ; FUNCTION
   #:resolve-static-dict                ; FUNCTION
   ))

(cl:in-package #:coalton-impl/codegen/resolve-instance)

(defun pred-context-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons tc:ty-predicate symbol))) x)))

(deftype pred-context ()
  '(satisfies pred-context-p))

(defun pred-type (pred env)
  "Returns a type represention of PRED"
  (declare (type tc:ty-predicate pred)
           (values tc:ty tc::ksubstitution-list &optional))
  (let* ((class-entry (tc:lookup-class env (tc:ty-predicate-class pred)))
         (pred-kind tc:+kstar+))

    (loop :for type :in (reverse (tc:ty-predicate-types pred)) :do
      (setf pred-kind (tc:make-kfun :from (tc:kind-of type) :to pred-kind)))

    (tc:apply-type-argument-list
     (tc:make-tycon
      :name (tc:ty-class-codegen-sym class-entry)
      :kind pred-kind)
     (tc:ty-predicate-types pred))))

(defun resolve-static-dict (pred context env)
  "Attempt to resolve PRED from static instances"
  (declare (type tc:ty-predicate pred)
           (type pred-context context)
           (values node &optional))

  ;; Lookup the predicate as a static instance
  (multiple-value-bind (instance subs)
      (tc:lookup-class-instance env pred)

    (let*
        ;; Apply substutions to find the superclass constraints
        ((instance-constraints
           (tc:apply-substitution
            subs
            (tc:ty-class-instance-constraints instance)))

         ;; Apply any fundep substitutions
         (fundep-subs (nth-value 1 (tc:solve-fundeps env instance-constraints subs)))
         (instance-constraints (tc:apply-substitution fundep-subs (tc:ty-class-instance-constraints instance)))

         ;; Generate dicts from those constraints
         (subdicts
           (mapcar
            (lambda (pred)
              (resolve-dict pred context env))
            instance-constraints))

         ;; Find the types of those dicts
         (arg-types (mapcar #'node-type subdicts)))

      (if (null subdicts)
          ;; If the instance has no superclasses return a variable
          (make-node-variable
           :type (pred-type pred env)
           :value (tc:ty-class-instance-codegen-sym instance))

          ;; Otherwise create a new dict at runtime
          (make-node-application
           :type (pred-type pred env) 
           :rator (make-node-variable
                   :type (tc:make-function-type* arg-types (pred-type pred env))
                   :value (tc:ty-class-instance-codegen-sym instance))
           :rands subdicts)))))


(defun superclass-accessors (pred ctx-pred env)
  (let* ((ctx-class (tc:lookup-class env (tc:ty-predicate-class ctx-pred)))
         (subs (tc:predicate-match (tc:ty-class-predicate ctx-class) ctx-pred))
         (ctx-class (tc:apply-substitution subs ctx-class))

         (superclass-ret
           (some
            (lambda (super-dict-entry)
              (let* ((superclass-pred (car super-dict-entry))
                     (superclass-accessor (cdr super-dict-entry))
                     (superclass-accessor-symbol
                       (alexandria:format-symbol
                        (symbol-package superclass-accessor)
                        "CLASS/~A-~A"
                        (tc:ty-predicate-class ctx-pred)
                        superclass-accessor)))
                (lookup-pred pred superclass-pred ctx-pred superclass-accessor-symbol env)))
            (tc:ty-class-superclass-dict ctx-class))))

    superclass-ret))

(defun lookup-pred (pred ctx-pred sub-pred method-accessor env)
  "Search for PRED in CTX-PRED recursing through superclasses if necessary"
  (declare (type tc:ty-predicate pred)
           (type tc:ty-predicate ctx-pred)
           (type tc:ty-predicate sub-pred)
           (type (or null symbol) method-accessor)
           (type tc:environment env)
           (values list &optional))

  (when (tc:type-predicate= pred ctx-pred)
    (return-from lookup-pred (list (make-node-variable
                                    :type (tc:make-function-type
                                           (pred-type sub-pred env)
                                           (pred-type pred env))
                                    :value method-accessor))))

  (let ((superclass-ret (superclass-accessors pred ctx-pred env)))

    (when superclass-ret
      (cons
       (make-node-variable
        :type (tc:make-function-type
               (pred-type sub-pred env)
               (pred-type ctx-pred env))
        :value method-accessor)
       superclass-ret))))

(defun lookup-pred-base (pred ctx-pred ctx-name env)
  (let ((node (make-node-variable
               :type (pred-type ctx-pred env)
               :value ctx-name)))
    (when (tc:type-predicate= pred ctx-pred)
      (return-from lookup-pred-base (list node)))

    (let ((superclass-ret (superclass-accessors pred ctx-pred env)))


      (when superclass-ret
        (cons node superclass-ret)))))

(defun pred-from-context (name context)
  "Lookup the predicate called NAME in CONTEXT"
  (declare (type symbol name)
           (type pred-context context)
           (values tc:ty-predicate &optional))
  (or (car (find name context :key #'cdr :test #'equalp))
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

  (loop :for (ctx-pred . ctx-sym) :in context :do
    (let ((super (lookup-pred-base pred ctx-pred ctx-sym env)))
      (when super
        (return-from resolve-context-super (build-call (reverse super)))))))

(defun resolve-dict (pred context env)
  "Resolve PRED to a node"
  (declare (type tc:ty-predicate pred)
           (type pred-context context)
           (type tc:environment env)
           (values t &optional))
  (or
   (resolve-context-super pred context env)
   (resolve-static-dict pred context env)))
