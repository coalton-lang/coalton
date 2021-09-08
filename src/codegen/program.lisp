(in-package #:coalton-impl/codegen)

(defun reshuffle-definitions (things)
  ;; This is an extremely gross function that just makes Lisp compilation order
  ;; nicer. In particular, it puts:
  ;;
  ;;    - First, all inline decls (they need to be caught early so the
  ;;      compiler can save them)
  ;;
  ;;    - Second, structure definitions
  ;;
  ;;    - Third, any other declaims (probably FREEZE-TYPEs)
  ;;
  ;;    - Fourth, stub definitions
  ;;
  ;;    - Fifth, anything that doesn't fit in the aforementioned
  ;;      categories.
  ;;
  ;;    - Sixth and finally, any SETFs of LOAD-TIME-VALUE definitions.
  ;;
  ;; This function is pretty brittle. If we change codegen details,
  ;; this has to change.
  ;;
  ;; Of important note, this function DOES NOT change the relative
  ;; ordering of things within each group.
  (labels ((sub-struct-p (thing)
             (and (eq 'cl:defstruct (car thing))
                  (find ':INCLUDE (cdadr thing) :key #'car :test #'eq)))
           (setf-ltv-p (thing)
             (and (or (eq 'cl:setf (car thing))
                      (eq 'cl:setq (car thing)))
                  (= 3 (length thing))
                  (typep (caddr thing) '(cons (member load-time-value))))))
    (loop :for thing :in things
          ;; (DEFSTRUCT (FOO ... (:INCLUDE ...
          :if (and (typep thing '(cons (member cl:defstruct)))
                   (sub-struct-p thing))
            :collect thing :into defsubstructs
          ;; (DEFSTRUCT FOO ...
          :else :if (and (typep thing '(cons (member cl:defstruct)))
                         (not (sub-struct-p thing)))
                  :collect thing :into defstructs
          ;; (DECLAIM (INLINE ...
          :else :if (typep thing '(cons
                                   (member cl:declaim)
                                   (cons
                                    (cons (member cl:inline cl:notinline)))))
                  :collect thing :into declaims-inline
          ;; (DECLAIM ...
          :else :if (typep thing '(cons (member cl:declaim)))
                  :collect thing :into declaims-other
          ;; (DEFINE-* FOO ':|@@unbound@@|)
          :else :if (or (member ':|@@unbound@@| thing)
                        (member '':|@@unbound@@| thing :test 'equal))
                  :collect thing :into deflex-stubs
          :else :if (setf-ltv-p thing)
                  :collect thing :into setfs-ltv
          ;; Other things
          :else :collect thing :into others
          :finally (return (append
                            declaims-inline
                            (unless (null defstructs)
                              (list
                               `(eval-when (:compile-toplevel :load-toplevel :execute)
                                  ,@defstructs)))
                            (unless (null defsubstructs)
                              (list
                               `(eval-when (:compile-toplevel :load-toplevel :execute)
                                  ,@defsubstructs)))
                            declaims-other
                            deflex-stubs
                            others
                            setfs-ltv)))))

(defun codegen-program (types bindings sccs classes instance-definitions docstrings env)
  (declare (type type-definition-list types)
	   (type typed-binding-list bindings)
	   (type list sccs)
	   (type instance-definition-list instance-definitions)
	   (type ty-class-list classes)
	   (type environment env))
  (let* ((optimizer (make-optimizer env))
	 (bindings (optimize-bindings optimizer bindings)))
    `(progn
       ;; Define types
       ,@(reshuffle-definitions (mapcan #'compile-type-definition types))

       ;; Define typeclasses
       ,@(reshuffle-definitions (compile-class-definitions classes env))


       ;; Define ...
       ,@(reshuffle-definitions (append
                                 ;; ... instance structs
                                 (compile-instance-definitions instance-definitions optimizer)
                                 ;; ... functions and variables
                                 (compile-toplevel-sccs bindings sccs env)))

       ;; Emit documentation
       ,@(compile-docstring-forms docstrings)

       ;; Emit a dummy value at the end so that REPL return values
       ;; don't look strange.
       (values))))

(defun compile-docstring-forms (docstrings)
  (loop :for (name docstring type) :in docstrings
        :append
	(unless (equalp docstring nil)
            (ecase type
              (:variable
               `((setf (documentation ',name 'variable) ,docstring)))
              (:function
               `((setf (documentation ',name 'variable) ,docstring
			(documentation ',name 'function) ,docstring)))
              (:type
               `((setf (documentation ',name 'type) ,docstring)))))))

(defun update-function-env (toplevel-bindings env)
  (declare (type typed-binding-list toplevel-bindings)
	   (type environment env)
	   (values environment))
  (multiple-value-bind (toplevel-functions toplevel-values)
      (split-binding-definitions toplevel-bindings)
    (loop :for (name . arity) :in toplevel-functions
	  :do
	     (setf env
		   (set-function
		    env
		    name
		    (make-function-env-entry
		     :name name
		     :arity arity))))
    (loop :for name :in toplevel-values
	  :do
	     (setf env (unset-function env name))))
  env)

(defun compile-function (name vars type return-type node env)
  (declare (type symbol name)
	   (type coalton-impl/typechecker::scheme-binding-list vars)
	   (type ty-scheme type)
	   (type ty-scheme return-type)
	   (type typed-node node)
	   (type environment env))
  (let* ((var-names (mapcar #'car vars))

	 (preds (reduce-context
		 env
		 (remove-duplicates (remove-if #'static-predicate-p (scheme-predicates type))
				    :test #'equalp)))

	 (dict-context (mapcar (lambda (pred) (cons pred (gensym))) preds))

	 (dict-types (mapcar (lambda (dict-context)
			       (cons
				(ty-class-codegen-sym (lookup-class env (ty-predicate-class (car dict-context))))
				(cdr dict-context)))
			     dict-context))

	 (params (append (mapcar #'cdr dict-context) var-names)))
    `((coalton-impl::define-global-lexical ,name ':|@@unbound@@|)
      (defun ,name ,params
	(declare (ignorable ,@params)
		 ,@(when *emit-type-annotations*
		     `(,@(mapcar (lambda (var) `(type ,(lisp-type (cdr var)) ,(car var))) vars)
		       ,@(mapcar (lambda (dict) `(type ,(car  dict) ,(cdr dict))) dict-types)
		       (values ,(lisp-type return-type) &optional))))                       
	,(compile-expression node dict-context env))
      (setf ,name (load-time-value ,(construct-function-entry `#',name (+ (length vars) (length preds))))))))

(defun compile-toplevel-sccs (toplevel-bindings sccs env)
  (loop :for scc :in sccs
	:for scc-typed-bindings
	  := (mapcar
	      (lambda (b)
		(find b toplevel-bindings :key #'car))
	      scc)
	:append
	(cond
	  ((every #'coalton-impl/typechecker::typed-node-abstraction-p (mapcar #'cdr scc-typed-bindings))
	   ;; Functions
           `(,@(loop :for (name . node) :in scc-typed-bindings
		     :for vars := (typed-node-abstraction-vars node)
		     :for type := (typed-node-type node)
		     :for subexpr := (typed-node-abstraction-subexpr node)
		     :for return-type := (typed-node-type (typed-node-abstraction-subexpr node))
		     :append (compile-function name vars type return-type subexpr env))))

	  ((and (= 1 (length scc-typed-bindings))
		t ;; TODO: We need to check that the variable is non-self-recursive using free-variables
		)
	   ;; Variables
	   (let* ((b (first scc-typed-bindings))
                  (preds (reduce-context env (scheme-predicates (typed-node-type (cdr b))))))
             (if (not (every #'static-predicate-p  preds))
                 `(,@(compile-function (car b) nil (typed-node-type (cdr b)) (typed-node-type (cdr b)) (cdr b) env))
	         `((coalton-impl::define-global-lexical ,(car b) ':|@@unbound@@|)
                   (setf ,(car b) (load-time-value
                                   ,(compile-expression (cdr b) nil env)))))))
	  (t (error "")))))


(alexandria:define-constant keyword-package
  (find-package "KEYWORD") :test #'equalp)

(defun compile-class-definitions (classes env)
  (declare (type ty-class-list classes)
	   (type environment env)
	   (values list))
  (loop :for class :in classes
	:for name := (ty-class-name class)
	:for method-decs
	  := (mapcar (lambda (m)
		       `(,(car m)
			 (error ,(format nil "Required method ~S" (car m)))
			 :type t
			 :read-only t))
		     (ty-class-unqualified-methods class))
        :for superclass-decs := (loop :for (superclass-pred . field-name) :in (ty-class-superclass-dict class)
                                      :for superclass := (lookup-class env (ty-predicate-class superclass-pred))
                                      :collect `(,field-name
                                                 (error ,(format nil "Required superclass ~S" field-name))
                                                 :type ,(ty-class-codegen-sym superclass)
                                                 :read-only t))
	:for package := (symbol-package name)
	:for codegen-name := (ty-class-codegen-sym class)
	:for method-funs := (mapcan (lambda (m)
				      (make-method-fun m package class))
				    (ty-class-unqualified-methods class))
        
	:append
	`((cl:defstruct
	      (,codegen-name (:constructor ,codegen-name))
	    ,@method-decs
            ,@superclass-decs)
	  ,@method-funs
	  #+sbcl
	  (declaim (sb-ext:freeze-type ,codegen-name)))))

(defun make-method-fun (m package class)
  (let* ((arity (coalton-impl/typechecker::function-type-arity
		 (coalton-impl/typechecker::qualified-ty-type
		  (coalton-impl/typechecker::fresh-inst (cdr m)))))
	 (params
	   (loop :for i :from 0 :below arity
		 :collect (alexandria:format-symbol package "_~A" i)))
	 (class-codegen-sym (ty-class-codegen-sym class))
	 (method-accessor (alexandria:format-symbol (symbol-package class-codegen-sym) "~A-~A" class-codegen-sym (car m))))
    ;; TODO: add type annotations
    `((declaim (inline ,(car m)))
      (defun ,(car m) (dict ,@params)
        ,(if (null params)
             `(,method-accessor dict)
             (apply #'apply-function-entry `(,method-accessor dict) params)))
      ;; Generate the wrapper functions
      (coalton-impl::define-global-lexical ,(car m)
          ,(construct-function-entry
            `#',(car m)
            (+ arity 1) ; We need a function of the arity + 1 to account for DICT
            )))))

(defun compile-instance-definitions (instance-definitions optimizer)
  (declare (type instance-definition-list instance-definitions)
	   (type optimizer optimizer)
	   (values list))
  (loop :with env := (optimizer-env optimizer)
        :for instance :in instance-definitions
        :for class-name := (instance-definition-class-name instance)
        :for predicate := (instance-definition-predicate instance)
        :for methods := (instance-definition-methods instance)
        :for codegen-sym := (instance-definition-codegen-sym instance)

        :for class := (lookup-class env class-name)
        :for class-codegen-sym := (ty-class-codegen-sym class)

        :for context := (instance-definition-context instance)
        :for context-dict := (mapcar (lambda (pred)
                                       (cons pred (gensym)))
                                     context)
        :for context-params := (mapcar #'cdr context-dict)
        :for dict-types := (mapcar (lambda (dict-context)
                                     (cons
                                      (ty-class-codegen-sym (lookup-class env (ty-predicate-class (car dict-context))))
                                      (cdr dict-context)))
                                   context-dict)

        
        :for codegen-methods
          := (mapcan
              (lambda (m)
                (let ((method-body (optimize-node optimizer (cdr m))))
                  
                  `(,(intern (symbol-name (car m)) keyword-package)
                    ,(if (coalton-impl/typechecker::typed-node-abstraction-p method-body)
                         (let ((vars (mapcar #'car (typed-node-abstraction-vars method-body)))
                               (subexpr (typed-node-abstraction-subexpr method-body)))
                           (construct-function-entry
                            `(lambda ,vars
                               (declare
                                (ignorable ,@vars)
                                ,@(when *emit-type-annotations*
                                    (mapcar (lambda (v) `(type ,(lisp-type (cdr v)) ,(car v) ))
                                            (typed-node-abstraction-vars method-body))))
                               ,(compile-expression subexpr context-dict env))
                            (length vars)))
                         (compile-expression (cdr m) context-dict env)))))
              methods)

        :for pred-subs := (coalton-impl/typechecker::predicate-match (ty-class-predicate class) predicate)
        :for codegen-superclasses
          := (loop :for (superclass-pred . field-name) :in (ty-class-superclass-dict class)
                   :for superclass := (lookup-class env (ty-predicate-class superclass-pred))
                   :append
                   `(,(intern (symbol-name field-name) keyword-package)
                     ,(lookup-dict (coalton-impl/typechecker::apply-substitution pred-subs superclass-pred)
                                   context-dict env)))

        :collect
        `(global-vars:define-global-var ,codegen-sym ':|@@unbound@@|)
        :collect
        (if (null context)
            `(cl:setf
              ,codegen-sym
              (load-time-value
               (,class-codegen-sym
                ,@codegen-methods
                ,@codegen-superclasses)))
            `(cl:defun ,codegen-sym ,context-params
               (declare
                (ignorable ,@context-params)
                ,@(when *emit-type-annotations*
                    (mapcar (lambda (dict-ctx) `(type ,(car dict-ctx) ,(cdr dict-ctx)))
                            dict-types)))
               (,class-codegen-sym
                ,@codegen-methods
                ,@codegen-superclasses))))
  )
 
