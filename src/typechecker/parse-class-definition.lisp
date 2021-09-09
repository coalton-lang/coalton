(in-package #:coalton-impl/typechecker)

;;;
;;; Parsing class defintions
;;;

(defun parse-class-definitions (forms env)
  (declare (type list forms)
           (type environment env)
           (values ty-class-list environment))

  ;; Parse out class definitions and sort them by superclass dependencies
  (let ((class-deps nil) ; DAG as list of (CLASS SUPERCLASS*) for topological sorting
        (class-forms nil)               ; List of (CLASS FORM)
        )
    (dolist (form forms)
      (unless (and (listp form)
                   (<= 2 (length form))
                   (eql 'coalton:define-class (first form)))
        (error "Malformed DEFINE-CLASS form ~A" form))
      ;; Parse out the type class signature to form dependency graph of superclasses
      (multiple-value-bind (class-context class-predicate class-tyvars subs)
          (parse-class-signature env (second form) nil nil :allow-unknown-classes t)

        (when (some (lambda (pred)
                      (eql (ty-predicate-class pred)
                           (ty-predicate-class class-predicate)))
                    class-context)
          (error 'cyclic-class-definitions-error :classes (list (ty-predicate-class class-predicate))))

        (let* ((class-name (ty-predicate-class class-predicate))
               (superclass-names (mapcar #'ty-predicate-class class-context))
               (docstring? (stringp (third form)))
               ;; Also parse out constraints of all methods
               (class-method-deps
                 (mapcan (lambda (form)
                           (unless (and (listp form)
                                        (= 2 (length form))
                                        (symbolp (first form)))
                             (error "Malformed DEFINE-CLASS method form ~A" form))
                           (qualified-ty-predicates
                            (parse-qualified-type-expr env (second form) class-tyvars subs :allow-unknown-classes t)))
                         (if docstring?
                             (cdddr form)
                             (cddr form))))
               (class-method-dep-names (mapcar #'ty-predicate-class class-method-deps)))
          ;; Add the dependency group to the DAG
          (push (append (list class-name) superclass-names class-method-dep-names)
                class-deps)
          ;; Add the class form for later use
          (push (cons class-name form)
                class-forms))))

    ;; Ensure we know about all of the superclasses and remove any
    ;; references to known classes from the DAG (tarjan scc will fail
    ;; otherwise).
    (setf class-deps
          (loop :for class-dep :in class-deps
                :collect (cons (car class-dep)
                               (mapcan (lambda (dep)
                                         (cond
                                           ;; If the dep is part of the current definitions then allow it
                                           ((member dep class-deps :key #'car)
                                            (list dep))
                                           ;; Else if we know about this class in the environment then remove it
                                           ((lookup-class env dep :no-error t)
                                            nil)
                                           ;; Otherwise, signal an error
                                           (t
                                            (error "Unknown class ~S in definition of class ~S" dep (car class-dep)))))
                                       (cdr class-dep)))))

    ;; Perform a topological sort of classes to ensure contexts can be resolved
    (let* ((sorted-classes (reverse (tarjan-scc class-deps)))
           (sorted-forms
             (loop :for class-group :in sorted-classes
                   :collect (progn
                              (unless (= 1 (length class-group))
                                (error 'cyclic-class-definitions-error :classes class-group))
                              (find (first class-group) class-forms :key #'car)))))

      ;; Now we can go through and re-parse and add classes to the environment
      (let ((classes (loop :for (class-name . form) :in sorted-forms
                           :collect
                           (multiple-value-bind (class methods docstring)
                               (parse-class-definition form env)


                             ;; Add class to environment
                             (setf env (set-class env (ty-class-name class) class))

                             ;; Add method types to environment
                             (dolist (method methods)
                               (setf env (set-value-type env (car method) (cdr method)))

                               (setf env (set-name env (car method)
                                                   (make-name-entry
                                                    :name (car method)
                                                    :type :method
                                                    :docstring nil
                                                    :location (or *compile-file-pathname* *load-truename*))))

                               (if (function-type-p (qualified-ty-type (fresh-inst (cdr method))))
                                 (let ((arity (function-type-arity
                                               (qualified-ty-type (fresh-inst (cdr method))))))
                                   (setf env (set-function env (car method)
                                                           (make-function-env-entry
                                                            :name (car method)
                                                            :arity arity))))
                                 (setf env (unset-function env (car method)))))

                             class))))

        (values
         classes
         env)))))

(defun parse-class-definition (form env)
  (declare (type list form)
           (type environment env)
           (values ty-class scheme-binding-list &optional))
  (unless (and (listp form)
               (<= 2 (length form))
               (eql 'coalton:define-class (first form)))
    (error "Malformed DEFINE-CLASS form ~A" form))

  (with-parsing-context ("class definition ~A" form)
    (multiple-value-bind (class-context class-predicate class-tyvars subs)
        (parse-class-signature env (second form) nil nil)

      (let* ((class-name (ty-predicate-class class-predicate))
             (class-codegen-sym (alexandria:format-symbol (symbol-package class-name) "CLASS/~A" class-name)))

        (let* ((disallowed-type-vars (mapcar #'cadr class-tyvars))
               (docstring (and (stringp (third form))
                               (third form)))
               (class-methods (mapcar (lambda (form)
                                        (unless (and (listp form)
                                                     (= 2 (length form))
                                                     (symbolp (first form)))
                                          (error "Malformed DEFINE-CLASS method form ~A" form))
                                        (cons (first form)
                                              (multiple-value-bind (parsed type-vars new-subs)
                                                  (parse-qualified-type-expr env (second form) class-tyvars subs :additional-class-predicates (list class-predicate))
                                                (setf class-tyvars type-vars
                                                      subs new-subs)
                                                (apply-substitution subs
                                                                    (quantify (remove-if
                                                                               (lambda (x) (member x (type-variables (apply-substitution subs disallowed-type-vars)) :test #'equalp))
                                                                               (type-variables (mapcar #'cadr class-tyvars)))
                                                                              parsed)))))
                                      (if docstring
                                          (cdddr form)
                                          (cddr form))))
               (class-predicate (apply-substitution subs class-predicate))
               (class-context (apply-substitution subs class-context))
               (superclass-dict (loop :for super :in class-context
                                      :for i :from 0
                                      :collect (cons super
                                                     (alexandria:format-symbol
                                                      (symbol-package class-name)
                                                      (format nil "SUPER-~D" i)))))
               (class (ty-class
                       class-name
                       class-predicate
                       class-context
                       class-methods
                       class-codegen-sym
                       superclass-dict
                       docstring
                       (or *compile-file-pathname* *load-truename*)))

               ;; Create a ENV with our new class defined so that reduce-context will work
               (env (set-class env class-name class))

               (method-context (append (list class-predicate) class-context))
               (qualified-methods
                 (loop :for (method-name . unqualified-method-type) :in (ty-class-unqualified-methods class)
                       :collect (with-parsing-context ("method ~S" method-name)
                                  (let* ((fresh-method-type (fresh-inst unqualified-method-type)))
                                    ;; Ensure that the class type variables
                                    ;; do not occur in the method constraint
                                    (dolist (tyvar class-tyvars)
                                      (when (member (tvar-tyvar (cadr tyvar))
                                                    (type-variables (qualified-ty-predicates fresh-method-type))
                                                    :test #'equalp)
                                        (coalton-impl/ast::error-parsing form "class type variable ~S cannot appear in method constraints" (car tyvar))))

                                    (let* ((qualified-method-type (qualify
                                                                   (reduce-context env
                                                                    (append method-context
                                                                            (qualified-ty-predicates fresh-method-type)))
                                                                   (qualified-ty-type fresh-method-type)))
                                           (method-type (quantify (type-variables qualified-method-type)
                                                                  qualified-method-type)))
                                      (cons method-name method-type)))))))
          (values class qualified-methods))))))

(defun parse-class-signature (env top-expr type-vars subs &key allow-unknown-classes)
  (declare (type environment env)
           (values ty-predicate-list ty-predicate list))
  (cond
    ;; If the expression is a list and contains => then it has constraints
    ((and (listp top-expr)
          (some #'coalton-double-arrow-p top-expr))
     ;; Split the expression into parts before and after the arrow
     (let ((subseqs (split-sequence:split-sequence-if #'coalton-double-arrow-p top-expr)))
       (unless (and (= 2 (length subseqs))
                    (= 1 (length (second subseqs))))
         (error-parsing-type top-expr "Malformed constrained type class"))
       ;; If the first member of the predicates is a list then we can assume there are multiple to parse.
       (let* ((preds (if (listp (first (first subseqs)))
                         (loop :for pred-expr :in (first subseqs)
                               :collect (multiple-value-bind (pred new-type-vars new-subs)
                                            (parse-type-predicate env pred-expr type-vars subs :allow-unknown-classes allow-unknown-classes)
                                          (setf type-vars new-type-vars
                                                subs new-subs)
                                          pred))
                         (multiple-value-bind (pred new-type-vars new-subs)
                             (parse-type-predicate env (first subseqs) type-vars subs :allow-unknown-classes allow-unknown-classes)
                           (setf type-vars new-type-vars
                                 subs new-subs)
                           (list pred))))
              (class-pred (multiple-value-bind (pred new-type-vars new-subs)
                              (parse-type-predicate env (first (second subseqs)) type-vars subs :allow-unknown-classes t)
                            (setf type-vars new-type-vars
                                  subs new-subs)
                            pred)))

         (values preds class-pred type-vars subs))))
    ;; Otherwise parse as a type
    (t
     (multiple-value-bind (class-pred type-vars subs)
         (parse-type-predicate env top-expr type-vars subs :allow-unknown-classes t)
       (values nil class-pred type-vars subs)))))
