(in-package #:coalton-impl/typechecker)

;;;
;;; Parsing class defintions
;;;

(defun parse-method-predicates (name type)
  (declare (type symbol name))

  (with-parsing-context ("method ~A" name)
    (unless (some #'coalton-double-arrow-p type)
      (return-from parse-method-predicates nil))

    (let ((subseqs (split-sequence:split-sequence-if #'coalton-double-arrow-p type)))
      (unless (= 2 (length subseqs))
        (error-parsing type "malformed method type"))

      (if (symbolp (car (first subseqs)))
          (list (first subseqs))
          (progn
            (unless (every (alexandria:compose #'symbolp #'car) (first subseqs))
              (error-parsing type "malformed method type"))
            (first subseqs))))))

(defun split-class-signature (signature error-message)
  (declare (type string error-message))

  (let (predicate context)
    (cond
      ;; (define-class (B ... => (C ...) ...)
      ;; (define-class ((B ...) (A ...) ... => (C ...)) ...)
      ((some #'coalton-double-arrow-p signature)
       (let ((subseqs (split-sequence:split-sequence-if #'coalton-double-arrow-p signature)))
         (unless (= 2 (length subseqs))
           (error-parsing signature error-message))

         ;; Support both single and multiple predicate forms
         (if (symbolp (car (first subseqs)))
             (setf context (list (first subseqs)))
             (setf context (first subseqs)))

         ;; Allow both of the following forms
         ;; (... => C :a)
         ;; (... => (C :a))
         (if (symbolp (car (second subseqs)))
             (setf predicate (second subseqs))
             (setf predicate (car (second subseqs))))

         (unless (symbolp (car predicate))
           (error-parsing signature error-message))))

      ;; (define-class (C ...) ...)
      ((symbolp (first signature))
       (setf predicate signature))

      (t
       (error-parsing signature error-message)))

    (values
     predicate
     context)))

(defun parse-class-definitions (forms env)
  (declare (type list forms)
           (type environment env)
           (values ty-class-list environment))

  ;; Parse out class definitions and sort them by superclass dependencies
  (let ((class-deps nil) ; DAG as list of (CLASS SUPERCLASS*) for topological sorting
        (class-forms nil) ; List of (name predicate methods docstring)
        )

    ;; Inital parsing of define-class forms
    ;; * split apart predicate, context, and methods
    ;; * find class dependencies to compute sccs
    (dolist (form forms)
      (unless (and (listp form)
                   (<= 2 (length form))
                   (eql 'coalton:define-class (first form)))
        (error-parsing form "Malformed DEFINE-CLASS"))

      (let ((name nil)
            (context nil)
            (predicate nil)
            (method-predicates nil)
            (methods nil)
            (docstring nil))

        (multiple-value-setq (predicate context) (split-class-signature (second form) "malformed DEFINE-CLASS"))

        (setf name (car predicate))

        (if (stringp (first (nthcdr 2 form)))
            (setf docstring (first (nthcdr 2 form))
                  methods (nthcdr 3 form))
            (setf methods (nthcdr 2 form)))

        (with-parsing-context ("DEFINE-CLASS for class ~A" (car predicate))
          (loop :for method :in methods
                :do (unless (= 2 (length method))
                      (error-parsing method "invalid method definition"))
                :do (unless (symbolp (car method))
                      (error-parsing method "invalid method name ~A" (car method)))
                :do (setf method-predicates (append method-predicates (parse-method-predicates (first method) (alexandria:ensure-list (second method)))))))

        (push (cons name
                    (remove-duplicates
                     (append (mapcar #'car context) (mapcar #'car method-predicates))))
              class-deps)
        (push (list name predicate context methods docstring) class-forms)))

    ;; Ensure we know about all of the superclasses and remove any
    ;; references to known classes from the DAG (tarjan scc will fail
    ;; otherwise).
    (setf class-deps
          (loop :for class-dep :in class-deps
                :do (when (find (car class-dep) (cdr class-dep))
                      (error 'cyclic-class-definitions-error :classes (list (car class-dep))))
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

      ;; Now we can go through to parse and then add classes to the environment
      (let ((classes
              (loop :for (name unparsed-predicate unparsed-context unparsed-methods docstring) :in sorted-forms
                    :collect (with-parsing-context ("DEFINE-CLASS for class ~A" name)
                               (multiple-value-bind (class methods)
                                   (parse-class-definition name unparsed-predicate unparsed-context unparsed-methods docstring env)
                                 (setf env (set-class env (ty-class-name class) class))

                                 ;; Add the class constructor function to the function env
                                 (let ((class-arity (+ (length (ty-class-superclasses class))
                                                       (length (ty-class-unqualified-methods class)))))
                                   (if (not (zerop class-arity))
                                       (setf env (set-function env
                                                               (ty-class-codegen-sym class)
                                                               (make-function-env-entry
                                                                :name (ty-class-codegen-sym class)
                                                                :arity class-arity)))
                                       (setf env (unset-function env (ty-class-codegen-sym class)))))

                                 (dolist (method methods)
                                   (setf env (set-value-type env (car method) (cdr method)))

                                   (setf env (set-name env (car method)
                                                       (make-name-entry
                                                        :name (car method)
                                                        :type :method
                                                        :docstring nil
                                                        :location (or *compile-file-pathname* *load-truename*))))

                                   (if (function-type-p (qualified-ty-type (fresh-inst (cdr method))))
                                       (let ((arity (+ (function-type-arity
                                                        (qualified-ty-type (fresh-inst (cdr method))))
                                                       (length (qualified-ty-predicates (fresh-inst (cdr method)))))))
                                         (setf env (set-function env (car method)
                                                                 (make-function-env-entry
                                                                  :name (car method)
                                                                  :arity arity))))
                                       (setf env (unset-function env (car method)))))

                                 class)))))

        (values
         classes
         env)))))

(defun parse-class-definition (class-name unparsed-predicate unparsed-context unparsed-methods docstring env)
  (declare (type symbol class-name)
           (type list unparsed-predicate unparsed-context unparsed-methods)
           (type (or null string) docstring)
           (type environment env)
           (values ty-class scheme-binding-list))

  (let* ((class-tyvar-names (collect-type-vars unparsed-predicate))

         (class-tyvars
           (loop :for tyvar :in class-tyvar-names
                 :collect (list tyvar (make-variable (make-kvariable)))))

         (ksubs nil))

    ;; Check for invalid elements in the context
    (loop :for elem :in (rest unparsed-predicate)
          :do (unless (and (symbolp elem)
                           (equalp (symbol-package elem) keyword-package))
                (error-parsing unparsed-predicate "invalid type class predicate")))

    ;; Check for type variables that appear in context but not in the predicate
    (loop :for unparsed-ctx :in unparsed-context
          :for ctx-tyvar-names := (collect-type-vars unparsed-ctx)
          :do (loop :for ctx-tyvar :in ctx-tyvar-names
                    :do (unless (find ctx-tyvar class-tyvar-names :test #'equalp)
                          (error-parsing unparsed-ctx "type variable ~S appears in superclass ~S but not in class" ctx-tyvar unparsed-ctx))))

    (let*  ((superclasses
              (loop :for unparsed-ctx :in unparsed-context
                    :collect (multiple-value-bind (predicate new-ksubs)
                                 (parse-type-predicate env unparsed-ctx class-tyvars ksubs)
                               (setf ksubs new-ksubs)
                               predicate)))

            (unqualifed-methods
              (loop :for (name unparsed-type) :in unparsed-methods
                    :for method-tyvars := (collect-type-vars unparsed-type)
                    :for method-only-tyvars
                      := (remove-if
                          (lambda (var)
                            (member var class-tyvar-names))
                          method-tyvars)
                    :for method-tvars
                      := (append
                          class-tyvars
                          (mapcar
                           (lambda (tvar)
                             (list tvar (make-variable (make-kvariable))))
                           method-only-tyvars))
                    :collect (multiple-value-bind (type new-ksubs)
                                 (parse-qualified-type-expr env unparsed-type method-tvars ksubs)
                               (check-for-invalid-method-preds
                                name
                                unparsed-type
                                type
                                (mapcar (alexandria:compose #'tvar-tyvar #'second) class-tyvars))
                               (setf ksubs new-ksubs)
                               (setf ksubs (kunify (kind-of type) kstar ksubs))
                               (cons name (apply-ksubstitution ksubs type)))))

            (predicate (ty-predicate
                        class-name
                        ;; Collect type variables in the order they were defined on the unparsed predicate
                        (loop :for var :in (rest unparsed-predicate)
                              :for tyvar := (second (find var class-tyvars :key #'car :test #'equalp))
                              :collect (apply-ksubstitution ksubs tyvar)))))

      (setf ksubs (kind-monomorphise-subs (kind-variables predicate) ksubs))
      (setf predicate (apply-ksubstitution ksubs predicate))

      (let* ((superclasses (apply-ksubstitution ksubs superclasses))

             (unqualifed-methods
               (loop :for (name . type) :in unqualifed-methods
                     :for ksubs := (kind-monomorphise-subs (kind-variables type) ksubs)
                     :collect (cons name (apply-ksubstitution ksubs type))))

             (superclass-dict
               (loop :for super :in superclasses
                                 :for i :from 0
                                 :collect (cons super
                                                (alexandria:format-symbol
                                                 (symbol-package class-name)
                                                 (format nil "SUPER-~D" i)))))

             (codegen-sym
               (alexandria:format-symbol (symbol-package class-name) "CLASS/~A" class-name)))

        (values
         (ty-class
          :name class-name
          :predicate predicate
          :superclasses superclasses
          :unqualified-methods (loop :for (name . type) :in  unqualifed-methods
                                     :collect (cons name (quantify nil (apply-ksubstitution ksubs type))))
          :codegen-sym codegen-sym
          :superclass-dict superclass-dict
          :superclass-map (let ((table (make-hash-table)))
                            (loop :for (pred . super-name) :in superclass-dict
                                  :for prefixed-name
                                    := (alexandria:format-symbol
                                        (symbol-package class-name)
                                        "~A-~A"
                                        codegen-sym
                                        super-name)
                                  :do (setf (gethash prefixed-name table) super-name))
                            table)
          :docstring docstring
          :location (or *compile-file-pathname* *load-truename*))
         (loop :for (name . qual-ty) :in unqualifed-methods
               :for type := (qualified-ty-type qual-ty)
               :for preds := (qualified-ty-predicates qual-ty)
               :for new-qual-ty := (apply-ksubstitution ksubs (qualify (cons predicate preds) type))
               :collect (cons name (quantify (type-variables new-qual-ty) new-qual-ty))))))))

(defun check-for-invalid-method-preds (name unparsed-type type class-tyvars)
  (declare (type symbol name)
           (type qualified-ty type)
           (type tyvar-list class-tyvars))

  (loop :for pred :in (qualified-ty-predicates type)
        :do (when (subsetp (mapcar #'tyvar-id (type-variables pred))
                           (mapcar #'tyvar-id class-tyvars)
                           :test #'equalp)
              (error-parsing (list name unparsed-type) "predicate in method decleration only constrains class variables"))))
