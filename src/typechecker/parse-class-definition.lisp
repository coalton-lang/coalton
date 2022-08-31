(defpackage #:coalton-impl/typechecker/parse-class-definition
  (:use
   #:cl
   #:coalton-impl/ast
   #:coalton-impl/typechecker/kinds
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/predicate
   #:coalton-impl/typechecker/scheme
   #:coalton-impl/typechecker/type-errors
   #:coalton-impl/typechecker/environment
   #:coalton-impl/typechecker/parse-type)
  (:import-from
   #:coalton-impl/typechecker/fundeps
   #:fundep
   #:closure
   #:make-fundep
   #:fundep-list)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:error #:coalton-impl/error))
  (:export
   #:parse-class-definitions            ; FUNCTION
   #:split-class-signature              ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/parse-class-definition)

;;;
;;; Parsing class defintions
;;;

(alexandria:define-constant +keyword-package+ (find-package "KEYWORD") :test #'equalp)

(defun parse-method-predicates (name type)
  (declare (type symbol name))

  (error:with-context ("method ~A" name)
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

(defun split-class-signature (signature error-message &key parse-fundeps)
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

    ;; If parse-fundeps then the predicate is a class predicate. Every
    ;; member must be a symbol, and everything after is a functional
    ;; dependency.
    (if parse-fundeps
        (multiple-value-bind (l r)
            (util:take-until #'listp predicate)
          (values
           l
           context
           r))
        (values
         predicate
         context
         nil))))

(defun parse-fundep (unparsed valid-variables)
  (declare (type list unparsed)
           (type util:symbol-list valid-variables)
           (values fundep))
  (let ((result (split-sequence:split-sequence-if #'coalton-arrow-p unparsed)))
    (unless (= (length result) 2)
      (error-parsing unparsed "invalid functional dependency"))
    
    (let* ((from (first result))
           (to (second result)))

      (when (or (null from) (null to))
        (error-parsing unparsed "invalid functional dependency"))


      (error:with-context ("dependency ~S" unparsed)
        (loop :for var :in (union from to)

              :unless (and (symbolp var) (equalp (symbol-package var) +keyword-package+))
                :do (error-parsing var "invalid dependency variable")

              :unless (find var valid-variables)
                :do (error-parsing var "is not a valid class variable")))

      (make-fundep
       :from from
       :to to))))

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
        (error-parsing form "malformed DEFINE-CLASS"))

      (let ((name nil)
            (context nil)
            (predicate nil)
            (fundeps nil)
            (method-predicates nil)
            (methods nil)
            (docstring nil))
        (declare (type symbol name)
                 (type list context)
                 (type util:symbol-list predicate)
                 (type fundep-list fundeps)
                 (type list method-predicates)
                 (type list methods)
                 (type (or null string) docstring))

        (multiple-value-bind (predicate_ context_ fundeps_)
            (split-class-signature (second form) "malformed DEFINE-CLASS" :parse-fundeps t)
          (setf predicate predicate_)
          (setf context context_)
          (setf name (car predicate))

          (error:with-context ("DEFINE-CLASS for class ~A" name)
            (loop :for unparsed :in fundeps_
                  :for fundep := (parse-fundep unparsed (cdr predicate))
                  :do (push fundep fundeps))))

        (if (stringp (first (nthcdr 2 form)))
            (setf docstring (first (nthcdr 2 form))
                  methods (nthcdr 3 form))
            (setf methods (nthcdr 2 form)))

        (error:with-context ("DEFINE-CLASS for class ~A" name)
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
        (push (list name predicate context methods docstring fundeps) class-forms)))

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
    (let* ((sorted-classes (reverse (algo:tarjan-scc class-deps)))
           (sorted-forms
             (loop :for class-group :in sorted-classes
                   :collect (progn
                              (unless (= 1 (length class-group))
                                (error 'cyclic-class-definitions-error :classes class-group))
                              (find (first class-group) class-forms :key #'car)))))

      ;; Now we can go through to parse and then add classes to the environment
      (let ((classes
              (loop :for (name unparsed-predicate unparsed-context unparsed-methods docstring fundeps) :in sorted-forms
                    :collect (error:with-context ("DEFINE-CLASS for class ~A" name)
                               (multiple-value-bind (class methods)
                                   (parse-class-definition name unparsed-predicate unparsed-context unparsed-methods docstring fundeps env)

                                 (let ((prev-class (lookup-class env (ty-class-name class) :no-error t)))

                                   ;; If the class was previously defined, then the fundeps must not change
                                   (if prev-class
                                     (unless (and
                                              (equalp (ty-class-fundeps class) (ty-class-fundeps prev-class)))
                                       (error 'invalid-fundep-change
                                              :name (ty-class-name class)))

                                     ;; If this is the first definition of the class, and it has fundeps, then initialize the fundep environment
                                     (when (ty-class-fundeps class)
                                       (setf env (initialize-fundep-environment env name))))

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
                                         (setf env (unset-function env (car method))))))

                                 class)))))

        (values
         classes
         env)))))

(defun parse-class-definition (class-name unparsed-predicate unparsed-context unparsed-methods docstring fundeps env)
  (declare (type symbol class-name)
           (type list unparsed-predicate unparsed-context unparsed-methods)
           (type (or null string) docstring)
           (type fundep-list fundeps)
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
                           (equalp (symbol-package elem) +keyword-package+))
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
                    :collect (error:with-context ("method ~S" name)
                               (multiple-value-bind (type new-ksubs)
                                   (parse-qualified-type-expr env unparsed-type method-tvars ksubs)
                                 (check-for-invalid-method-preds
                                  name
                                  unparsed-type
                                  type
                                  (mapcar #'second class-tyvars))
                                 (check-for-ambigious-method
                                  name
                                  unparsed-type
                                  method-tyvars
                                  class-tyvar-names
                                  fundeps)
                                 (setf ksubs new-ksubs)
                                 (setf ksubs (kunify (kind-of type) +kstar+ ksubs))
                                 (cons name (apply-ksubstitution ksubs type))))))

            (predicate (make-ty-predicate
                        :class class-name
                        ;; Collect type variables in the order they were defined on the unparsed predicate
                        :types (loop :for var :in (rest unparsed-predicate)
                                     :for tyvar := (second (find var class-tyvars :key #'car :test #'equalp))
                                     :collect (apply-ksubstitution ksubs tyvar)))))

      (setf ksubs (kind-monomorphize-subs (kind-variables predicate) ksubs))
      (setf predicate (apply-ksubstitution ksubs predicate))

      (let* ((superclasses (apply-ksubstitution ksubs superclasses))

             (unqualifed-methods
               (loop :for (name . type) :in unqualifed-methods
                     :for ksubs := (kind-monomorphize-subs (kind-variables type) ksubs)
                     :collect (cons name (apply-ksubstitution ksubs type))))

             (superclass-dict
               (loop :for super :in superclasses
                     :for i :from 0
                     :collect (cons super
                                    (alexandria:format-symbol
                                     (symbol-package class-name)
                                     (format nil "SUPER-~D" i)))))

             (codegen-sym
               (alexandria:format-symbol (symbol-package class-name) "CLASS/~A" class-name))

             (class-variable-map (make-hash-table)))

        ;; Build the class variable map
        (loop :for var :in (cdr unparsed-predicate)
              :for i :from 0
              :do (setf (gethash var class-variable-map) i))

        (values
         (make-ty-class
          :name class-name
          :predicate predicate
          :superclasses superclasses
          :class-variables (cdr unparsed-predicate)
          :class-variable-map class-variable-map
          :fundeps fundeps
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


(defun check-for-ambigious-method (name unparsed-type method-tyvars class-tyvars fundeps)
  (declare (ignore name))
  (let ((method-closure (closure method-tyvars fundeps)))
    (unless (subsetp class-tyvars method-closure :test #'equalp)
      (error 'ambigious-constraint-variables
             :type unparsed-type
             :variables (set-difference class-tyvars method-closure :test #'equalp)))))
