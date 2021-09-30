(in-package #:coalton-impl/typechecker)

;;;
;;; Parsing type defintions
;;;


(serapeum:defstruct-read-only type-definition
  (name :type symbol)
  (type :type ty)
  (runtime-type :type t)

  ;; See the fields with the same name on type-entry
  (compressed-type :type boolean)
  (newtype :type boolean)

  (constructors :type constructor-entry-list))

#+sbcl
(declaim (sb-ext:freeze-type type-definition))

(defun type-definition-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'type-definition-p x)))

(deftype type-definition-list ()
  '(satisfies type-definition-list-p))


(defun parse-type-definitions (forms repr-table env)
  "Parse the type defintion FORM in the ENVironment

Returns (TYPE-DEFINITIONS DOCSTRINGS)"
  (declare (type list forms)
           (type environment env)
           (values type-definition-list list))

  ;; Pull out and verify DEFINE-TYPE and type
  (let ((parsed-tcons nil)
        (parsed-docstrings nil))
    ;; First, go through and parse out all tycons so we can build an
    ;; environment
    (dolist (form forms)
      (assert (and (eql 'coalton:define-type (first form))
                   (<= 2 (length form))
                   (or (listp (second form))
                       (symbolp (second form))))
              () "Malformed DEFINE-TYPE form ~A" form)
      (destructuring-bind (def-type type &rest ctors) form
        (declare (ignore def-type))
        ;; Pull bare symbols into a list for easier parsing
        (setf type (alexandria:ensure-list type))

        ;; Pull out the type name and type variables
        (destructuring-bind (tycon-name &rest tyvar-names) type
          (assert (and (symbolp tycon-name)
                       (every #'symbolp tyvar-names))
                  () "Malformed DEFINE-TYPE type ~A" type)
          (assert (every (lambda (var)
                           (equalp (symbol-package var)
                                   keyword-package))
                         tyvar-names)
                  () "Type variables must be in the KEYWORD package. In type ~A" form)

          ;; Push this tycon onto the list
          (let ((tycon-type (%make-tcon (%make-tycon :name tycon-name
                                                     :kind (tvar-count-to-kind (length tyvar-names)))))
                (type-vars (loop :for i :below (length tyvar-names)
                                 :collect (make-variable))))

            ;; If there is a docstring it should occur as the first member of ctors. Pull this out
            (when (stringp (car ctors))
              (push (list tycon-name (car ctors) :type) parsed-docstrings)
              (setf ctors (cdr ctors)))

            ;; Save this for later
            (push (list tycon-name tycon-type ctors type-vars tyvar-names) parsed-tcons)))))

    ;; Then, re-parse all of the type definitions and ctors using the environment
    (let* ((new-bindings
             (mapcar
              (lambda (parsed)
                (cons
                 (first parsed)
                 (type-entry
                  :name (first parsed)
                  :runtime-type (first parsed)
                  :type (second parsed)
                  :compressed-type nil
                  :newtype nil)))
              parsed-tcons))
           (new-env (push-type-environment env new-bindings))
           (parsed-defs (loop :for parsed :in parsed-tcons
                              :collect
                              (let* ((tycon-name (first parsed))
                                     (tcon (second parsed))
                                     (ctors (third parsed))
                                     (type-vars (fourth parsed))
                                     (tyvar-names (fifth parsed))
                                     (type-vars-alist nil)
                                     (applied-tycon (apply-type-argument-list tcon type-vars)))

                                (with-parsing-context ("definition of type ~A" tycon-name)
                                  ;; Populate the type variable table for use in parsing types
                                  (setf type-vars-alist
                                        (loop :for name :in tyvar-names
                                              :for tvar :in type-vars
                                              :collect (cons name
                                                             (cons tvar
                                                                   (kind-arity (kind-of tvar))))))
                                  ;; Parse out the ctors
                                  (let* ((parsed-ctors
                                           (loop :for ctor in ctors
                                                 :collect
                                                 (parse-type-ctor ctor applied-tycon type-vars-alist tycon-name new-env)))

                                         ;; If every constructor entry has an arity of 0 then this type can be compiled as an enum
                                         (enum-type (every (lambda (ctor)
                                                             (= 0 (constructor-entry-arity ctor)))
                                                           parsed-ctors))

                                         ;; If there is a single constructor with a single field then this type can be compiled as a newtype 
                                         (newtype (and (= 1 (length parsed-ctors))
                                                       (= 1 (constructor-entry-arity (first parsed-ctors)))))

                                         (repr (gethash tycon-name repr-table)))
                                    (cond
                                      ;; If the type is repr lisp then
                                      ;; do *not* attempt to generate
                                      ;; an optimized implementation
                                      ((eql repr :lisp)
                                       (make-type-definition
                                        :name tycon-name
                                        :type tcon
                                        :runtime-type tycon-name
                                        :compressed-type nil
                                        :newtype nil
                                        :constructors parsed-ctors))

                                      ((and enum-type
                                            (eql coalton-impl:*interaction-mode* ':release))
                                       (let ((parsed-ctors (mapcar #'rewrite-ctor parsed-ctors)))
                                         (make-type-definition
                                          :name tycon-name
                                          :type tcon
                                          :runtime-type `(member ,@(mapcar #'constructor-entry-compressed-repr parsed-ctors))
                                          :compressed-type t
                                          :newtype nil
                                          :constructors parsed-ctors)))

                                      ((and newtype
                                            (eql coalton-impl:*interaction-mode* ':release))
                                       (let (;; The runtime type of a newtype is the runtime type of it's only constructor's only argument
                                             (runtime-type (qualified-ty-type (fresh-inst (first (constructor-entry-arguments (first parsed-ctors)))))))
                                         (make-type-definition
                                          :name tycon-name
                                          :type tcon
                                          :runtime-type runtime-type
                                          :compressed-type nil
                                          :newtype t
                                          :constructors parsed-ctors)))

                                      (t
                                       (make-type-definition
                                        :name tycon-name
                                        :type tcon
                                        :runtime-type tycon-name
                                        :compressed-type nil
                                        :newtype nil
                                        :constructors parsed-ctors)))))))))
      (values parsed-defs parsed-docstrings))))

(defun rewrite-ctor (ctor)
  (assert (= 0 (constructor-entry-arity ctor)))
  (make-constructor-entry
   :name (constructor-entry-name ctor)
   :arity (constructor-entry-arity ctor)
   :constructs (constructor-entry-constructs ctor)
   :scheme (constructor-entry-scheme ctor)
   :arguments (constructor-entry-arguments ctor)
   :classname (constructor-entry-classname ctor)
   :compressed-repr (constructor-entry-classname ctor)))

(defun parse-type-ctor (form applied-tycon type-vars constructs env)
  (declare (type t form)
           (type ty applied-tycon)
           (type list type-vars)
           (type environment env)
           (type symbol constructs)
           (values constructor-entry))
  ;; Make sure we have a list we can destructure
  (setf form (alexandria:ensure-list form))
  (destructuring-bind (ctor-name &rest tyarg-names) form

    (with-parsing-context ("constructor definition of ~A" ctor-name)
      ;; Lookup all type arguments either within the given TYPE-VARS or
      ;; as a type in the environment
      (let* ((tyvars (mapcar (lambda (v) (tvar-tyvar (cadr v))) type-vars))
             (tyargs (mapcar (lambda (arg)
                               (qualified-ty-type (fresh-inst (parse-and-resolve-type env arg type-vars tyvars))))
                             tyarg-names)))

        (unless (subsetp (type-variables tyargs) tyvars :test #'equalp)
          (error "Constructor ~A cannot use type variables that are not in scope."
                 form))

        ;; Build up the constructor function
        (labels ((build-function (args)
                   (if (car args)
                       (make-function-type (car args)
                                           (build-function (cdr args)))
                       applied-tycon))
                 ;; This is a version of quantify allowing us to
                 ;; preserve the order of tyvars instead of building it
                 ;; from the type.
                 (quantify-using-tvar-order (tyvars type)
                   (let* ((vars (remove-if
                                 (lambda (x) (not (find x (type-variables type) :test #'equalp)))
                                 tyvars))
                          (kinds (mapcar #'kind-of vars))
                          (subst (loop :for var :in vars
                                       :for id :from 0
                                       :collect (%make-substitution var (%make-tgen id)))))
                     (%make-ty-scheme kinds (apply-substitution subst type)))))
          (make-constructor-entry
           :name ctor-name
           :arity (length tyarg-names)
           :arguments (mapcar (lambda (arg)
                                (quantify tyvars (qualify nil arg)))
                              tyargs)
           :constructs constructs
           :scheme (quantify-using-tvar-order tyvars
                                              (qualify nil (build-function tyargs)))
           :classname (alexandria:format-symbol
                       (symbol-package constructs)
                       "~A/~A" constructs ctor-name)
           :compressed-repr nil))))))

(defun tvar-count-to-kind (tvar-count)
  "Create a KIND from the number of type variables"
  (if (= 0 tvar-count)
      kStar
      (kFun kStar (tvar-count-to-kind (1- tvar-count)))))
