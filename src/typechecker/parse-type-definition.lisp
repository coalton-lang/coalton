(defpackage #:coalton-impl/typechecker/parse-type-definition
  (:use
   #:cl
   #:coalton-impl/ast
   #:coalton-impl/typechecker/kinds
   #:coalton-impl/typechecker/type-errors
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/substitutions
   #:coalton-impl/typechecker/predicate
   #:coalton-impl/typechecker/scheme
   #:coalton-impl/typechecker/environment
   #:coalton-impl/typechecker/parse-type)
  (:import-from
   #:coalton-impl/typechecker/substitutions
   #:apply-substitution)
  (:import-from
   #:coalton-impl/typechecker/lisp-type
   #:lisp-type)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:error #:coalton-impl/error))
  (:export
   #:type-definition                    ; STRUCT
   #:make-type-definition               ; CONSTRUCTOR
   #:type-definition-name               ; ACCESSOR
   #:type-definition-type               ; ACCESSOR
   #:type-definition-runtime-type       ; ACCESSOR
   #:type-definition-explicit-repr      ; ACCESSOR
   #:type-definition-enum-repr          ; ACCESSOR
   #:type-definition-newtype            ; ACCESSOR
   #:type-definition-constructors       ; ACCESSOR
   #:type-definition-constructor-types  ; ACCESSOR
   #:type-definition-docstring          ; ACCESSOR
   #:type-definition-list               ; TYPE
   #:parse-type-definitions             ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/parse-type-definition)

;;;
;;; Parsing type defintions
;;;

(alexandria:define-constant +keyword-package+ (find-package "KEYWORD") :test #'equalp)

(defstruct type-definition
  (name              (util:required 'name)              :type symbol                 :read-only t)
  (type              (util:required 'type)              :type ty                     :read-only t)
  (runtime-type      (util:required 'runtime-type)      :type t                      :read-only t)

  ;; See the fields with the same name on type-entry
  (explicit-repr     (util:required 'explicit-repr)     :type explicit-repr          :read-only t)
  (enum-repr         (util:required 'enum-repr)         :type boolean                :read-only t)
  (newtype           (util:required 'newtype)           :type boolean                :read-only t)

  (constructors      (util:required 'constructors)      :type constructor-entry-list :read-only t)
  (constructor-types (util:required 'constructor-types) :type scheme-list            :read-only t)

  (docstring         (util:required 'docstring)         :type (or null string)       :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type type-definition))

(defun type-definition-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'type-definition-p x)))

(deftype type-definition-list ()
  '(satisfies type-definition-list-p))

(defstruct partial-define-type
  (name         (util:required 'name)         :type symbol                :read-only t)
  (tyvar-names  (util:required 'tyvar-names)  :type util:symbol-list      :read-only t)
  (constructors (util:required 'constructors) :type list                  :read-only t)
  (docstring    (util:required 'docstring)    :type (or null string)      :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type partial-define-type))

(defun partial-define-type-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'partial-define-type-p x)))

(deftype partial-define-type-list ()
  '(satisfies partial-define-type-list-p))

(defun parse-type-definition (partial-type self-type type-vars ksubs env)
  (declare (type partial-define-type partial-type)
           (type ty self-type)
           (type list type-vars)
           (type ksubstitution-list ksubs)
           (type environment env)
           (values list list ksubstitution-list))

  (let* ((tyvar-names (partial-define-type-tyvar-names partial-type))

         (unparsed-ctors (partial-define-type-constructors partial-type))

         (local-type-vars
           (loop :for tyvar-name :in tyvar-names
                 :collect (list tyvar-name (make-variable (make-kvariable)))))

         (tyvars
           (append
            type-vars
            local-type-vars))

         (constructors
           (loop :for ctor :in unparsed-ctors
                 :for name := (first ctor)
                 :for fields := (rest ctor)
                 :collect (cons
                           name
                           (loop :for field :in fields
                                 :collect (multiple-value-bind (type new-ksubs)
                                              (parse-type-expr env field tyvars ksubs)
                                            (setf ksubs new-ksubs)
                                            (setf ksubs (kunify (kind-of type) +kstar+ ksubs))
                                            type))))))

    ;; Unify the kind of the type with the kind:
    ;; kind(tvar1) -> kind(tvar2) ... -> *
    (setf ksubs (kunify
                 (kind-of self-type)
                 (make-kind-function*
                  (loop :for (name type) :in local-type-vars
                        :collect (kind-of type))
                  +kstar+)
                 ksubs))

    (values
     constructors
     local-type-vars
     ksubs)))

(defun parse-type-impls (partial-types env)
  "Parse the PARTIAL-TYPES in a single scc"
  (declare (type partial-define-type-list partial-types)
           (type environment env))

  (let* ((type-names (mapcar #'partial-define-type-name partial-types))

         (type-vars
           (loop :for type-name :in type-names
                 :collect (list type-name
                                (make-tycon
                                 :name type-name
                                 :kind (make-kvariable)))))

         (ksubs nil)

         (type-constructors
           (loop :for partial-type :in partial-types
                 :for self-type := (second (find (partial-define-type-name partial-type) type-vars :key #'first))
                 :collect (multiple-value-bind (constructors local-tyvars new-ksubs)
                              (parse-type-definition partial-type self-type type-vars ksubs env)
                            (setf ksubs new-ksubs)
                            (cons constructors local-tyvars)))))

    (values
     (loop :for partial-type :in partial-types
           :for (name type) :in type-vars
           :for (ctors . local-tyvars) :in type-constructors

           :for type_ := (apply-ksubstitution ksubs type)
           :for ksubs_ := (kind-monomorphize-subs (kind-variables type_) ksubs)
           :for type__ := (apply-ksubstitution ksubs_ type)

           :for tyvar-types
             := (loop :for (name tyvar) :in local-tyvars
                      :collect (apply-ksubstitution ksubs_ tyvar))

           :for applied-type
             := (apply-type-argument-list
                 type__
                 tyvar-types)

           ;; Declare the current type in the env. This decleration is incomplete, and is discarded away after parsing
           :do (setf
                env
                (set-type
                 env
                 name 
                 (make-type-entry
                  :name name
                  :runtime-type name
                  :type type__
                  :constructors nil
                  :explicit-repr nil
                  :enum-repr nil
                  :newtype nil
                  :docstring nil
                  :location nil)))

           :collect (list
                     name
                     type__
                     (loop :for (ctor-name . args) :in ctors
                           :for arity := (length args)
                           :for classname := (alexandria:format-symbol
                                              (symbol-package name)
                                              "~A/~A" name ctor-name)
                           :for args_ := (apply-ksubstitution ksubs_ args)
                           :for type := (make-function-type* args_ applied-type)
                           :for scheme := (quantify-using-tvar-order tyvar-types (qualify nil type))

                           :for entry := (make-constructor-entry
                                          :name ctor-name
                                          :arity arity
                                          :constructs name
                                          :classname classname
                                          :compressed-repr nil)

                           :do (check-constructor name ctor-name env)

                           :do (setf env (set-constructor env ctor-name entry))

                           :collect (cons scheme entry))
                     (partial-define-type-docstring partial-type)))
     env)))

(defun check-constructor (ty-name ctor-name env)
  "Verify that a given constructor isn't used by another type"
  (declare (type symbol ty-name)
           (type symbol ctor-name)
           (type environment env))
  
   (error:with-context ("define-type of ~S" ty-name)
    (let ((ctor-entry (lookup-constructor env ctor-name :no-error t)))
      (when (and ctor-entry (not (eq ty-name (constructor-entry-constructs ctor-entry))))
        (error 'duplicate-ctor
               :ctor-name ctor-name
               :ty-name (constructor-entry-constructs ctor-entry))))))

(defun parse-type-definitions (forms repr-table env)
  "Parse the type defintion FORM in the ENVironment

Returns TYPE-DEFINITIONS"
  (declare (type list forms)
           (type environment env)
           (values type-definition-list environment))

  ;; Pull out and verify DEFINE-TYPE and type
  (let ((type-definitions nil) ; list (name tvars constructors docstring)
        (type-dependencies nil)         ; list (name dependencies*)
        )
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

          ;; Push this tycon onto the list
          (let (;; If the first ctor is a string then it is the docstring and we should skip it.
                (constructors
                  (mapcar #'alexandria:ensure-list
                          (if (stringp (car ctors))
                              (cdr ctors)
                              ctors)))

                ;; Pull out the docstring if it exists
                (docstring
                  (when (stringp (car ctors))
                    (car ctors))))

            (error:with-context ("definition of type ~A" tycon-name)
              ;; Check for invalid type variables
              (unless (every (lambda (var)
                               (equalp (symbol-package var)
                                       +keyword-package+))
                             tyvar-names)
                (error-parsing form "type variables must all be in the KEYWORD package."))

              ;; Check for duplicate constructors
              (labels ((check-for-duplicate-constructors (ctors)
                         (if (find (car (car ctors)) (rest ctors) :key #'car :test #'equalp)
                             (error-parsing form "duplicate constructor ~A" (car (car ctors)))
                             (when (rest ctors)
                               (check-for-duplicate-constructors (rest ctors))))))
                (check-for-duplicate-constructors constructors))

              ;; Check for duplicate type variables
              (labels ((check-for-duplicate-type-variables (tyvar-names)
                         (if (find (car tyvar-names) (rest tyvar-names) :test #'equalp)
                             (error-parsing form "duplicate type variable ~S" (car tyvar-names))
                             (when (rest tyvar-names) 
                               (check-for-duplicate-type-variables (rest tyvar-names))))))
                (check-for-duplicate-type-variables tyvar-names))

              ;; Check for type variables appearing in constructors but not in the type
              (loop :for (ctor-name . fields) :in constructors
                    :do (loop :for field :in fields
                              :for field-tyvars := (collect-type-vars field)
                              :do (loop :for field-tyvar :in field-tyvars
                                        :do (unless (find field-tyvar tyvar-names :test #'equalp)
                                              (error-parsing
                                               form
                                               "type variable ~S appears in constructor ~A but not in type"
                                               field-tyvar
                                               ctor-name))))))

            ;; Push the partialy parsed define-type form
            (push
             (make-partial-define-type
              :name tycon-name
              :tyvar-names tyvar-names
              :constructors constructors
              :docstring docstring)
             type-definitions)

            ;; Push the types dependencies for computing SCCs
            (push
             (cons
              tycon-name
              (remove-duplicates
               (loop :for (name . args) :in constructors
                     :append (collect-types args))
               :test #'equalp))
             type-dependencies)))))

    (let* ((translation-unit-types (mapcar #'car type-dependencies))

           ;; Remove types from type-dependencies that are not
           ;; currently being defined. This is util:required for tarjan-scc.
           (type-dependencies
             (loop :for (name . deps) :in type-dependencies
                   :collect (cons name (intersection deps translation-unit-types))))

           (parsed-tcons
             ;; Temporary env to allow referencing partially defined types in parse-type
             (let ((env env))
               (loop :for scc :in (reverse (algo:tarjan-scc type-dependencies))
                     :for partial-types
                       := (loop :for type-name :in scc
                                :collect (find type-name type-definitions :test #'equalp :key #'partial-define-type-name))
                     :append (multiple-value-bind (data new-env)
                                 (parse-type-impls partial-types env)
                               (setf env new-env)
                               data)))))

      (values
       (loop :for (tycon-name tcon ctor-data docstring) :in parsed-tcons
             :collect
             (error:with-context ("definition of type ~A" tycon-name)
               
               ;; Parse out the ctors
               (let* ((parsed-ctors (mapcar #'cdr ctor-data))

                      (ctor-types (mapcar #'car ctor-data))

                      ;; If every constructor entry has an arity of 0
                      ;; then this type can be compiled as an enum
                      (enum-type (every (lambda (ctor)
                                          (= 0 (constructor-entry-arity ctor)))
                                        parsed-ctors))

                      ;; If there is a single constructor with a single
                      ;; field then this type can be compiled as a
                      ;; newtype
                      (newtype (and (= 1 (length parsed-ctors))
                                    (= 1 (constructor-entry-arity (first parsed-ctors)))))

                      (repr (car (gethash tycon-name repr-table)))
                      (repr-arg (cdr (gethash tycon-name repr-table)))

                      (type-definition
                        (cond
                          ;; If the type is repr lisp then do *not* attempt to
                          ;; generate an optimized implementation
                          ((eql repr :lisp)
                           (make-type-definition
                            :name tycon-name
                            :type tcon
                            :runtime-type tycon-name
                            :explicit-repr :lisp
                            :enum-repr nil
                            :newtype nil
                            :constructors parsed-ctors
                            :constructor-types ctor-types
                            :docstring docstring))

                          ((eql repr :native)
                           (progn
                             (unless repr-arg
                               (error "Type ~A cannot have native repr of NIL" tycon-name)))
                           (make-type-definition
                            :name tycon-name
                            :type tcon
                            :runtime-type repr-arg
                            :explicit-repr (list repr repr-arg)
                            :enum-repr nil
                            :newtype nil
                            :constructors parsed-ctors
                            :constructor-types ctor-types
                            :docstring docstring))

                          ((and newtype (eql repr :transparent))
                           (let (;; The runtime type of a newtype is the runtime type of it's only constructor's only argument
                                 (runtime-type (function-type-from
                                                (qualified-ty-type
                                                 (fresh-inst (first ctor-types))))))
                             (make-type-definition
                              :name tycon-name
                              :type tcon
                              :runtime-type (lisp-type runtime-type env)
                              :explicit-repr repr
                              :enum-repr nil
                              :newtype t
                              :constructors parsed-ctors
                              :constructor-types ctor-types
                              :docstring docstring)))

                          ((and (eql repr :transparent) (not newtype))
                           (error "Type ~A cannot be repr transparent. To be repr transparent a type must have a single constructor with a single field." tycon-name))

                          ((or (and enum-type (eql repr :enum))
                               (and enum-type (coalton-impl/settings:coalton-release-p)))
                           (let ((parsed-ctors (mapcar #'rewrite-ctor parsed-ctors)))
                             (make-type-definition
                              :name tycon-name
                              :type tcon
                              :runtime-type `(member ,@(mapcar #'constructor-entry-compressed-repr parsed-ctors))
                              :explicit-repr repr
                              :enum-repr t
                              :newtype nil
                              :constructors parsed-ctors
                              :constructor-types ctor-types
                              :docstring docstring)))

                          ((and (eql repr :enum) (not enum-type))
                           (error "Type ~A cannot be repr enum. To be repr enum a type must only have constructors without fields." tycon-name))

                          (repr
                           (error "Type ~A supplied an unknown or incompatable repr ~A" tycon-name repr))

                          ((not repr)
                           (make-type-definition
                            :name tycon-name
                            :type tcon
                            :runtime-type tycon-name
                            :explicit-repr nil
                            :enum-repr nil
                            :newtype nil
                            :constructors parsed-ctors
                            :constructor-types ctor-types
                            :docstring docstring)))))

                 ;; Define the parsed type in the environment
                 (setf env
                       (set-type
                        env
                        tycon-name
                        (make-type-entry
                         :name tycon-name
                         :runtime-type (type-definition-runtime-type type-definition)
                         :type (type-definition-type type-definition)
                         :constructors (mapcar #'constructor-entry-name (type-definition-constructors type-definition))
                         :explicit-repr (type-definition-explicit-repr type-definition)
                         :enum-repr (type-definition-enum-repr type-definition)
                         :newtype (type-definition-newtype type-definition)
                         :docstring (type-definition-docstring type-definition)
                         :location (or *compile-file-pathname* *load-truename*))))

                 ;; Define the types constructors in the environment
                 (loop :for ctor :in (type-definition-constructors type-definition)
                       :for ctor-type :in (type-definition-constructor-types type-definition)
                       :for ctor-name := (constructor-entry-name ctor) :do

                         (progn
                           ;; Add the constructors to the constructor environment
                           (setf env
                                 (set-constructor
                                  env
                                  ctor-name
                                  ctor))

                           ;; Add the constructor as a value to the value environment
                           (setf env
                                 (set-value-type
                                  env
                                  ctor-name
                                  ctor-type))

                           ;; Register the constructor in the name environment
                           (setf env
                                 (set-name
                                  env
                                  ctor-name
                                  (make-name-entry
                                   :name ctor-name
                                   :type :constructor
                                   :docstring nil
                                   :location (or *compile-file-pathname*
                                                 *load-truename*))))

                           ;; If the constructor takes paramaters then
                           ;; add it to the function environment
                           (if (not (= (constructor-entry-arity ctor) 0))
                               (setf env
                                     (set-function
                                      env
                                      ctor-name
                                      (make-function-env-entry
                                       :name ctor-name
                                       :arity (constructor-entry-arity ctor))))

                               ;; If the constructor does not take
                               ;; parameters then remove it from the
                               ;; function environment
                               (setf env
                                     (unset-function
                                      env
                                      ctor-name)))))

                 type-definition)))
       env))))

(defun rewrite-ctor (ctor)
  (declare (type constructor-entry ctor)
           (values constructor-entry))
  (assert (= 0 (constructor-entry-arity ctor)))
  (make-constructor-entry
   :name (constructor-entry-name ctor)
   :arity (constructor-entry-arity ctor)
   :constructs (constructor-entry-constructs ctor)
   :classname (constructor-entry-classname ctor)
   :compressed-repr (constructor-entry-classname ctor)))
