(defpackage #:coalton-impl/codegen/program
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/translate-expression
   #:translate-toplevel)
  (:import-from
   #:coalton-impl/codegen/translate-instance
   #:translate-instance)
  (:import-from
   #:coalton-impl/codegen/codegen-class
   #:codegen-class-definitions)
  (:import-from
   #:coalton-impl/codegen/codegen-expression
   #:abstraction-lambda-list
   #:annotate-function-body
   #:codegen-expression
   #:node-output-values-type)
  (:import-from
   #:coalton-impl/codegen/codegen-type-definition
   #:codegen-type-definition)
  (:import-from
   #:coalton-impl/codegen/optimizer
   #:optimize-bindings)
  (:import-from
   #:coalton-impl/codegen/inliner
   #:function-declared-inline-p)
  (:import-from
   #:coalton-impl/codegen/calling-convention
   #:best-function-entrypoint
   #:calling-convention-descriptor-emit-defun-fn
   #:calling-convention-descriptor-emit-pre-declarations-fn
   #:calling-convention-descriptor-exclude-from-block-compile-p
   #:emit-stale-entrypoint-stubs
   #:*current-block-compilation-names*
   #:emit-standard-defun
   #:find-calling-convention
   #:synthesize-wrapper-abstraction)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:settings #:coalton-impl/settings)
   (#:global-lexical #:coalton-impl/global-lexical)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:*codegen-hook*
   #:compile-translation-unit))

(in-package #:coalton-impl/codegen/program)

(defvar *codegen-hook* nil
  "A hook that provides access to intermediate program representations during code generation.

A function bound here will be called with a keyword category, and one or more additional arguments, depending on the value of that keyword. Keyword values:

  :AST name type value

    Toplevel definitions, after type checking and before compilation.")

(defun function-type-lambda-list (node env)
  (declare (type node-abstraction node)
           (type tc:environment env)
           (values list &optional))
  (append
   (loop :for nil :in (node-abstraction-vars node)
         :for ty :in (tc:function-type-arguments (node-type node))
         :collect (tc:lisp-type ty env))
   (when (node-abstraction-keyword-params node)
     (list* '&key
            (loop :for entry :in (tc:function-ty-keyword-input-types (node-type node))
                  :collect `(,(tc:keyword-ty-entry-keyword entry)
                             ,(tc:lisp-type (tc:keyword-ty-entry-type entry) env)))))))

;; The following functions control the output order of compiled
;; definitions and interleaved lisp expressions.
;;
;; Toplevel define and instance forms are compiled to 1 or more named,
;; lisp-source-valued output definitions: when these definitions are
;; generated, they are associated with the starting source offset of their
;; toplevel form:
;;
;;   toplevel definition:
;;     #<def .... (300 . 345)>
;;
;;   bindings (lisp definitions):
;;     (300 b1 .. bn)
;;
;; Then when compile-definitions emits the full set of output
;; definitions, any lisp source forms that occurred earlier in the
;; file are emitted first.

(defun bindings-offset (bindings offsets)
  "Given a list of binding names, and a name -> offset map, return the earliest binding start offset."
  (reduce #'min
          (mapcar (lambda (binding)
                    (gethash (car binding) offsets 0))
                  bindings)))

(defun merge-forms (forms-a forms-b &optional merged)
  "Stably merge two lists of forms.

1. The inputs and output are lists of 2-lists, structured as (OFFSET FORM).
2. The order of both lists is preserved.
3. Lists are merged by recursively selecting the head of the list with the lowest offset.

Example:

  (merge-forms '((2 \"b\") (8 \"d\") (1 \"a\"))
               '((5 \"x\") (7 \"x\")))

    => ((2 \"b\") (5 \"x\") (7 \"x\") (8 \"d\") (1 \"a\"))

(Note that the order of elements in the first list is preserved)"
  (cond ((endp forms-a)
         (return-from merge-forms
           (nreconc merged forms-b)))
        ((endp forms-b)
         (return-from merge-forms
           (nreconc merged forms-a)))
        ((< (caar forms-a)
            (caar forms-b))
         (push (pop forms-a) merged))
        (t
         (push (pop forms-b) merged)))
  (merge-forms forms-a forms-b merged))

(defun compile-definitions (sccs definitions lisp-forms offsets env &optional previous-function-entries)
  "Compile SCCs and generate a final output definition list, merging any present lisp sources."
  (let ((bindings (loop :for scc :in sccs
                        :for bindings := (remove-if-not (lambda (binding)
                                                          (find (car binding) scc))
                                                        definitions)
                        :collect (cons (bindings-offset bindings offsets)
                                       (compile-scc bindings env previous-function-entries))))
        (lisp-forms (mapcar (lambda (lisp-form)
                              (cons (car (source:location-span (source:location lisp-form)))
                                    `((locally (declare #+sbcl (optimize (sb-c::type-check 1)))
                                        ,@(parser:toplevel-lisp-form-body lisp-form)))))
                            lisp-forms)))
    (mapcan #'cdr (merge-forms bindings lisp-forms))))

(defun wrap-forms-with-low-type-check (forms)
  (when forms
    `((locally (declare #+sbcl (optimize (sb-c::type-check 0)))
        ,@forms))))

(defun definition-bindings (definitions env offsets)
  "Translate the DEFINITIONS in this TU into bindings, updating an OFFSETS hashtable to record the source offset of each binding's source definition."
  (loop :for define :in definitions
        :for offset := (car (source:location-span (source:location define)))
        :for name := (tc:node-variable-name (tc:toplevel-define-name define))
        :for compiled-node := (translate-toplevel define env name)

        :when *codegen-hook*
          :do (funcall *codegen-hook* ':AST
                       name
                       (tc:lookup-value-type env name)
                       (tc:binding-value define))

        :do (setf (gethash name offsets) offset)
        :collect (cons name compiled-node)))

(defun instance-bindings (instances env offsets)
  "Translate the INSTANCES defined by this TU into bindings, updating an OFFSETS hashtable to record the source offset of each binding's source instance."
  (loop :for instance :in instances
        :for offset := (car (source:location-span (source:location instance)))
        :for instance-bindings := (translate-instance instance env)

        :do (dolist (binding instance-bindings)
              (setf (gethash (car binding) offsets) offset))
        :append instance-bindings))

(defun snapshot-previous-functions (env definitions)
  "Record existing FUNCTION-ENV-ENTRY structures for DEFINITIONS before
redefinition cleans ENV."
  (declare (type tc:environment env)
           (type list definitions)
           (values hash-table &optional))
  (let ((table (make-hash-table :test #'eq)))
    (loop :for (name . _) :in definitions
          :for entry := (tc:lookup-function env name :no-error t)
          :when entry
            :do (setf (gethash name table) entry))
    table))

(defun clean-environment-for-redefinition (env definitions)
  "If we're redefinining functions that can be inlinable, we should remove it from ENV to avoid the code from expanded using old definition."
  (loop :for (name . _) :in definitions
        :for fun := (tc:lookup-function env name :no-error t)
        :do (setf env (tc:unset-function env name))
        :finally (return env)))

(defun collect-block-compilation-names (definitions env)
  "Return all canonical and block-compilable worker function names defined in DEFINITIONS."
  (declare (type list definitions)
           (type tc:environment env)
           (values list &optional))
  (let ((names nil))
    (loop :for (name . _) :in definitions
          :for entry := (tc:lookup-function env name :no-error t)
          :do (push name names)
              (when entry
                (dolist (spec (tc:function-env-entry-entrypoints entry))
                  (let ((desc (find-calling-convention (tc:entrypoint-spec-convention spec) :no-error t)))
                    (unless (and desc
                                 (calling-convention-descriptor-exclude-from-block-compile-p desc))
                      (pushnew (tc:entrypoint-spec-symbol spec) names :test #'eq))))))
    (nreverse names)))

(defun compile-translation-unit (translation-unit monomorphize-table inline-p-table env
                                 &optional calling-convention-table)
  (declare (type tc:translation-unit translation-unit)
           (type hash-table monomorphize-table)
           (type hash-table inline-p-table)
           (type tc:environment env)
           (type (or null hash-table) calling-convention-table))

  (let* ((offsets (make-hash-table))
         (definitions
           (append
            (definition-bindings (tc:translation-unit-definitions translation-unit) env offsets)
            (instance-bindings (tc:translation-unit-instances translation-unit) env offsets)))
         (previous-function-entries (snapshot-previous-functions env definitions))
         (env (clean-environment-for-redefinition env definitions)))

    (multiple-value-bind (definitions env)
        (optimize-bindings definitions
                           monomorphize-table
                           inline-p-table
                           *package*
                           env
                           calling-convention-table)

      (let* ((definition-names (collect-block-compilation-names definitions env))
             (*current-block-compilation-names*
               #+sbcl (and (eq sb-ext:*block-compile-default* ':specified)
                           definition-names)
               #-sbcl nil)
             (sccs (node-binding-sccs definitions))
             (lisp-forms (tc:translation-unit-lisp-forms translation-unit)))

        (values
         `(progn
            ;; Muffle redefinition warnings in SBCL. A corresponding
            ;; SB-EXT:UNMUFFLE-CONDITIONS appears at the bottom.
            #+sbcl
            ,@(when settings:*emit-type-annotations*
                (list '(declaim (sb-ext:muffle-conditions sb-kernel:redefinition-warning))))

            ,@(when (tc:translation-unit-types translation-unit)
                (list
                 `(eval-when (:compile-toplevel :load-toplevel :execute)
                    ,@(loop :for type :in (tc:translation-unit-types translation-unit)
                            :append (codegen-type-definition type env)))))

            ,@(when (tc:translation-unit-classes translation-unit)
                (list
                 `(eval-when (:compile-toplevel :load-toplevel :execute)
                    ,@(codegen-class-definitions
                       (tc:translation-unit-classes translation-unit)
                       env))))

            #+sbcl
            ,@(when (eq sb-ext:*block-compile-default* :specified)
                (list
                 `(declaim (sb-ext:start-block ,@definition-names))))

            ,@(compile-definitions sccs definitions lisp-forms offsets env previous-function-entries)

            #+sbcl
            ,@(when (eq sb-ext:*block-compile-default* :specified)
                (list
                 `(declaim (sb-ext:end-block))))

            #+sbcl
            ,@(when settings:*emit-type-annotations*
                (list '(declaim (sb-ext:unmuffle-conditions sb-kernel:redefinition-warning))))

            (values))
         env)))))

(defun compile-function (name node env &key local-notinline-names)
  (declare (type symbol name)
           (type node-abstraction node)
           (type tc:environment env)
           (type list local-notinline-names))
  `(defun ,name ,(abstraction-lambda-list node)
     (declare (ignorable ,@(loop :for param :in (node-abstraction-keyword-params node)
                                 :append (list (keyword-param-var param)
                                               (keyword-param-supplied-p-var param)))
                         ,@(node-abstraction-vars node))
              ,@(when local-notinline-names
                  `((notinline ,@local-notinline-names))))
     ,(annotate-function-body
       node
       (codegen-expression (node-abstraction-subexpr node) env)
       env)))

(defun make-ftype-declaim (name node env)
  (declare (type symbol name)
           (type node-abstraction node)
           (type tc:environment env))
  `(declaim
    (ftype
     (function
      ,(function-type-lambda-list node env)
      ,(node-output-values-type node env))
     ,name)))

(defun compile-scc (bindings env &optional previous-function-entries)
  "Compile SCC definitions in a translation unit."
  (declare (type binding-list bindings)
           (type tc:environment env)
           (type (or null hash-table) previous-function-entries))
  (let* ((inline-function-names
           (loop :for (name . node) :in bindings
                 :when (and (node-abstraction-p node)
                            (function-declared-inline-p name env))
                   :collect name))
         (inline-worker-names
           (loop :for name :in inline-function-names
                 :for entry := (tc:lookup-function env name :no-error t)
                 :for primary := (and entry (best-function-entrypoint entry))
                 :when primary
                   :collect (tc:entrypoint-spec-symbol primary)))
         (stale-stub-forms
           (loop :for (name . _) :in bindings
                 :for old-entry := (and previous-function-entries
                                        (gethash name previous-function-entries))
                 :for new-entry := (tc:lookup-function env name :no-error t)
                 :append (emit-stale-entrypoint-stubs name old-entry new-entry env)))
         (special-declarations
           (loop :for binding :in bindings
                 :for name := (car binding)
                 :when (util:dynamic-variable-name-p name)
                   :collect `(declaim (special ,name))))
         (global-definitions
           (loop :for (name . node) :in bindings
                 :if (util:dynamic-variable-name-p name)
                   :collect `(defvar ,name)
                 :else
                   :collect `(global-lexical:define-global-lexical ,name ,(tc:lisp-type (node-type node) env))))
         ;; Function declarations must appear before function definitions to
         ;; optimize mutual tail-calls and register worker entrypoints.
         (function-type-declarations
           (when settings:*emit-type-annotations*
             (loop :for (name . node) :in bindings
                   :when (node-abstraction-p node)
                     :append
                     (let* ((entry (tc:lookup-function env name :no-error t))
                            (entrypoints (and entry (tc:function-env-entry-entrypoints entry))))
                       (append
                        (loop :for spec :in entrypoints
                              :for spec-sym := (tc:entrypoint-spec-symbol spec)
                              :for spec-node := (or (tc:lookup-code env spec-sym :no-error t)
                                                    (make-node-abstraction
                                                     :type (tc:entrypoint-spec-function-type spec)
                                                     :vars (loop :repeat (tc:entrypoint-spec-arity spec)
                                                                 :collect (gensym))
                                                     :subexpr (node-abstraction-subexpr node)))
                              :collect (make-ftype-declaim spec-sym spec-node env))
                        (list (make-ftype-declaim name node env)))))))
         ;; Emit any convention-specific compile-time pre-declarations for workers in the SCC.
         (worker-pre-declarations
           (let ((compile-time-registrations nil))
             (loop :for (name . node) :in bindings
                   :when (node-abstraction-p node)
                     :do (let* ((entry (tc:lookup-function env name :no-error t))
                                (primary (and entry (best-function-entrypoint entry)))
                                (desc (and primary
                                           (find-calling-convention
                                            (tc:entrypoint-spec-convention primary)
                                            :no-error t))))
                           (when desc
                             (let* ((worker-sym (tc:entrypoint-spec-symbol primary))
                                    (worker-node (tc:lookup-code env worker-sym)))
                               (dolist (form (funcall (calling-convention-descriptor-emit-pre-declarations-fn desc)
                                                      worker-sym
                                                      worker-node
                                                      env))
                                 (push form compile-time-registrations))))))
             (when compile-time-registrations
               (list
                `(eval-when (:compile-toplevel)
                   ,@(nreverse compile-time-registrations))))))
         ;; Keep emitted definitions globally inline for Lisp callers, but
         ;; stop inline SCC members from recursively inlining each other while
         ;; their own bodies are compiled.
         (inline-declarations
           (loop :for (name . node) :in bindings
                 :when (and (node-abstraction-p node)
                            (function-declared-inline-p name env))
                   :collect `(declaim (inline ,name))))
         (definition-forms
           (append
            stale-stub-forms
            (loop :for (name . node) :in bindings
                  :if (node-abstraction-p node)
                    :append
                    (let* ((entry (tc:lookup-function env name :no-error t))
                           (primary (and entry (best-function-entrypoint entry))))
                      (if (null primary)
                          (list
                           (compile-function name
                                             node
                                             env
                                             :local-notinline-names inline-function-names)
                           `(setf ,name #',name))
                          (let* ((primary-sym (tc:entrypoint-spec-symbol primary))
                                 (primary-conv (tc:entrypoint-spec-convention primary))
                                 (primary-desc (find-calling-convention primary-conv))
                                 (primary-node (tc:lookup-code env primary-sym))
                                 (secondary-specs
                                   (remove primary
                                           (tc:function-env-entry-entrypoints entry)
                                           :test #'eq))
                                 (return-type (tc:function-return-type (node-type node)))
                                 (lisp-wrapper-node
                                   (synthesize-wrapper-abstraction
                                    (node-type node)
                                    (mapcar (lambda (ty) (tc:make-abi-leaf :type ty))
                                            (tc:function-type-arguments (node-type node)))
                                    (if (typep return-type 'tc:result-ty)
                                        (tc:make-abi-multi-value
                                         :type return-type
                                         :components (mapcar (lambda (ct) (tc:make-abi-leaf :type ct))
                                                             (tc:result-ty-output-types return-type)))
                                        (tc:make-abi-leaf :type return-type))
                                    primary)))
                            (append
                             ;; 1. Primary worker entrypoint (holds the compiled body)
                             (list
                              (funcall (calling-convention-descriptor-emit-defun-fn primary-desc)
                                       primary-sym
                                       primary-node
                                       env
                                       :local-notinline-names inline-worker-names))
                             ;; 2. Any secondary worker entrypoints (e.g. :UNPACKED forwarding to :UNBOXED)
                             (loop :for sec :in secondary-specs
                                   :for sec-sym := (tc:entrypoint-spec-symbol sec)
                                   :for sec-wrapper
                                     := (synthesize-wrapper-abstraction
                                         (tc:entrypoint-spec-function-type sec)
                                         (tc:entrypoint-spec-input-layouts sec)
                                         (tc:entrypoint-spec-output-layout sec)
                                         primary)
                                   :collect (compile-function sec-sym sec-wrapper env))
                             ;; 3. Canonical :LISP entrypoint (forwards to primary worker)
                             (list
                              (compile-function name
                                                lisp-wrapper-node
                                                env
                                                :local-notinline-names inline-function-names)
                              `(setf ,name #',name)))))))

            (loop :for (name . node) :in bindings
                  :if (not (node-abstraction-p node))
                    :collect `(setf
                                ,name
                                ,(codegen-expression node env)))

            (loop :for (name . node) :in bindings
                  :for entry := (tc:lookup-name env name :no-error t)
                  :for docstring := (and entry (source:docstring entry))
                  :append (when docstring
                            (list `(setf (documentation ',name 'variable)
                                         ,docstring)))
                  :append (when (and docstring
                                     entry
                                     (node-abstraction-p node))
                            (list `(setf (documentation ',name 'function)
                                         ,docstring)))))))

    ;; Print types of definitions.
    (when settings:*compile-print-types*
      (dolist (binding bindings)
        (let* ((name (car binding))
               (type (tc:lookup-value-type env name :no-error t)))
          (unless (null type)
            (format t "~&;; ~a :: ~a~%"
                    name
                    (tc:type-to-string type env))))))

    (append
     special-declarations
     (wrap-forms-with-low-type-check
      global-definitions)
     function-type-declarations
     worker-pre-declarations
     inline-declarations
     (wrap-forms-with-low-type-check
      definition-forms))))
