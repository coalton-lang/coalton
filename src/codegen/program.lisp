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
   #:codegen-expression
   #:function-declarations)
  (:import-from
   #:coalton-impl/codegen/codegen-type-definition
   #:codegen-type-definition)
  (:import-from
   #:coalton-impl/codegen/optimizer
   #:optimize-bindings)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:settings #:coalton-impl/settings)
   (#:global-lexical #:coalton-impl/global-lexical)
   (#:rt #:coalton-impl/runtime)
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

(defun compile-definitions (sccs definitions lisp-forms offsets env)
  "Compile SCCs and generate a final output definition list, merging any present lisp sources."
  (let ((bindings (loop :for scc :in sccs
                        :for bindings := (remove-if-not (lambda (binding)
                                                          (find (car binding) scc))
                                                        definitions)
                        :collect (cons (bindings-offset bindings offsets)
                                       (compile-scc bindings env))))
        (lisp-forms (mapcar (lambda (lisp-form)
                              (cons (car (parser:toplevel-lisp-form-source lisp-form))
                                    (parser:toplevel-lisp-form-body lisp-form)))
                            lisp-forms)))
    (mapcan #'cdr (merge-forms bindings lisp-forms))))

(defun definition-bindings (definitions env offsets)
  "Translate the DEFINITIONS in this TU into bindings, updating an OFFSETS hashtable to record the source offset of each binding's source definition."
  (loop :for define :in definitions
        :for offset := (car (tc:toplevel-define-source define))
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
        :for offset := (car (tc:toplevel-define-instance-source instance))
        :for instance-bindings := (translate-instance instance env)

        :do (dolist (binding instance-bindings)
              (setf (gethash (car binding) offsets) offset))
        :append instance-bindings))

(defun compile-translation-unit (translation-unit monomorphize-table env)
  (declare (type tc:translation-unit translation-unit)
           (type hash-table monomorphize-table)
           (type tc:environment env))

  (let* ((offsets (make-hash-table))
         (definitions
           (append
            (definition-bindings (tc:translation-unit-definitions translation-unit) env offsets)
            (instance-bindings (tc:translation-unit-instances translation-unit) env offsets)))
         (definition-names (mapcar #'car definitions)))

    (multiple-value-bind (definitions env)
        (optimize-bindings definitions monomorphize-table *package* env)

      (let ((sccs (node-binding-sccs definitions))
            (lisp-forms (tc:translation-unit-lisp-forms translation-unit)))

        (values
         `(progn
            ;; Muffle redefinition warnings in SBCL. A corresponding
            ;; SB-EXT:UNMUFFLE-CONDITIONS appears at the bottom.
            #+sbcl ,@(when settings:*emit-type-annotations*
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

            ,@(compile-definitions sccs definitions lisp-forms offsets env)

            #+sbcl
            ,@(when (eq sb-ext:*block-compile-default* :specified)
                (list
                 `(declaim (sb-ext:end-block))))

            #+sbcl ,@(when settings:*emit-type-annotations*
                       (list '(declaim (sb-ext:unmuffle-conditions sb-kernel:redefinition-warning))))

            (values))
         env)))))

(defun compile-function (name node env)
  (declare (type symbol name)
           (type node-abstraction node)
           (type tc:environment env))
  `(defun ,name ,(node-abstraction-vars node)
     ,(function-declarations node env)
     ,(codegen-expression (node-abstraction-subexpr node) env)))

(defun compile-scc (bindings env)
  "Compile SCC definitions in a translation unit."
  (declare (type binding-list bindings)
           (type tc:environment env))
  (append
   ;; Predeclare symbol macros
   (loop :for (name . node) :in bindings
         :collect `(global-lexical:define-global-lexical ,name ,(tc:lisp-type (node-type node) env)))

   ;; Compile functions
   (loop :for (name . node) :in bindings
         :if (node-abstraction-p node)
           :append (list
                    (compile-function name node env)
                    `(setf
                      ,name
                      ,(rt:construct-function-entry
                        `#',name (length (node-abstraction-vars node))))))

  ;; Compile variables
  (loop :for (name . node) :in bindings
        :if (not (node-abstraction-p node))
          :collect `(setf
                      ,name
                      ,(codegen-expression node env)))

  ;; Print types of definitions
  (when settings:*compile-print-types*
    (dolist (binding bindings)
      (let* ((name (car binding))
             (type (tc:lookup-value-type env name :no-error t)))
        (unless (null type)
          (format t "~&;; ~a :: ~a~%" name type)))))

  ;; Docstrings
  (loop :for (name . node) :in bindings
        :for entry := (tc:lookup-name env name :no-error t)
        :for type := (tc:lookup-value-type env name :no-error t)
        :for docstring
          := (cond
               ((and entry (tc:name-entry-docstring entry) type)
                (format nil "~A :: ~A~%~A" name type (tc:name-entry-docstring entry)))

               ((and entry (tc:name-entry-docstring entry))
                (tc:name-entry-docstring entry))

                (type
                 (format nil "~A :: ~A" name type)))
        :append (when docstring
                  (list `(setf (documentation ',name 'variable)
                               ,docstring)))
        :append (when (and entry (node-abstraction-p node))
                  (list `(setf (documentation ',name 'function)
                               ,docstring))))))
