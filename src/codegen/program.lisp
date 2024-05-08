(defpackage #:coalton-impl/codegen/program
  (:use
   #:cl
   #:coalton-impl/codegen/ast
   #:coalton-impl/codegen/base)
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
   #:codegen-expression)
  (:import-from
   #:coalton-impl/codegen/codegen-type-definition
   #:codegen-type-definition)
  (:import-from
   #:coalton-impl/codegen/optimizer
   #:optimize-bindings)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:settings #:coalton-impl/settings)
   (#:global-lexical #:coalton-impl/global-lexical)
   (#:rt #:coalton-impl/runtime)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:compile-translation-unit))

(in-package #:coalton-impl/codegen/program)

(defun compile-translation-unit (backend translation-unit monomorphize-table env)
  (declare (type tc:translation-unit translation-unit)
           (type hash-table monomorphize-table)
           (type tc:environment env))

  (let* ((definitions
           (append
            (loop :for define :in (tc:translation-unit-definitions translation-unit)
                  :for name := (tc:node-variable-name (tc:toplevel-define-name define))

                  :for compiled-node := (translate-toplevel define env)

                  :do (emit-ast backend name
                                (tc:lookup-value-type env name)
                                (tc:binding-value define))

                  :collect (cons name compiled-node))

            ;; HACK: this load bearing reverse should be replaced with an actual solution
            (loop :for instance :in (reverse (tc:translation-unit-instances translation-unit))
                  :append (translate-instance instance env))))

         (definition-names
           (mapcar #'car definitions)))

    (multiple-value-bind (definitions env)
        (optimize-bindings definitions monomorphize-table *package* env)

      (let ((sccs (node-binding-sccs definitions)))

        ;; Muffle redefinition warnings in SBCL. A corresponding
        ;; SB-EXT:UNMUFFLE-CONDITIONS appears at the bottom.
        #+sbcl
        (when settings:*emit-type-annotations*
          (emit backend
                '(declaim (sb-ext:muffle-conditions sb-kernel:redefinition-warning))))

        (when (tc:translation-unit-types translation-unit)
          (emit backend
                `(eval-when (:compile-toplevel :load-toplevel :execute)
                   ,@(loop :for type :in (tc:translation-unit-types translation-unit)
                           :append (codegen-type-definition type env)))))

        (when (tc:translation-unit-classes translation-unit)
          (emit backend
                `(eval-when (:compile-toplevel :load-toplevel :execute)
                   ,@(codegen-class-definitions
                      (tc:translation-unit-classes translation-unit)
                      env))))

        #+sbcl
        (when (eq sb-ext:*block-compile-default* :specified)
          (emit backend `(declaim (sb-ext:start-block ,@definition-names))))

        (loop :for scc :in sccs
              :for bindings
                := (remove-if-not
                    (lambda (binding)
                      (find (car binding) scc))
                    definitions)
              :do (compile-scc backend bindings env))

        #+sbcl
        (when (eq sb-ext:*block-compile-default* :specified)
          (emit backend `(declaim (sb-ext:end-block))))

        #+sbcl
        (when settings:*emit-type-annotations*
          (emit backend
                '(declaim (sb-ext:unmuffle-conditions sb-kernel:redefinition-warning)))))
      env)))

(defun compile-function (name node env)
  (declare (type symbol name)
           (type node-abstraction node)
           (type tc:environment env))
  (let ((type-decs
          (when settings:*emit-type-annotations*
            (append
             (loop :for name :in (node-abstraction-vars node)
                   :for i :from 0
                   :for arg-ty := (nth i (tc:function-type-arguments (node-type node)))
                   :collect `(type ,(tc:lisp-type arg-ty env) ,name))
             (list `(values ,(tc:lisp-type (node-type (node-abstraction-subexpr node)) env)
                            &optional))))))
    `(defun ,name ,(node-abstraction-vars node)
       (declare (ignorable ,@(node-abstraction-vars node))
                ,@type-decs)
       ,(codegen-expression (node-abstraction-subexpr node) name env))))

(defun compile-scc (backend bindings env)
  (declare (type binding-list bindings)
           (type tc:environment env))

  ;; Predeclare symbol macros
  (loop :for (name . node) :in bindings
        :do (emit backend
                  `(global-lexical:define-global-lexical ,name ,(tc:lisp-type (node-type node) env))))

  ;; Compile functions
  (loop :for (name . node) :in bindings
        :when (node-abstraction-p node)
          :do (emit backend (compile-function name node env))
              (emit backend
                    `(setf ,name ,(rt:construct-function-entry
                                   `#',name (length (node-abstraction-vars node))))))

  ;; Compile variables
  (loop :for (name . node) :in bindings
        :unless (node-abstraction-p node)
          :do (emit backend `(setf ,name ,(codegen-expression node nil env))))

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
        :when docstring
          :do (emit backend `(setf (documentation ',name 'variable) ,docstring))
        :when (and entry (node-abstraction-p node))
          :do (emit backend `(setf (documentation ',name 'function) ,docstring))))
