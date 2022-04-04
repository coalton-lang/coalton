(defpackage #:coalton-impl/codegen/program
  (:use
   #:cl
   #:coalton-impl/util
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/algorithm
   #:immutable-map-data)
  (:import-from
   #:coalton-impl/codegen/lisp-type
   #:lisp-type)
  (:import-from
   #:coalton-impl/codegen/function-entry
   #:construct-function-entry
   #:apply-function-entry)
  (:import-from
   #:coalton-impl/codegen/compile-expression
   #:compile-toplevel)
  (:import-from
   #:coalton-impl/codegen/codegen-class
   #:codegen-class-definitions)
  (:import-from
   #:coalton-impl/codegen/compile-instance
   #:compile-instance)
  (:import-from
   #:coalton-impl/codegen/codegen-expression
   #:codegen-expression)
  (:import-from
   #:coalton-impl/codegen/codegen-type-definition
   #:codegen-type-definition)
  (:import-from
   #:coalton-impl/codegen/function-entry
   #:construct-function-entry
   #:apply-function-entry)
  (:import-from
   #:coalton-impl/codegen/typecheck-node
   #:typecheck-node)
  (:import-from
   #:coalton-impl/codegen/hoister
   #:make-hoister
   #:pop-final-hoist-point)
  (:import-from
   #:coalton-impl/codegen/transformations
   #:canonicalize
   #:pointfree
   #:direct-application
   #:inline-methods
   #:static-dict-lift)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:compile-translation-unit))

(in-package #:coalton-impl/codegen/program)


(defun compile-translation-unit (translation-unit env)
  (declare (type tc:translation-unit translation-unit)
           (type tc:environment env))

  (let* ((inline-funs nil)

         (add-inline
           (lambda (name)
             (declare (type symbol name))
             (push name inline-funs)))

         (definitions
           (append
            (loop :for (name . node) :in (tc:translation-unit-definitions translation-unit)
                  :for compiled-node := (compile-toplevel (tc:fresh-inst (tc:lookup-value-type env name)) node env)
                  :do (when coalton-impl::*coalton-dump-ast*
                        (format t "~A :: ~A~%~A~%~%~A~%~%"
                                name
                                (tc:lookup-value-type env name)
                                node
                                compiled-node))
                  :collect (cons name compiled-node))
            (loop :for instance :in (tc:translation-unit-instances translation-unit)
                  :append (compile-instance instance add-inline env))))

         (hoister
           (make-hoister))

         (definitions
           (append
            (loop :for (name . node) :in definitions
                  :for pointfree-node := (pointfree node)
                  :for canonicalized-node := (canonicalize pointfree-node)
                  :for method-inline-node := (inline-methods canonicalized-node env)
                  :for lifted-node := (static-dict-lift method-inline-node hoister (tc:translation-unit-package translation-unit) env)
                  :do (typecheck-node lifted-node env)
                  :collect (cons name lifted-node))
            (pop-final-hoist-point hoister)))

         (sccs (node-binding-sccs definitions)))

    (setf env (update-function-env definitions env))

    (let* ((function-table (make-function-table env))

           (definitions
             (loop :for (name . node) :in definitions
                   :for direct-node := (direct-application node function-table)
                   :do (typecheck-node direct-node env)
                   :collect (cons name direct-node))))

      (values
       `(progn
          ,@(loop :for name :in inline-funs
                  :collect `(declaim (inline ,name)))

          (eval-when (:compile-toplevel :load-toplevel :execute)
            ,@(loop :for type :in (tc:translation-unit-types translation-unit)
                    :append (codegen-type-definition type env)))

          (eval-when (:compile-toplevel :load-toplevel :execute)
            ,@(codegen-class-definitions
               (tc:translation-unit-classes translation-unit)
               env))

          #+sbcl
          ,@(when (eq sb-ext:*block-compile-default* :specified)
              (list `(declaim (sb-ext:start-block ,@(mapcar #'car definitions)))))

          ,@(loop :for scc :in sccs
                  :for bindings
                    := (remove-if-not
                        (lambda (binding)
                          (find (car binding) scc))
                        definitions)
                  :append (compile-scc bindings env))

          #+sbcl
          ,@(when (eq sb-ext:*block-compile-default* :specified)
              (list `(declaim (sb-ext:end-block))))

          (values))
       env))))

(defun split-binding-definitions (bindings)
  (let ((functions nil)
        (variables nil))
    (loop :for (name . node) :in bindings
          :do (if (node-abstraction-p node)
                  (push (cons name (length (node-abstraction-vars node))) functions)
                  (push name variables)))
    (values functions variables)))

(defun update-function-env (bindings env)
  (declare (type binding-list bindings)
           (type tc:environment env)
           (values tc:environment))
  (multiple-value-bind (toplevel-functions toplevel-values)
      (split-binding-definitions bindings)
    (loop :for (name . arity) :in toplevel-functions
          :do
             (setf env
                   (tc:set-function
                    env
                    name
                    (tc:make-function-env-entry
                     :name name
                     :arity arity))))
    (loop :for name :in toplevel-values
          :do
             (setf env (tc:unset-function env name))))
  env)

(defun make-function-table (env)
  (declare (type tc:environment env)
           (values hash-table))
  (let ((table (make-hash-table)))
    (fset:do-map (name entry (immutable-map-data (tc:environment-function-environment env)))
      (setf (gethash name table) (tc:function-env-entry-arity entry)))
    table))

(defun compile-function (name node env)
  (declare (type symbol name)
           (type node-abstraction node)
           (type tc:environment env))
  (let ((type-decs
           (when coalton-impl:*emit-type-annotations*
             (append
              (loop :for name :in (node-abstraction-vars node)
                    :for i :from 0
                    :for arg-ty := (nth i (tc:function-type-arguments (node-type node)))
                    :collect `(type ,(lisp-type arg-ty env) ,name))
              (list `(values ,(lisp-type (node-type (node-abstraction-subexpr node)) env)
                             &optional))))))

    `(defun ,name ,(node-abstraction-vars node)
       (declare (ignorable ,@(node-abstraction-vars node))
                ,@type-decs)
       ,(codegen-expression (node-abstraction-subexpr node) name env))))

(defun compile-scc (bindings env)
  (declare (type binding-list bindings)
           (type tc:environment env))
  (append
   ;; Predeclare symbol macros
   (loop :for (name . node) :in bindings
         :collect `(coalton-impl:define-global-lexical ,name ':|@@unbound@@|))

   ;; Compile functions
   (loop :for (name . node) :in bindings
         :if (node-abstraction-p node)
           :append (list
                    (compile-function name node env)
                    `(setf
                      ,name
                      ,(construct-function-entry
                       `#',name (length (node-abstraction-vars node))))))

  ;; Compile variables
  (loop :for (name . node) :in bindings
        :if (not (node-abstraction-p node))
          :collect `(setf
                     ,name
                     ,(codegen-expression node nil env)))
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
