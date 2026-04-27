(defpackage #:coalton-impl/codegen/codegen-class
  (:use
   #:cl)
  (:import-from
   #:coalton-impl/codegen/struct-or-class
   #:struct-or-class
   #:make-struct-or-class-field)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:settings #:coalton-impl/settings)
   (#:global-lexical #:coalton-impl/global-lexical)
   (#:rt #:coalton-impl/runtime)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:codegen-class-definitions))

(in-package #:coalton-impl/codegen/codegen-class)

;;; Method wrapper keyword generation.
;;;
;;; These parallel the keyword handling in codegen-expression (abstraction-
;;; lambda-list, keyword-tail-forms), but operate on class method types
;;; rather than codegen AST nodes.  They use plists instead of keyword-param
;;; structs because the specs are generated from type information alone,
;;; without a parsed AST node.

(defun method-wrapper-keyword-specs (visible-type)
  (declare (type tc:ty visible-type)
           (values list &optional))
  (when (typep visible-type 'tc:function-ty)
    (loop :for entry :in (tc:function-ty-keyword-input-types visible-type)
          :collect (list :keyword (tc:keyword-ty-entry-keyword entry)
                         :var (gensym "KEYWORD-ARG-")
                         :supplied-p-var (gensym "KEYWORD-SUPPLIED-P-")))))

(defun method-wrapper-lambda-list (positional-params keyword-specs)
  (declare (type list positional-params keyword-specs)
           (values list &optional))
  (append
   (list 'dict)
   positional-params
   (when keyword-specs
     (list* '&key
            (loop :for spec :in keyword-specs
                  :collect `((,(getf spec :keyword) ,(getf spec :var))
                             nil
                             ,(getf spec :supplied-p-var)))))))

(defun method-wrapper-call (method-accessor visible-type positional-params keyword-specs)
  (declare (type symbol method-accessor)
           (type tc:ty visible-type)
           (type list positional-params keyword-specs)
           (values t &optional))
  (cond
    ((not (null keyword-specs))
     `(apply #'rt:call-coalton-function
             (,method-accessor dict)
             (append
              (list ,@positional-params)
              ,@(loop :for spec :in keyword-specs
                      :collect `(if ,(getf spec :supplied-p-var)
                                    (list ,(getf spec :keyword) ,(getf spec :var))
                                    '())))))
    ((not (null positional-params))
     `(rt:exact-call (,method-accessor dict) ,@positional-params))
    ((tc:function-type-p visible-type)
     `(rt:exact-call (,method-accessor dict)))
    (t
     `(,method-accessor dict))))

(defun codegen-class-definitions (classes env)
  (declare (type tc:ty-class-list classes)
           (type tc:environment env)
           (values list))
  (loop :for class :in classes
        :for name := (tc:ty-class-name class)
        :for codegen-name := (tc:ty-class-codegen-sym class)
        :for package := (symbol-package name)

        :for fields
          := (append
              (loop :for (superclass-pred . field-name) :in (tc:ty-class-superclass-dict class)
                    :for superclass := (tc:lookup-class env (tc:ty-predicate-class superclass-pred))
                    :collect (make-struct-or-class-field
                              :name field-name
                              :type (tc:ty-class-codegen-sym superclass)))

              (loop :for method :in (tc:ty-class-unqualified-methods class)
                    :collect (make-struct-or-class-field
                              :name (tc:ty-class-method-name method)
                              :type (tc:lisp-type (tc:ty-class-method-type method) env))))

        :append (struct-or-class
                 :classname codegen-name
                 :constructor codegen-name
                 :fields fields
                 :mode (if (settings:coalton-release-p)
                           :struct
                           :class))

        :append (mapcan (lambda (m)
                          (make-method-fun m package class))
                        (tc:ty-class-unqualified-methods class))))

(defun make-method-fun (method package class)
  (declare (type tc:ty-class-method method))
  (let* ((qual-ty
           (tc:fresh-inst (tc:ty-class-method-type method)))
         (visible-type
           (tc:qualified-ty-type qual-ty))
         (method-contraint-args
           (length (tc:qualified-ty-predicates qual-ty)))
         (keyword-specs
           (method-wrapper-keyword-specs visible-type))
         (arity (+ (tc:function-type-arity (tc:qualified-ty-type qual-ty))
                   method-contraint-args))
         (params
           (loop :for i :from 0 :below arity
                 :collect (alexandria:format-symbol package "_~A" i)))
         (class-codegen-sym
           (tc:ty-class-codegen-sym class))
         (method-name
           (tc:ty-class-method-name method))
         (method-docstring
           (source:docstring method))
         (method-accessor
           (alexandria:format-symbol (symbol-package class-codegen-sym)
                                     "~A-~A" class-codegen-sym method-name)))
    `((declaim (inline ,method-name))
      (defun ,method-name ,(method-wrapper-lambda-list params keyword-specs)
        (declare #.settings:*coalton-optimize*)
        (declare (ignorable dict ,@params
                            ,@(loop :for spec :in keyword-specs
                                    :append (list (getf spec :var)
                                                  (getf spec :supplied-p-var)))))
        ,(method-wrapper-call method-accessor visible-type params keyword-specs))
      ;; Generate the wrapper functions
      (global-lexical:define-global-lexical ,method-name rt:function-entry)
      (setf ,method-name
            ;; We need a function of arity + 1 to account for DICT
            ,(rt:construct-function-entry `#',method-name (+ arity 1)))
      ,@(when method-docstring
          `((setf (documentation ',method-name 'variable)
                  ,method-docstring)
            (setf (documentation ',method-name 'function)
                  ,method-docstring))))))
