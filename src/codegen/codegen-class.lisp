(defpackage #:coalton-impl/codegen/codegen-class
  (:use
   #:cl)
  (:import-from
   #:coalton-impl/codegen/struct-or-class
   #:struct-or-class
   #:make-struct-or-class-field)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:global-lexical #:coalton-impl/global-lexical)
   (#:rt #:coalton-impl/runtime)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:codegen-class-definitions))

(in-package #:coalton-impl/codegen/codegen-class)

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
                          (make-method-fun m package class env))
                        (tc:ty-class-unqualified-methods class))))

(defun make-method-fun (method package class env)
  (declare (type tc:ty-class-method method)
           (type tc:environment env))
  (let* ((qual-ty
           (tc:fresh-inst (tc:ty-class-method-type method)))
         (method-contraint-args
           (length (tc:qualified-ty-predicates qual-ty)))
         (arity (+ (tc:function-type-arity (tc:qualified-ty-type qual-ty))
                   method-contraint-args))
         (params
           (loop :for i :from 0 :below arity
                 :collect (alexandria:format-symbol package "_~A" i)))
         (class-codegen-sym
           (tc:ty-class-codegen-sym class))
         (method-name
           (tc:ty-class-method-name method))
         (method-accessor
           (alexandria:format-symbol (symbol-package class-codegen-sym)
                                     "~A-~A" class-codegen-sym method-name)))
    `((declaim (inline ,method-name))
      (defun ,method-name (dict ,@params)
        (declare #.settings:*coalton-optimize*)
        ,(if (null params)
             `(,method-accessor dict)
             `(rt:call-coalton-function (,method-accessor dict) ,@params)))
      ;; Generate the wrapper functions
      (global-lexical:define-global-lexical ,method-name rt:function-entry)
      (setf ,method-name
            ;; We need a function of arity + 1 to account for DICT
            ,(rt:construct-function-entry `#',method-name (+ arity 1))
            (documentation ',method-name 'variable)
            ,(format nil "~A :: ~A" method-name (tc:lookup-value-type env method-name))
            (documentation ',method-name 'function)
            ,(format nil "~A :: ~A" method-name (tc:lookup-value-type env method-name))))))
