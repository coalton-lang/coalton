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
                              :name (car method)
                              :type (tc:lisp-type (cdr method) env))))

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

(defun make-method-fun (m package class env)
  (declare (type tc:environment env))
  (let* ((qual-ty (tc:fresh-inst (cdr m)))

         (method-contraint-args
           (length (tc:qualified-ty-predicates qual-ty)))

         (arity (+ (tc:function-type-arity
                    (tc:qualified-ty-type qual-ty))
                   method-contraint-args))
         (params
           (loop :for i :from 0 :below arity
                 :collect (alexandria:format-symbol package "_~A" i)))
         (class-codegen-sym (tc:ty-class-codegen-sym class))
         (method-accessor (alexandria:format-symbol (symbol-package class-codegen-sym) "~A-~A" class-codegen-sym (car m))))

    `((declaim (inline ,(car m)))
      (defun ,(car m) (dict ,@params)
        (declare #.settings:*coalton-optimize*)
        ,(if (null params)
             `(,method-accessor dict)
             `(rt:call-coalton-function (,method-accessor dict) ,@params)))
      ;; Generate the wrapper functions
      (global-lexical:define-global-lexical ,(car m) rt:function-entry)
      (setf ,(car m) ,(rt:construct-function-entry
                       `#',(car m)
                       (+ arity 1) ; We need a function of arity + 1 to account for DICT
                       ))
      (setf (documentation ',(car m) 'variable)
            ,(format nil "~A :: ~A" (car m) (tc:lookup-value-type env (car m))))
      (setf (documentation ',(car m) 'function)
            ,(format nil "~A :: ~A" (car m) (tc:lookup-value-type env (car m)))))))
