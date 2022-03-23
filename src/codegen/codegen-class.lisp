(defpackage #:coalton-impl/codegen/codegen-class
  (:use
   #:cl
   #:coalton-impl/util)
  (:import-from
   #:coalton-impl/codegen/struct-or-class
   #:struct-or-class
   #:make-struct-or-class-field)
  (:import-from
   #:coalton-impl/codegen/lisp-type
   #:lisp-type)
  (:import-from
   #:coalton-impl/codegen/function-entry
   #:apply-function-entry
   #:construct-function-entry)
  (:local-nicknames
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
                              :type (lisp-type (cdr method) env))))

        :append (struct-or-class
                  :classname codegen-name
                  :constructor codegen-name
                  :fields fields
                  :mode (if (coalton-impl:coalton-release-p)
                            :struct
                            :class))



        :append (mapcan (lambda (m)
                          (make-method-fun m package class))
                        (tc:ty-class-unqualified-methods class))))

(defun make-method-fun (m package class)
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

    ;; TODO: add type annotations
    `((declaim (inline ,(car m)))
      (defun ,(car m) (dict ,@params)
        ,(if (null params)
             `(,method-accessor dict)
             (apply #'apply-function-entry
                    `(,method-accessor dict) params)))
      ;; Generate the wrapper functions
      (coalton-impl:define-global-lexical ,(car m)
          ,(construct-function-entry
            `#',(car m)
            (+ arity 1) ; We need a function of arity + 1 to account for DICT
            )))))

