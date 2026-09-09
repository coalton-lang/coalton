(defpackage #:coalton-impl/codegen/codegen-exception
  (:use
   #:cl)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:global-lexical #:coalton-impl/global-lexical)
   (#:soc #:coalton-impl/codegen/struct-or-class))
  
  (:export
   #:codegen-exception                  ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/codegen-exception)

(defun codegen-exception (&key
                            (classname (error "Exception Name required"))
                            (constructor (error "Constructor required"))
                            (superclass nil)
                            (fields nil))
  "Generate DEFINE-CONDITION for an exception type."
  (declare (type symbol classname constructor)
           (type list fields))
  (let ((field-names (mapcar #'soc:struct-or-class-field-name fields)))
    (append 
     (list
      `(define-condition ,classname ,(if superclass (list superclass) '(cl:error))
         ,(loop :for field :in fields
                :for name := (soc:struct-or-class-field-name field)
                :for lisp-type := (soc:struct-or-class-field-type field)
                :for package := (symbol-package classname)
                :for accessor
                  := (alexandria:format-symbol package "~A-~A" classname name)
                :collect `(,name
                           :type ,lisp-type
                           :initarg ,name
                           :accessor ,accessor))
         ,@(when fields
             `((:default-initargs
                ,@(loop :for field :in fields
                        :for name := (soc:struct-or-class-field-name field)
                        :append `(,name (error ""))))))))

     (list
      `(defun ,constructor ,field-names
         ,@(when settings:*emit-type-annotations*
             `((declare ,@(loop :for field :in fields
                                :collect `(type ,(soc:struct-or-class-field-type field)
                                                ,(soc:struct-or-class-field-name field))))))
         (make-instance ',classname ,@(mapcan
                                       (lambda (field)
                                         `(',field ,field))
                                       field-names))))
     ;; Constructors are also values: nullary constructors denote an instance,
     ;; while constructors with fields denote a first-class function.
     (if fields
         (list
          `(global-lexical:define-global-lexical ,constructor function)
          `(setf ,constructor #',constructor))
         (list
          `(global-lexical:define-global-lexical ,constructor ,classname)
          `(setf ,constructor (,constructor)))))))
