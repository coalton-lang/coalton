(defpackage #:coalton-impl/codegen/struct-or-class
  (:use
   #:cl
   #:coalton-impl/util)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:global-lexical #:coalton-impl/global-lexical)
   (#:rt #:coalton-impl/runtime))
  (:export
   #:struct-or-class
   #:struct-or-class-field
   #:make-struct-or-class-field
   #:struct-or-class-field-name
   #:struct-or-class-field-type))

(in-package #:coalton-impl/codegen/struct-or-class)

(defstruct struct-or-class-field
  (name (required 'name) :type symbol :read-only t)
  (type (required 'type) :type t      :read-only t))

(defun struct-or-class (&key
                              (classname (error "Class Name required"))
                              (constructor (error "Constructor required"))
                              (superclass nil)
                              (fields nil)
                              mode)
  (declare (type symbol classname)
           (type symbol constructor)
           (type symbol superclass)
           (type list fields)
           (type (member :class :struct) mode))

  (let ((field-names (mapcar #'struct-or-class-field-name fields)))

    (append
     (ecase mode
       (:struct
        (list
         `(defstruct (,classname
                      (:copier nil)
                      (:predicate nil)
                      ,@(when superclass
                          (list `(:include ,superclass)))
                      (:constructor ,constructor ,field-names)) 
            ,@(loop :for field :in fields
                    :for name := (struct-or-class-field-name field)
                    :for lisp-type := (struct-or-class-field-type field)
                    :collect `(,name (error "") :type ,lisp-type)))))

       (:class
        (append 
         (list
          `(defclass ,classname
               ,(if superclass
                    (list superclass)
                    (list))
             ,(loop :for field :in fields
                    :for name := (struct-or-class-field-name field)
                    :for lisp-type := (struct-or-class-field-type field)
                    :for package := (symbol-package classname)
                    :for accessor
                      := (alexandria:format-symbol package "~A-~A" classname name)
                    :collect `(,name
                               :type ,lisp-type
                               :initarg ,name
                               :accessor ,accessor))
             (:default-initargs
              ,@(loop :for field :in fields
                      :for name := (struct-or-class-field-name field)
                      :append `(,name (error ""))))))

         (list
          `(declaim (inline ,constructor))
          `(defun ,constructor ,field-names
             ,@(when settings:*emit-type-annotations*
                 `((declare ,@(loop :for field :in fields
                                    :collect `(type ,(struct-or-class-field-type field) ,(struct-or-class-field-name field))))))
             (make-instance ',classname ,@(mapcan
                                           (lambda (field)
                                             `(',field ,field))
                                           field-names)))))))
     (if (not (null fields))
         (append
          `((global-lexical:define-global-lexical ,constructor rt:function-entry)
            (setf ,constructor ,(rt:construct-function-entry `#',constructor (length fields))))
          (loop :for field :in fields
                :for package := (symbol-package classname)
                :for field-name := (alexandria:format-symbol package "~A-~A" classname (struct-or-class-field-name field))
                :collect `(global-lexical:define-global-lexical ,field-name rt:function-entry)
                :collect `(setf ,field-name ,(rt:construct-function-entry `#',field-name 1))))

         (progn
           `((global-lexical:define-global-lexical ,constructor ,classname)
             (setf ,constructor (,constructor))))))))

