(defpackage #:coalton-impl/codegen/struct-or-class
  (:use
   #:cl
   #:coalton-impl/util)
  (:import-from
   #:coalton-impl/codegen/function-entry
   #:construct-function-entry)
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

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type struct-or-class-field))

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
                      ,@(when superclass
                          (list `(:include ,superclass)))
                      (:constructor ,constructor ,field-names)
                      (:predicate nil))
            ,@(loop :for field :in fields
                    :for name := (struct-or-class-field-name field)
                    :collect `(,name (error ""))))))

       (:class
        (list 
         `(defclass ,classname
              ,(if superclass
                   (list superclass)
                   (list))
            ,(loop :for field :in fields
                   :for name := (struct-or-class-field-name field)
                   :for package := (symbol-package classname)
                   :for accessor
                     := (alexandria:format-symbol package "~A-~A" classname name)
                   :collect `(,name
                              :initform (error "")
                              :initarg ,name
                              :accessor ,accessor)))

         `(defun ,constructor ,field-names
            (make-instance ',classname ,@(mapcan
                                          (lambda (field)
                                            `(',field ,field))
                                          field-names))))))
     (when (not (null fields))
       (cons
        `(coalton-impl:define-global-lexical ,constructor
             ,(construct-function-entry `#',constructor (length fields)))
        (loop :for field :in fields
              :for package := (symbol-package classname)
              :for field-name := (alexandria:format-symbol package "~A-~A" classname (struct-or-class-field-name field))
              :collect `(coalton-impl:define-global-lexical ,field-name
                          ,(construct-function-entry `#',field-name 1))))))))


