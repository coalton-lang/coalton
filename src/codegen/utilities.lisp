(in-package #:coalton-impl/codegen)

(defstruct struct-or-class-field
  (name (required 'name) :type symbol :read-only t)
  (type (required 'type) :type t      :read-only t))

#+sbcl
(declaim (sb-ext:freeze-type struct-or-class-field))

(defun struct-or-class (&key
                          (classname (error "Class Name required"))
                          (constructor (error "Constructor required"))
                          (superclass nil)
                          (fields nil)
                          (mode (error "Mode required")))
  (declare (type symbol classname)
           (type symbol constructor)
           (type symbol superclass)
           (type list fields)
           (type (member :class :struct) mode))

  (let ((field-names (mapcar #'struct-or-class-field-name fields)))

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
                   :for type := (struct-or-class-field-type field)
                   :collect `(,name (error "") :type ,type :read-only t)))))

      (:class
       (list 
        `(defclass ,classname
             ,(if superclass
                  (list superclass)
                  (list))
           ,(loop :for field :in fields
                  :for name := (struct-or-class-field-name field)
                  :for type := (struct-or-class-field-type field)
                  :for package := (symbol-package classname)
                  :for accessor
                    := (alexandria:format-symbol package "~A-~A" classname name)
                  :collect `(,name
                             :initform (error "")
                             :initarg ,name
                             :type ,type
                             :accessor ,accessor)))

        `(defun ,constructor ,field-names
           (make-instance ',classname ,@(mapcan
                                         (lambda (field)
                                           `(',field ,field))
                                         field-names))))))))
