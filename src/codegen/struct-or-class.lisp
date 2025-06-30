(defpackage #:coalton-impl/codegen/struct-or-class
  (:use
   #:cl)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:global-lexical #:coalton-impl/global-lexical)
   (#:rt #:coalton-impl/runtime))
  (:export
   #:struct-or-class                    ; FUNCTION
   #:struct-or-class-field              ; STRUCT
   #:make-struct-or-class-field         ; CONSTRUCTOR
   #:struct-or-class-field-name         ; ACCESSOR
   #:struct-or-class-field-type         ; ACCESSOR
   ))

(in-package #:coalton-impl/codegen/struct-or-class)

(defstruct struct-or-class-field
  (name (util:required 'name) :type symbol :read-only t)
  (type (util:required 'type) :type t      :read-only t))

(defun list-if-release (&rest xs)
  (if (not (coalton-impl/settings:coalton-release-p))
      nil
      xs))

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

  (let ((field-names (mapcar #'struct-or-class-field-name fields))
        (accessor-names (loop :for field :in fields
                              :for package := (symbol-package classname)
                              :collect (alexandria:format-symbol
                                        package
                                        "~A-~A"
                                        classname
                                        (struct-or-class-field-name field)))))

    (append
     (ecase mode
       (:struct
        (append
         ;; Inline constructor and readers (release-only).
         (list-if-release
          `(declaim (inline ,constructor ,@accessor-names)))
         ;; Define the struct.
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
                     :collect `(,name (error "") :type ,lisp-type))))
         ;; Freeze the type (release-only).
         (list-if-release
          #+sbcl
          `(declaim (sb-ext:freeze-type ,classname)))))

       (:class
        (append 
         (list
          `(defclass ,classname
               ,(if superclass
                    (list superclass)
                    (list))
             ,(loop :for field :in fields
                    :for field-name :in field-names
                    :for accessor-name :in accessor-names
                    :for lisp-type := (struct-or-class-field-type field)
                    :collect `(,field-name
                               :type ,lisp-type
                               :initarg ,field-name
                               :accessor ,accessor-name))
             (:default-initargs
              ,@(loop :for field-name :in field-names
                      :append `(,field-name (error ""))))))

         (list
          ;; NOTE: We are omitting the inline call because this causes
          ;; SBCL IR1 bugs for instance definitions.
          ;;
          ;; `(declaim (inline ,constructor))
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
          (list
           `(global-lexical:define-global-lexical ,constructor rt:function-entry)
           `(setf ,constructor ,(rt:construct-function-entry `#',constructor (length fields))))
          (loop :for reader :in accessor-names
                :collect `(global-lexical:define-global-lexical ,reader rt:function-entry)
                :collect `(setf ,reader ,(rt:construct-function-entry `#',reader 1))))

         (list
           `(global-lexical:define-global-lexical ,constructor ,classname)
           `(setf ,constructor (,constructor)))))))

