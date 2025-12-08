(defpackage #:coalton-impl/codegen/struct-or-class
  (:use
   #:cl
   #:coalton-compatibility-layer)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:global-lexical #:coalton-impl/global-lexical)
   (#:rt #:coalton-impl/runtime)
   (#:compat #:coalton-compatibility-layer))
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
  (if (not (settings:coalton-release-p))
      nil
      xs))

(defun struct-or-class (&key
                          (classname (error "Class Name required"))
                          (constructor (error "Constructor required"))
                          (superclass nil)
                          (fields nil)
                          mode)
  "Generate either a DEFSTRUCT or DEFCLASS for a type depending on the MODE argument.

The intention is that MODE, which is one of ':CLASS or ':STRUCT, is
itself reflective of the Coalton release mode, where development mode
strongly correlates to ':CLASS and release mode to
':STRUCT.

Ultimately, the caller may decide to force either ':CLASS or ':STRUCT
regardless of Coalton's release mode."
  (declare (type symbol classname constructor superclass)
           (type list fields)
           (type (member :class :struct) mode))

  (let ((field-names (mapcar #'struct-or-class-field-name fields))
        (reader-names (loop :for field :in fields
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
         ;; Declare the types of the constructor and readers:
         ;;
         ;; XXX: These seem to cause bugs because they overwrite or
         ;; disagree with SBCL's own conception of the types of these
         ;; functions. Maybe we can re-enable or revise later, hence
         ;; we'll keep the code.
         ;;
         ;; As a specific example, readers are inferred to be
         ;; (FUNCTION (T) ...) as opposed to (FUNCTION (STRUCT-TYPE)
         ;; ...).
         #+#:broken-on-sbcl
         (when settings:*emit-type-annotations*
           (list*
            `(declaim (ftype (function
                              ,(mapcar #'struct-or-class-field-type fields)
                              (values ,classname &optional))
                             ,constructor))
            (loop :for field :in fields
                  :for reader-name :in reader-names
                  :for lisp-type := (struct-or-class-field-type field)
                  :collect `(declaim (ftype (function (,classname)
                                                      (values ,lisp-type &optional))
                                            ,reader-name)))))

         ;; Inline constructor and readers (release-only):
         (list-if-release
          `(declaim (inline ,constructor ,@reader-names)))

         ;; Define the struct:
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
                     :collect `(,name nil :type ,lisp-type :read-only t))))

         ;; Freeze the type (release-only):
         (list-if-release
          `(compat:try-freeze-type ,classname))))

       (:class
        (append
         (list
          ;; Class (and slot) definitions:
          `(defclass ,classname ,(if superclass
                                     (list superclass)
                                     (list))
             ,(loop :for field :in fields
                    :for field-name :in field-names
                    :for reader-name :in reader-names
                    :for lisp-type := (struct-or-class-field-type field)
                    :collect `(,field-name
                               :type ,lisp-type
                               :initarg ,field-name))))

         ;; Class reader definitions:
         (loop :for field :in fields
               :for field-name :in field-names
               :for reader-name :in reader-names
               :for lisp-type := (struct-or-class-field-type field)
               :when settings:*emit-type-annotations*
                 :collect `(declaim (ftype (function (,classname)
                                                     (values ,lisp-type &optional))
                                           ,reader-name))
               :when (settings:coalton-release-p)
                 :collect `(declaim (inline ,reader-name))
               :collect `(defun ,reader-name (obj)
                           (slot-value obj ',field-name)))

         ;; Constructor function definition:
         (when settings:*emit-type-annotations*
           (list
            `(declaim (ftype (function
                              ,(mapcar #'struct-or-class-field-type fields)
                              (values ,classname &optional))
                             ,constructor))))

         (list-if-release
          `(declaim (inline ,constructor)))

         (list
          `(defun ,constructor ,field-names
             (make-instance ',classname ,@(mapcan
                                           (lambda (field)
                                             `(',field ,field))
                                           field-names)))))))
     (if (not (null fields))
         (append
          (list
           `(global-lexical:define-global-lexical ,constructor rt:function-entry)
           `(setf ,constructor ,(rt:construct-function-entry `#',constructor (length fields))))
          (loop :for reader :in reader-names
                :collect `(global-lexical:define-global-lexical ,reader rt:function-entry)
                :collect `(setf ,reader ,(rt:construct-function-entry `#',reader 1))))

         (list
          `(global-lexical:define-global-lexical ,constructor ,classname)
          `(setf ,constructor (,constructor)))))))

