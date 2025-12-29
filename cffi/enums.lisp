(coalton-cffi/utils:define-cffi-package #:coalton-cffi/enums
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-cffi/aliases
   #:coalton-cffi/types
   #:coalton-cffi/pointers)
  (:export
   #:define-foreign-enum))

(in-package #:coalton-cffi/enums)

(named-readtables:in-readtable coalton:coalton)

(cl:defun check-name (name)
  "Ensure that `name` is a symbol."
  (cl:typecase name
    (cl:symbol
     name)
    (cl:t
     (cl:error "Expected symbol for name but got ~S." name))))

(cl:defun check-type (type)
  "Ensure that `type` is a known Coalton type."
  (cl:handler-case
      (cl:eval `(coalton (fn (x) (the ,type x))))
    (cl:error () (cl:error "Unknown return type ~S." type)))
  type)

(cl:defun check-rest (rest)
  "Ensure that `rest` is `cl:nil`."
  (cl:typecase rest
    (cl:null
     rest)
    (cl:t
     (cl:error "Unexpected trailing form in name ~S." rest))))

(cl:defun parse-name (name)
  "Extract and validate the name, base Coalton type, and base foreign type from `name`."
  (cl:typecase name
    ((cl:or cl:symbol (cl:cons cl:symbol cl:null))
     (cl:when (cl:listp name)
       (cl:setf name (cl:first name)))
     (cl:values name 'Int ':int))
    (cl:list
     (cl:destructuring-bind (name type . rest) name
       (check-name name)
       (check-type type)
       (check-rest rest)
       (cl:let ((ctype
                  (cl:handler-case (coalton-type-to-foreign-type type)
                    (cl:error ()
                      (cl:error "Unknown instance FOREIGNREPR ~S." type)))))
         (cl:values name type ctype))))
    (cl:t
     (cl:error "Expected symbol or list for name but got ~S." name))))

(cl:defun check-member-name (name)
  "Ensure that `name` is a symbol."
  (cl:typecase name
    (cl:symbol
     name)
    (cl:t
     (cl:error "Expected symbol for member name but got ~S." name))))

(cl:defun check-member-index (index)
  "Ensure that `index` is a fixnum."
  (cl:typecase index
    (cl:fixnum
     index)
    (cl:t
     (cl:error "Expected fixnum for member index but got ~S." index))))

(cl:defun check-member-rest (rest)
  "Ensure that `rest` is `cl:nil`."
  (cl:typecase rest
    (cl:null
     rest)
    (cl:t
     (cl:error "Unexpected trailing form in member ~S." rest))))

(cl:defun contains-duplicates-p (sequence cl:&key (test #'cl:eql))
  "Does `sequence` contain any duplicates with respect to `test`?"
  (cl:declare (cl:values cl:boolean cl:&optional))
  (cl:not (cl:= (cl:length sequence)
                (cl:length (cl:remove-duplicates sequence :test test)))))

(cl:defun check-member-names (names)
  "Ensure that `names` does not contain any duplicates."
  (cl:when (contains-duplicates-p (cl:mapcar #'cl:symbol-name names)
                                  :test #'cl:string=)
    (cl:error "Duplicates in member names."))
  names)

(cl:defun check-member-indices (indices)
  "Ensure that `indices` does not contain any duplicates."
  (cl:when (contains-duplicates-p indices :test #'cl:=)
    (cl:error "Duplicates in member indices."))
  indices)

(cl:defun parse-members (members)
  "Extract and validate the names and indices from `members`."
  (cl:let ((next-index 0))
    (cl:flet
        ((parse-member (member)
           (cl:typecase member
             ((cl:or cl:symbol (cl:cons cl:symbol cl:null))
              (cl:when (cl:listp member)
                (cl:setf member (cl:first member)))
              (cl:incf next-index)
              (cl:values member (cl:1- next-index)))
             (cl:list
              (cl:destructuring-bind (name index . rest) member
                (check-member-name name)
                (check-member-index index)
                (check-member-rest rest)
                (cl:setf next-index (cl:max next-index (cl:1+ index)))
                (cl:values name index)))
             (cl:t
              (cl:error "Expected symbol or list for member but got ~S."
                        member)))))
      (cl:loop
         :with docstring := (cl:if (cl:stringp (cl:first members))
                                   (cl:pop members)
                                   "")
         :for member :in members
         :for (name index) := (cl:multiple-value-list (parse-member member))
         :collect name :into names
         :collect index :into indices
         :finally (cl:progn
                    (check-member-names names)
                    (check-member-indices indices)
                    (cl:return (cl:values docstring names indices)))))))

(cl:defmacro define-foreign-enum (name cl:&body members)
  "Define a new foreign enumeration type called `name`.

`name` is a symbol that names the new Coalton type or a list containing that symbol and a Coalton type that should be used as the base type for the enum. The default base type is `Int`.

Each of `members` is a symbol naming an enum member or a list containing that symbol and an index that should be used for the enum member. The default indices start at 0 and increment by 1.

First, a Coalton ADT with `(repr :enum)` is defined using the names provided.

Then, `cffi:defcenum` is used to define a foreign enum using the name provided prefixed with `\"CL-\"`, the symbols defined by the Coalton compiler, and the indices as described above.

Then, instances of `ForeignRepr` and `SimpleForeignRepr` are defined for the new type, as well as an instance of `Into` from the new type to the base type and of `TryInto` from the base type to the new type.

For example, the following are equivalent.

(define-foreign-enum T
  A
  (B 3))

(cl:progn

  (coalton-toplevel
    (repr :enum)
    (define-type T
      A
      B))

  (cffi:defcenum cl-t
    (T/A 0)
    (T/B 3))

  (define-foreign-repr-instance T cl-t)
  (define-simple-foreign-repr-instance T)

  (coalton-toplevel

    (define-instance (Into T Int)
      (define (into x)
        (match x
          ((A) 0)
          ((B) 3))))

    (define-instance (TryInto Int T String)
      (define (tryinto x)
        (match x
          (0 (Ok A))
          (3 (Ok B))
          (_ (Err \"Not a member.\")))))))"
  (cl:multiple-value-bind (name type ctype)
      (parse-name name)
    (cl:multiple-value-bind (docstring member-names member-indices)
        (parse-members members)
      (cl:let ((cl-name
                 (cl:intern (uiop:strcat "CL-" (cl:symbol-name name))))
               (cl-names
                 (cl:loop
                    :with prefix := (uiop:strcat (cl:symbol-name name) "/")
                    :for member-name :in member-names
                    :for namestring := (cl:symbol-name member-name)
                    :collect (cl:intern (uiop:strcat prefix namestring))))
               (x (cl:gensym "X")))
        `(cl:progn

           (coalton-toplevel
             (repr :enum)
             (derive Eq Hash)
             (define-type ,name
               ,docstring
               ,@member-names))

           (cffi:defcenum (,cl-name ,ctype)
             ,@(cl:loop
                  :for name :in cl-names
                  :for index :in member-indices
                  :collect `(,name ,index)))

           (define-foreign-repr-instance ,name ,cl-name)
           (define-simple-foreign-repr-instance ,name)

           (coalton-toplevel

             (define-instance (Into ,name ,type)
               (define (into ,x)
                 (match ,x
                   ,@(cl:loop
                        :for name  :in member-names
                        :for index :in member-indices
                        :collect `((,name) ,index)))))

             (define-instance (TryInto ,type ,name String)
               (define (tryinto ,x)
                 (match ,x
                   ,@(cl:loop
                        :for index :in member-indices
                        :for name :in member-names
                        :collect `(,index (Ok ,name)))
                   (_ (Err "Not a member value.")))))))))))
