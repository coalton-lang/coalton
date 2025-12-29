(coalton-cffi/utils:define-cffi-package #:coalton-cffi/functions
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-cffi/types)
  (:export
   #:define-foreign-function))

(in-package #:coalton-cffi/functions)

(named-readtables:in-readtable coalton:coalton)

(cl:defun check-arg-name (name)
  "Ensure that `name` is a symbol."
  (cl:typecase name
    (cl:symbol
     name)
    (cl:t
     (cl:error "Expected symbol for arg name but got ~S." name))))

(cl:defun check-arg-type (type)
  "Ensure that `type` is a known Coalton type."
  (cl:handler-case
      (cl:eval `(coalton (fn (x) (the ,type x))))
    (cl:error () (cl:error "Unknown arg type ~S." type)))
  type)

(cl:defun parse-arg (arg)
  "Extract and validate the name and type from `slot`."
  (cl:typecase arg
    (cl:list
     (cl:destructuring-bind (name type . rest) arg
       (check-arg-name name)
       (check-arg-type type)
       (cl:unless (cl:endp rest)
         (cl:error "Unexpected trailing form in function argument ~S." rest))
       (cl:values name type)))
    (cl:t
     (cl:error "Expected list for arg but got ~S." arg))))

(cl:defun parse-args (args)
  "Extract and validate a docstring and the argument names, types, and foreign types from `args`."
  (cl:loop
     :with docstring := (cl:if (cl:stringp (cl:first args)) (cl:pop args) "")
     :for arg :in args
     :for (name type) := (cl:multiple-value-list (parse-arg arg))
     :for ctype := (cl:handler-case (coalton-type-to-foreign-type type)
                     (cl:error ()
                       (cl:error "Unknown instance FOREIGNREPR ~S." type)))
     :collect name  :into names
     :collect type  :into types
     :collect ctype :into ctypes
     :finally (cl:return (cl:values docstring names types ctypes))))

(cl:defun check-function-name (name)
  "Ensure that `name` is a symbol."
  (cl:typecase name
    (cl:symbol
     name)
    (cl:t
     (cl:error "Expected symbol for function name but got ~S." name))))

(cl:defun check-function-cname (cname)
  "Ensure that `cname` is a string."
  (cl:typecase cname
    (cl:string
     cname)
    (cl:t
     (cl:error "Expected string for function cname but got ~S." cname))))

(cl:defun check-return-type (type)
  "Ensure that `type` is a known Coalton type."
  (cl:handler-case
      (cl:eval `(coalton (fn (x) (the ,type x))))
    (cl:error () (cl:error "Unknown return type ~S." type)))
  type)

(cl:defmacro define-foreign-function ((name cname) return-type cl:&body args)
  "Define a new foreign function called `name`.

`name` is a symbol that names the new Coalton function.

`cname` is a string that represents the C function to which the FFI should refer.

`return-type` is a valid Coalton type that is an instance of `ForeignRepr`.

The first element of `args` may be a docstring.

Each of `args` is of the form `(name type)`, where `name` is a symbol, and `type` is a known Coalton type that is an instance of `ForeignRepr`.

First, `cffi:defcfun` is used to define an inlined foreign function call using the Coalton function name prefixed with \"CL-\".

Then, a Coalton function is defined using the names and types provided, which calls the Common Lisp foreign function defined previously.

For example, the following are equivalent.

(define-foreign-function (memcpy \"memcpy\") (Pointer Void)
  \"Copy `n` bytes from `src` to `dest`.\"
  (dest (Pointer Void))
  (src  (Pointer Void))
  (n    Size))

(cl:progn

  (cl:declaim (cl:inline cl-memcpy))
  (cffi:defcfun (cl-memcpy \"memcpy\") :pointer
    \"Copy `n` bytes from `src` to `dest`.\"
    (dest :pointer)
    (src  :pointer)
    (n    :size))

  (coalton-toplevel
    (inline)
    (declare memcpy (Pointer Void -> Pointer Void -> Size -> Pointer Void))
    (define (memcpy dest src n)
      \"Copy `n` bytes from `src` to `dest`.\"
      (lisp (Pointer Void) (dest src n)
        (cl-memcpy dest src n)))))"
  (check-function-name name)
  (check-function-cname cname)
  (check-return-type return-type)

  (cl:let ((return-ctype
             (cl:handler-case (coalton-type-to-foreign-type return-type)
               (cl:error ()
                 (cl:error "Unknown instance FOREIGNREPR ~S" return-type)))))

    (cl:multiple-value-bind
          (docstring arg-names arg-types arg-ctypes)
        (parse-args args)

      (cl:let ((cl-name (cl:intern (uiop:strcat "CL-" (cl:symbol-name name)))))

        `(cl:progn

           (cl:declaim (cl:inline ,cl-name))
           (cffi:defcfun (,cl-name ,cname) ,return-ctype
             ,docstring
             ,@(cl:mapcar #'cl:list arg-names arg-ctypes))

           (coalton-toplevel
             (inline)
             (declare ,name (,@(cl:loop
                                  :for arg-type :in arg-types
                                  :collect arg-type
                                  :collect '->)
                             ,return-type))
             (define (,name ,@arg-names)
               ,docstring
               (lisp ,return-type (,@arg-names)
                 (,cl-name ,@arg-names)))))))))
