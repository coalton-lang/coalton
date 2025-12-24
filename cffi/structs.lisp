(coalton-cffi/utils:define-cffi-package #:coalton-cffi/structs
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-cffi/aliases
   #:coalton-cffi/types
   #:coalton-cffi/pointers)
  (:export
   #:Slot
   #:slot-offset
   #:slot-pointer
   #:slot-ref
   #:slot-set!
   #:slot-aref
   #:slot-aset!
   #:slot-aptr
   #:define-foreign-struct))

(in-package #:coalton-cffi/structs)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type (Slot :T :U)
    "A type for representing a slot of a foreign struct."
    (%Slot Offset))

  (inline)
  (declare slot-offset (Slot :T :U -> Offset))
  (define (slot-offset slot)
    "The offset of a foreign struct slot."
    (match slot
      ((%Slot offset)
       offset))))

(coalton-toplevel

  (inline)
  (declare slot-pointer (Pointer :T -> Slot :T :U -> Pointer :U))
  (define (slot-pointer struct slot)
    "Get a pointer to `slot` in `struct`."
    (let ptr    = (unsafe-reinterpret struct))
    (let offset = (slot-offset slot))
    (shift-pointer offset ptr))

  (inline)
  (declare slot-ref (SimpleForeignRepr :U => Pointer :T -> Slot :T :U -> :U))
  (define (slot-ref struct slot)
    "Get the element for `slot` in `struct`."
    (let ptr    = (unsafe-reinterpret struct))
    (let offset = (slot-offset slot))
    (mem-ref ptr offset))

  (inline)
  (declare slot-set!
           (SimpleForeignRepr :U => Pointer :T -> Slot :T :U -> :U -> Unit))
  (define (slot-set! struct slot value)
    "Set the element for `slot` in `struct` to `value`."
    (let ptr    = (unsafe-reinterpret struct))
    (let offset = (slot-offset slot))
    (mem-set! ptr offset value)))

(coalton-toplevel

  (inline)
  (declare slot-aref
           (SimpleForeignRepr :U => Pointer :T -> Slot :T :U -> UFix -> :U))
  (define (slot-aref struct slot index)
    "Get the `index`th element for `slot` in `struct`."
    (mem-aref (slot-pointer struct slot) index))

  (inline)
  (declare slot-aset!
           ((SimpleForeignRepr :U)
            => Pointer :T -> Slot :T :U -> UFix -> :U -> Unit))
  (define (slot-aset! struct slot index value)
    "Set the `index`th element for `slot` in `struct` to `value`."
    (mem-aset! (slot-pointer struct slot) index value))

  (inline)
  (declare slot-aptr
           (ForeignRepr :U => Pointer :T -> Slot :T :U -> UFix -> Pointer :U))
  (define (slot-aptr struct slot index)
    "Get a pointer to the `index`th element for `slot` in `struct`."
    (mem-aptr (slot-pointer struct slot) index)))

(cl:defun check-slot-name (name)
  "Ensure that `name` is a symbol."
  (cl:typecase name
    (cl:symbol
     name)
    (cl:t
     (cl:error "Expected symbol for slot name but got ~S." name))))

(cl:defun check-slot-count (count)
  "Ensure that `count` is a non-zero, positive fixnum."
  (cl:typecase count
    ((cl:and cl:fixnum (cl:integer 1))
     count)
    (cl:t
     (cl:error "Expected positive fixnum for slot count but got ~S." count))))

(cl:defun check-slot-type (type)
  "Ensure that `type` is a known Coalton type."
  (cl:handler-case
      (cl:eval `(coalton (fn (x) (the ,type x))))
    (cl:error () (cl:error "Unknown slot type ~S." type)))
  type)

(cl:defun check-slot-rest (rest)
  "Ensure that `rest` is a valid plist to be used with `cffi:defcstruct`."
  (cl:loop
     :with found := nil
     :for (k v) :on rest :by #'cl:cddr
     :do (cl:case k
           ((:offset)
            (cl:when (cl:member ':offset found)
              (cl:error "Multiple instances of :OFFSET in slot definition."))
            (cl:typecase v
              (cl:integer
               (cl:push ':offset found))
              (cl:t
               (cl:error "Expected integer for slot offset but got ~S." v))))
           (cl:otherwise
            (cl:error "Unknown slot option ~S." k))))
  rest)

(cl:defun parse-slot (slot)
  "Extract and validate the name, count, type, and rest form from `slot`."
  (cl:destructuring-bind (name count type . rest) slot
    (check-slot-name  name)
    (check-slot-count count)
    (check-slot-type  type)
    (check-slot-rest  rest)
    (cl:values name count type rest)))

(cl:defun parse-slots (slots)
  "Extract and validate the names, counts, types, foreign types, and rest forms from `slots`."
  (cl:loop
     :for slot :in slots
     :for (name count type rest) := (cl:multiple-value-list (parse-slot slot))
     :for ctype := (cl:handler-case (coalton-type-to-foreign-type type)
                     (cl:error ()
                       (cl:error "Unknown instance FOREIGNREPR ~S." type)))
     :collect name   :into names
     :collect count  :into counts
     :collect type   :into types
     :collect ctype  :into ctypes
     :collect rest   :into rests
     :finally (cl:return (cl:values names counts types ctypes rests))))

(cl:defun check-struct-name (name)
  "Ensure that `name` is a symbol."
  (cl:typecase name
    (cl:symbol
     name)
    (cl:t
     (cl:error "Expected symbol for struct name but got ~S." name))))

(cl:defun check-struct-conc-name (conc-name)
  "Ensure that `conc-name` is a string or a symbol."
  (cl:typecase conc-name
    ((cl:or cl:symbol cl:string)
     conc-name)
    (cl:t
     (cl:error "Expected symbol or string for conc-name but got ~S."
               conc-name))))

(cl:defun ensure-string (value)
  "Return `value` as a string."
  (cl:etypecase value
    (cl:string
     value)
    (cl:symbol
     (cl:symbol-name value))))

(cl:defun cl-nothingp (x)
  "Constantly `cl:nil`."
  (cl:declare (cl:ignore x))
  cl:nil)

(cl:deftype cl-nothing ()
  "A type that has no members."
  '(cl:satisfies cl-nothingp))

(cl:defmacro define-foreign-struct ((name conc-name) cl:&body slots)
  "Define a new foreign structure type called `name`.

`name` is a symbol that names the new Coalton type.

`conc-name` is a symbol or string that prefixes the slot names to yield to variable names used to specify the slots.

Each of `slots` is of the form `(name count type &key offset)`, where `name` is a symbol, `count` is a positive (non-zero) fixnum, `type` is a known Coalton type, and `offset` is an optional integer.

First, `cffi:defcstruct` is used to define a foreign structure using the Coalton type name prefixed with \"CL-\". The slot names are used directly.

Then, an uninstantiable Coalton type is defined with an instance of `ForeignRepr` using ``(:struct ,name)`.

Then, for each slot, a Coalton variable named using `conc-name` and the provided slot name is defined of type ``(Slot ,name ,slot-type)` that can be used to reference the value of the struct slot.

For example, the following are equivalent.

(define-foreign-struct (MyStruct \"my-struct.\")
  (my-slot 1 Int))

(cl:progn

  (cl:eval-when (:compile-toplevel :load-toplevel :execute)
    (cffi:defcstruct cl-mystruct
      (my-slot ':int :count 1)))

  (coalton-toplevel
    (repr :native cl-nothing)
    (define-type MyStruct))

  (define-foreign-repr-instance MyStruct (:struct cl-mystruct))

  (coalton-toplevel
    (define my-struct.my-slot
      (the (Slot MyStruct Int) (%Slot 0)))))

Continuing with the example above, the `MyStruct` struct can be created and manipulated as in the following.

> (coalton
    (let ((my-struct (foreign-alloc-uninitialized 1)))
      (slot-set! my-struct my-struct.my-slot 5)
      (let ret = (slot-ref my-struct my-struct.my-slot))
      (foreign-free my-struct)
      ret)
5

If a slot's count is greater than one, then use `slot-aref`, `slot-aset!`, and `slot-aptr`.

WARNING: Do not use types that allocate new memory when they are translated into foreign objects, as the consequential behavior is undefined. For example, do not define a struct with a slot of type `String`."
  (check-struct-name name)
  (check-struct-conc-name conc-name)

  (cl:setf conc-name (ensure-string conc-name))
  
  (cl:multiple-value-bind
        (slot-names slot-counts slot-types slot-ctypes slot-rests)
      (parse-slots slots)

    (cl:let ((cl-name (cl:intern (uiop:strcat "CL-" (cl:symbol-name name)))))

      `(cl:progn

         (cl:eval-when (:compile-toplevel :load-toplevel :execute)
           (cffi:defcstruct ,cl-name
             ,@(cl:loop
                  :for name  :in slot-names
                  :for count :in slot-counts
                  :for ctype :in slot-ctypes
                  :for rest  :in slot-rests
                  :collect (cl:list* name ctype ':count count rest))))

         (coalton-toplevel
           (repr :native cl-nothing)
           (define-type ,name))

         (define-foreign-repr-instance ,name (:struct ,cl-name))

         (coalton-toplevel
           ,@(cl:loop
                :for slot-name
                  :in slot-names
                :for slot-type
                  :in slot-types
                :for slot-var
                  := (cl:intern
                      (uiop:strcat conc-name (cl:symbol-name slot-name)))
                :collect
                `(define ,slot-var
                   (the (Slot ,name ,slot-type)
                        (%Slot (lisp Offset ()
                                 (cffi:foreign-slot-offset
                                  '(:struct ,cl-name)
                                  ',slot-name)))))))))))
