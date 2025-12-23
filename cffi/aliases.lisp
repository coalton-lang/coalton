(coalton-cffi/utils:define-cffi-package #:coalton-cffi/aliases
  (:use
   #:coalton
   #:coalton-prelude)
  (:export
   #:define-foreign-integer-alias
   #:ForeignChar
   #:UnsignedForeignChar
   #:Short
   #:UnsignedShort
   #:Int
   #:UnsignedInt
   #:Long
   #:UnsignedLong
   #:LongLong
   #:UnsignedLongLong
   #:Float
   #:Double
   #:Bool
   #:IntPtr
   #:UIntPtr
   #:Size
   #:SSize
   #:PtrDiff
   #:Offset))

(in-package #:coalton-cffi/aliases)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro define-foreign-integer-alias (name ctype signed-or-unsigned)
  "Define a type alias called `name` for the foreign integer type `ctype`.

`ctype` must be a type known to the CFFI system, such as a built-in type, like `:int`, or a type defined using the groveller.

`signed-or-unsigned` must be either `:signed` or `:unsigned`."
  (cl:declare (cl:type (cl:member :signed :unsigned) signed-or-unsigned))
  `(coalton-toplevel
     (define-type-alias ,name
       ,(cl:let* ((size
                    (cffi:foreign-type-size ctype))
                  (signed-size
                    (cl:ecase signed-or-unsigned
                      ((:signed) (cl:- size))
                      ((:unsigned) size))))
          (cl:case signed-size
            ((-8) 'I64)
            ((-4) 'I32)
            ((-2) 'I16)
            ((-1)  'I8)
            (( 1)  'U8)
            (( 2) 'U16)
            (( 4) 'U32)
            (( 8) 'U64)
            (cl:otherwise
             (cl:error "Unable to define alias for foreign type of size ~D."
                       size)))))))

;;; Here, we define a collection of type aliases to represent the
;;; available primitive foreign types.

(define-foreign-integer-alias ForeignChar         :char               :signed)
(define-foreign-integer-alias UnsignedForeignChar :unsigned-char      :unsigned)
(define-foreign-integer-alias Short               :short              :signed)
(define-foreign-integer-alias UnsignedShort       :unsigned-short     :unsigned)
(define-foreign-integer-alias Int                 :int                :signed)
(define-foreign-integer-alias UnsignedInt         :unsigned-int       :unsigned)
(define-foreign-integer-alias Long                :long               :signed)
(define-foreign-integer-alias UnsignedLong        :unsigned-long      :unsigned)
(define-foreign-integer-alias LongLong            :long-long          :signed)
(define-foreign-integer-alias UnsignedLongLong    :unsigned-long-long :unsigned)

(coalton-toplevel
  (define-type-alias Float  F32)
  (define-type-alias Double F64))

(define-foreign-integer-alias Bool    :bool    :signed)
(define-foreign-integer-alias IntPtr  :intptr  :signed)
(define-foreign-integer-alias UIntPtr :uintptr :unsigned)
(define-foreign-integer-alias Size    :size    :unsigned)
(define-foreign-integer-alias SSize   :ssize   :signed)
(define-foreign-integer-alias PtrDiff :ptrdiff :signed)
(define-foreign-integer-alias Offset  :offset  :signed)
