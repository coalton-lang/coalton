(coalton-library/utils:defstdlib-package #:coalton-library/show
  (:documentation "A way to show linear, human-readable, textual representations of objects.")
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:local-nicknames
   (#:types #:coalton-library/types)
   (#:utils #:coalton-library/utils))
  (:export
   #:make-string-builder

   #:ShowType
   #:show-type-to
   #:show-inferred-type-as-string
   #:show-inferred-type-as-readable-string

   #:Show
   #:show-to
   #:show-as-string
   #:show
   #:show*

   #:Expose
   ))

(in-package #:coalton-library/show)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(cl:defvar *show-stream* (cl:make-synonym-stream 'cl:*standard-output*)
  "The stream to which `show` will show to by default.")

(coalton-toplevel

  ;; For internal purposes only.
  (repr :native cl:stream)
  (define-type %OutputStream)

  (declare make-string-builder (Unit -> (Tuple (String -> Unit) (Unit -> String))))
  (define (make-string-builder)
    "Make a string builder and extractor. Return a tuple of two values:

1. A procedure which consumes strings (accumulating state underneath).

2. A thunk which extracts (and clears) the strings consumed."
    (let ((declare stream %OutputStream)
          (stream (lisp %OutputStream ()
                    (cl:make-string-output-stream)))

          (declare consume (String -> Unit))
          (consume (fn (s)
                     (lisp Unit (s stream)
                       (cl:write-string s stream)
                       Unit)))

          (declare extract (Unit -> String))
          (extract (fn ()
                     (lisp String (stream)
                       (cl:get-output-stream-string stream)))))
      (Tuple consume extract)))

  (inline)
  (declare write (String -> Unit))
  (define (write s)
    "Write the string `s` to the `Show` stream. Suitable for use with the
`Show` type class."
    (lisp Unit (s)
      (cl:write-string s *show-stream*)
      Unit)))

(coalton-toplevel
  (define-class (ShowType :a)
    (show-type-to
     "Execute a callback on a string representation of the Coalton type
captured by the proxy. The callback may be called multiple times on
different sections of the string, and the callback is guaranteed to be
called on sections left-to-right. For example, for a callback `f`, the
type `(Optional U64)` may be sequenced as

```
(f \"(Opt\")
(f \"ional \")
(f \"U64)\")

Set the readable boolean to `True` to emit strings whose concatenation
can be read by `CL:READ`."
     (Boolean -> (String -> Unit) -> types:Proxy :a -> Unit)))

  (declare show-inferred-type-as-string (ShowType :a => :a -> String))
  (define (show-inferred-type-as-string x)
    "Display `x`'s statically inferred type as a string."
    (match (make-string-builder)
      ((Tuple consume extract)
       (show-type-to False consume (types:proxy-of x))
       (extract))))

  (declare show-inferred-type-as-readable-string (ShowType :a => :a -> String))
  (define (show-inferred-type-as-readable-string x)
    "Display `x`'s statically inferred type as a readable (i.e., as if by
`CL:READ`) string."
    (match (make-string-builder)
      ((Tuple consume extract)
       (show-type-to True consume (types:proxy-of x))
       (extract))))
  
  (define-class (Show :a)
    "Objects which have a convenient, textual, linear printed representation for
display in a terminal. This is principally for program output and debugging."
    (show-to
     "Execute a callback on a debugging string representation of an
object. The callback may be called multiple times on different
sections of the string, and the callback is guaranteed to be called on
sections left-to-right.

For example, for a callback function `f`, an object like `(1 2 3)` may
be executed as

```
(f \"(\")
(f \"1 2\")
(f \" \")
(f \"3)\")
```"
     ((String -> Unit) -> :a -> Unit)))

  (declare show-as-string (Show :a => :a -> String))
  (define (show-as-string x)
    "Display `x` as a string.

This is not necessarily identical to `(the String (into x))`."
    (match (make-string-builder)
      ((Tuple consume extract)
       (show-to consume x)
       (extract))))

  (declare show (Show :a => :a -> Unit))
  (define (show x)
    "Display `x` to the \"show stream\", which is by
default `(standard-output)`, but can be overridden with
`with-show-stream`. The stream will be flushed immediately."
    (show-to write x)
    (lisp Unit ()
      (cl:finish-output *show-stream*)
      Unit)))

(defmacro show* (cl:&rest items)
  "Show each of the items `items` sequentially."
  `(progn
     ,@(cl:loop :for item :in items :collect `(show ,item))))

(coalton-toplevel
  (repr :transparent)
  (define-type (Expose :a)
    "A transparent wrapper type to force any object to be able to be shown
according to whatever Lisp thinks. Use

    (show (Expose x))

to print any object `x` as if by `prin1`."
    (Expose :a)))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro define-show-instance (type)
    `(coalton-toplevel
       (define-instance (ShowType ,(cl:read-from-string type))
         (define (show-type-to readable? f _)
           (f (utils:sym readable? ,type))))
       
       (define-instance (Show ,(cl:read-from-string type))
         (define (show-to f x)
           (f (lisp String (x)
                (cl:with-standard-io-syntax
                  (cl:prin1-to-string x)))))))))

(coalton-toplevel
  (define-instance (ShowType Boolean)
    (define (show-type-to readable? f _)
      (f (utils:sym readable? "Boolean"))))

  (define-instance (Show Boolean)
    (define (show-to f x)
      (f (if x "True" "False"))))

  (define-instance (ShowType :a => ShowType (Optional :a))
    (define (show-type-to readable? f p)
      (f "(")
      (f (utils:sym readable? "Optional"))
      (f " ")
      (show-type-to readable? f (types:proxy-inner p))
      (f ")")))

  (define-instance (Show :a => Show (Optional :a))
    (define (show-to f x)
      (match x
        ((Some y)
         (f "(Some ")
         (show-to f y)
         (f ")"))
        ((None)
         (f "None")))))
  
  (define-instance (ShowType :a => ShowType (List :a))
    (define (show-type-to readable? f p)
      (f "(")
      (f (utils:sym readable? "List"))
      (f " ")
      (show-type-to readable? f (types:proxy-inner p))
      (f ")")))

  (define-instance (Show :a => Show (List :a))
    (define (show-to f x)
      (match x
        ((Nil)
         (f "()"))
        ((Cons y ys)
         (f "(")
         (show-to f y)
         (rec % ((items ys))
           (match items
             ((Nil) (f ")"))
             ((Cons z zs)
              (show-to f z)
              (% zs))))))))

  (define-instance ((ShowType :a) (ShowType :b) => ShowType (:a -> :b))
    (define (show-type-to readable? f p)
      (f "(")
      (f (utils:sym readable? "Arrow"))
      (f " ")
      (show-type-to readable? f (types:proxy-function-from p))
      (f " ")
      (show-type-to readable? f (types:proxy-function-to p))
      (f ")")))


  (define-instance (ShowType :a => ShowType (types:Proxy :a))
    (define (show-type-to readable? f p)
      (f "(")
      (f (utils:sym readable? "Proxy"))
      (f " ")
      (show-type-to readable? f (types:proxy-inner p))
      (f ")")))

  (define-instance (Show (types:Proxy :a))
    (define (show-to f _)
      (f "Proxy")))

  (define-instance (ShowType types:LispType)
    (define (show-type-to readable? f p)
      (f (utils:sym readable? "LispType"))))

  (define-instance (Show types:LispType)
    (define (show-to f x)
      (f (lisp String (x)
           (cl:prin1-to-string x)))))
  
  (define-instance (ShowType (Expose :a))
    (define (show-type-to readable? f _)
      (f (utils:sym readable? "Expose"))))

  (define-instance (Show (Expose :a))
    (define (show-to f x)
      (f (lisp String (x)
           (cl:with-standard-io-syntax
             (cl:prin1-to-string x)))))))

(define-show-instance "Integer")
(define-show-instance "UFix")
(define-show-instance "Bit")
(define-show-instance "U8")
(define-show-instance "U16")
(define-show-instance "U32")
(define-show-instance "U64")

(define-show-instance "IFix")
(define-show-instance "I8")
(define-show-instance "I16")
(define-show-instance "I32")
(define-show-instance "I64")

(define-show-instance "F32")
(define-show-instance "F64")

(define-show-instance "Fraction")

(define-show-instance "String")
(define-show-instance "Char")

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/SHOW")
