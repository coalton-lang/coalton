(coalton-library/utils:defstdlib-package #:coalton-library/show
  (:documentation "A way to show linear, human-readable, textual representations of objects.")
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/classes)
  (:export
   #:LispOutputStream
   #:standard-output
   #:error-output
   #:finish-output
   #:make-string-output
   #:with-show-stream

   #:Show
   #:show-to
   #:show-to-string
   #:newline
   #:show*

   #:Reveal
   #:Expose
   ))

(in-package #:coalton-library/show)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(cl:defvar *show-stream* (cl:make-synonym-stream 'cl:*standard-output*)
  "The stream to which `show` will show to by default.")

(coalton-toplevel
  (repr :native cl:stream)
  (define-type LispOutputStream
    "A Lisp output stream.")

  (inline)
  (declare standard-output (Unit -> LispOutputStream))
  (define (standard-output)
    "Lisp's `*standard-output*`. This value is dynamic."
    (lisp LispOutputStream ()
      cl:*standard-output*))

  (inline)
  (declare error-output (Unit -> LispOutputStream))
  (define (error-output)
    "Lisp's `*error-output*`. This value is dynamic."
    (lisp LispOutputStream ()
      cl:*error-output*))

  (declare finish-output (LispOutputStream -> Unit))
  (define (finish-output s)
    "Finish all output on the stream `s`."
    (lisp Unit (s)
      (cl:finish-output s)
      Unit))

  (declare make-string-output (Unit -> (Tuple LispOutputStream (Unit -> String))))
  (define (make-string-output)
    "Make a string output stream. Return a tuple of two values:

1. The `LispOutputStream` that can be written to.

2. A thunk which extracts (and clears) the characters written to the stream."
    (let ((stream (lisp LispOutputStream ()
                    (cl:make-string-output-stream)))
          (extract (fn ()
                     (lisp String (stream)
                       (cl:get-output-stream-string stream)))))
      (Tuple stream extract)))

  (inline)
  (declare show-stream (Unit -> LispOutputStream))
  (define (show-stream)
    (lisp LispOutputStream ()
      *show-stream*))

  (declare with-show-stream (LispOutputStream -> (Unit -> :a) -> :a))
  (define (with-show-stream s f)
    "Call the thunk `f`, overriding as `s` the stream as that which `show`
will write to."
    (lisp :a (s f)
      (cl:let ((*show-stream* s))
        (call-coalton-function f Unit)))))

(coalton-toplevel
  (define-class (Show :a)
    "Objects which have a convenient, textual, linear printed representation for
display in a terminal. This is principally for program output and debugging."
    (show-to
     "Display an object to a Lisp stream."
     (LispOutputStream -> :a -> Unit)))

  (declare show-to-string (Show :a => :a -> String))
  (define (show-to-string x)
    "Display `x` as a string.

This is not necessarily identical to `(the String (into x))`."
    (match (make-string-output)
      ((Tuple stream extract)
       (show-to stream x)
       (extract))))

  (declare show (Show :a => :a -> Unit))
  (define (show x)
    "Display `x` to the \"show stream\", which is by
default `(standard-output)`, but can be overridden with
`with-show-stream`. The stream will be flushed immediately."
    (let ((stream (show-stream)))
      (show-to stream x)
      (finish-output stream)))

  (declare newline (Unit -> Unit))
  (define (newline)
    "Show a newline (as is by `show`)."
    (let ((stream (show-stream)))
      (lisp Unit (stream)
        (cl:terpri stream)
        (cl:finish-output stream)
        Unit))))

(defmacro show* (cl:&rest items)
  "Show each of the items `items` sequentially."
  `(progn
     ,@(cl:loop :for item :in items :collect `(show ,item))))

(coalton-toplevel
  (repr :transparent)
  (define-type (Reveal :a)
    "A transparent wrapper type to force any object to be able to be shown
according to whatever Lisp thinks. Use

    (show (Reveal x))

to print any object `x` as if by `princ`."
    (Reveal :a))

  (repr :transparent)
  (define-type (Expose :a)
    (Expose :a)))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-show-instance (type)
    `(define-instance (Show ,type)
       (define (show-to s x)
         (lisp Unit (s x)
           (cl:with-standard-io-syntax
             (cl:princ x s))
           Unit)))))

(coalton-toplevel
  (define-instance (Show Boolean)
    (define (show-to s x)
      (show-to s (if x "True" "False"))))

  (define-instance (Show :a => Show (List :a))
    (define (show-to s x)
      (match x
        ((Nil) (show-to s "()"))
        ((Cons y ys)
         (show-to s #\()
         (show-to s y)
         (rec % ((items ys))
           (match items
             ((Nil) (show-to s #\)))
             ((Cons z zs)
              (show-to s z)
              (% zs))))))))

  (define-instance (Show (Reveal :a))
    (define (show-to s x)
      (lisp Unit (s x)
        (cl:with-standard-io-syntax
          (cl:princ x s))
        Unit)))

  (define-instance (coalton-library/types:RuntimeRepr :a => (Show (Expose :a)))
    (define (show-to s x)
      (let ((ctype (coalton-library/types:coalton-type-string
                    (coalton-library/types:proxy-inner
                     (coalton-library/types:proxy-of x)))))
        (lisp Unit (s x ctype)
          (cl:with-standard-io-syntax
            (cl:format s "[~A] ~A" ctype x))
          Unit))))
  
  (define-show-instance Integer)
  (define-show-instance UFix)
  (define-show-instance Bit)
  (define-show-instance U8)
  (define-show-instance U16)
  (define-show-instance U32)
  (define-show-instance U64)

  (define-show-instance IFix)
  (define-show-instance I8)
  (define-show-instance I16)
  (define-show-instance I32)
  (define-show-instance I64)

  (define-show-instance F32)
  (define-show-instance F64)

  (define-show-instance Fraction)

  (define-show-instance String)
  (define-show-instance Char))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/SHOW")
