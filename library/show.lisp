(coalton-library/utils:defstdlib-package #:coalton-library/show
  (:documentation "A way to show linear, human-readable, textual representations of objects.")
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/classes)
  (:export
   #:make-string-builder

   #:Show
   #:show-to
   #:show-to-string
   #:show
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
     ((String -> Unit) -> :a -> Unit))
    #+ig
    (show-type-to
     "Display the Lisp type of `:a` to a stream. Set the readable boolean to
`True` to print package prefixes."
     ((String -> Unit) -> Boolean -> Proxy :a -> Unit)))

  (declare show-to-string (Show :a => :a -> String))
  (define (show-to-string x)
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
       (define (show-to f x)
         (f (lisp String (x)
              (cl:with-standard-io-syntax
                (cl:princ-to-string x))))))))

(coalton-toplevel
  (define-instance (Show Boolean)
    (define (show-to f x)
      (f (if x "True" "False"))))

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

  (define-instance (Show (Reveal :a))
    (define (show-to f x)
      (f (lisp String (x)
           (cl:with-standard-io-syntax
             (cl:princ-to-string x))))))

  #+ig
  (define-instance (coalton-library/types:RuntimeRepr :a => (Show (Expose :a)))
    (define (show-to f x)
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
