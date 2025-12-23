(coalton-cffi/utils:define-cffi-package #:coalton-cffi/libraries
  (:use
   #:coalton
   #:coalton-prelude)
  (:export
   #:ForeignLibrary
   #:load-foreign-library
   #:unload-foreign-library
   #:define-foreign-library))

(in-package #:coalton-cffi/libraries)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :native cl:symbol)
  (define-type ForeignLibrary)

  (inline)
  (declare load-foreign-library (ForeignLibrary -> Unit))
  (define (load-foreign-library foreign-library)
    "Load `foreign-library`."
    (lisp Unit (foreign-library)
      (cffi:load-foreign-library foreign-library)
      Unit))

  (inline)
  (declare unload-foreign-library (ForeignLibrary -> Unit))
  (define (unload-foreign-library foreign-library)
    "Unload `foreign-library`."
    (lisp Unit (foreign-library)
      (cffi:close-foreign-library foreign-library)
      Unit)))

(cl:defmacro define-foreign-library (name cl:&body load-clauses)
  "Define a `ForeignLibrary` called `name` using `load-clauses`.

This is a wrapper for `cffi:define-foreign-library`.

Each of `load-clauses` is a list of the form `(feature designator)`.

`feature` is `cl:t` or a keyword that may or may not be in `cl:*features*`, or a list that begins with `:or`, `:and`, or `:not`, followed by more features. For example, `(:or :a (:and :b (:not :c)))`.

`designator` is a path (`cl:string` or `cl:pathname`) to a library or a list that begins with `:framework`, `:or`, or `:default`. If the list begins with `:framework`, CFFI expects a path to a Darwin framework. If the list begins with `:or`, the remaining elements are more designators that are searched for sequentially. If the list begins with `:default`, then CFFI chooses the suffix (`.so`, `.dylib`, etc.) for the system.

For each load clause, the feature expression is evaluated. If it evaluates to `cl:t`, then the designator associated with that feature is used to search for the library.

Please, see the documentation for `cffi:define-foreign-library` and `cffi:load-foreign-library` for more information on the syntax of `load-clauses`."
  `(define ,name
     (lisp ForeignLibrary ()
       (cffi:define-foreign-library ,name
         ,@load-clauses))))
