(cl:defpackage #:coalton-library/utils
  (:use #:coalton)
  (:export #:defstdlib-package #:generate-unary-wrapper))

(cl:in-package #:coalton-library/utils)

(cl:defmacro defstdlib-package (name cl:&rest args)
  `(cl:eval-when (:compile-toplevel :load-toplevel)
     #+sb-package-locks
     (cl:when (cl:find-package ',name)
       (sb-ext:unlock-package ',name))
     (cl:defpackage ,name ,@args)
     #+sb-package-locks
     (sb-ext:lock-package ',name)))

(cl:defun generate-unary-wrapper
    (return-type coalton-fun cl-fun cl:&key
     domain)
  "Returns the definition of a coalton function which is a unary wrapper around a corresponding CL function. Takes an optional domain, which is a symbol evaluating to a function, which returns true iff if a value is in the domain"
  `(define (,coalton-fun x)
     (lisp ,return-type (x)
       ,(cl:when domain
          `(cl:unless (cl:funcall ,domain x) 
             (cl:error "~a is not in the domain of ~a for ~a"
                       x ',coalton-fun ',return-type)))
       (,cl-fun x))))
