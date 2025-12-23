(coalton-cffi/utils:define-cffi-package #:coalton-cffi/complex
  (:use
   #:cl
   #:cffi)
  (:export
   #:cl-complex-float
   #:cl-complex-double))

(in-package #:coalton-cffi/complex)

;;; This package provides CFFI support for complex numbers by defining
;;; the necessary generic method from the CFFI package to translate
;;; native complex numbers into sequential floats or doubles in
;;; foreign memory.

(defmacro define-foreign-complex-number-type (name element-type c-element-type)
  (let ((value   (gensym "VALUE"))
        (type    (gensym "TYPE"))
        (ptr     (gensym "PTR"))
        (pointer (gensym "POINTER"))
        (param   (gensym "PARAM"))
        (var     (gensym "VAR"))
        (body    (gensym "BODY"))
        (ctype   (gensym "CTYPE")))
    `(progn

       (define-foreign-type ,name (cffi::enhanced-foreign-type) ()
         (:actual-type :array ,c-element-type 2))

       (define-parse-method ,name ()
         (make-instance ',name))

       (defmethod translate-to-foreign
           (,value (,type ,name))
         (declare (type (complex ,element-type) ,value))
         (let ((,pointer (foreign-alloc '(:array ,c-element-type 2))))
           (setf (mem-aref ,pointer ',c-element-type 0) (realpart ,value)
                 (mem-aref ,pointer ',c-element-type 1) (imagpart ,value))
           ,pointer))

       (defmethod translate-into-foreign-memory
           (,value (,type ,name) ,pointer)
         (declare (type (complex ,element-type) ,value))
         (setf (mem-aref ,pointer ',c-element-type 0) (realpart ,value)
               (mem-aref ,pointer ',c-element-type 1) (imagpart ,value)))

       (defmethod cffi::translate-aggregate-to-foreign
           (,ptr ,value (,type ,name))
         (setf (mem-aref ,ptr ',c-element-type 0) (realpart ,value)
               (mem-aref ,ptr ',c-element-type 1) (imagpart ,value)))

       (defmethod translate-from-foreign
           (,ptr (,type ,name))
         (declare (values (complex ,element-type) &optional))
         (complex (mem-aref ,ptr ',c-element-type 0)
                  (mem-aref ,ptr ',c-element-type 1)))

       (defmethod free-translated-object
           (,ptr (,type ,name) ,param)
         (declare (ignore ,param))
         (foreign-array-free ,ptr))

       (defmethod expand-to-foreign
           (,value (,type ,name))
         (let ((,ctype ,c-element-type)
               (,pointer (gensym "POINTER")))
           `(let ((,,pointer (foreign-alloc '(:array ,,ctype 2))))
              (setf (mem-aref ,,pointer ',,ctype 0) (realpart ,,value)
                    (mem-aref ,,pointer ',,ctype 1) (imagpart ,,value))
              ,,pointer)))

       (defmethod expand-from-foreign
           (,ptr (,type ,name))
         (let ((,ctype ',c-element-type))
           `(complex (mem-aref ,,ptr ,,ctype 0)
                     (mem-aref ,,ptr ,,ctype 1))))

       (defmethod expand-into-foreign-memory
           (,value (,type ,name) ,ptr)
         (let ((,ctype ',c-element-type))
           `(setf (mem-aref ,,ptr ,,ctype 0) (realpart ,,value)
                  (mem-aref ,,ptr ,,ctype 1) (imagpart ,,value))))

       (defmethod expand-to-foreign-dyn
           (,value ,var ,body (,type ,name))
         `(cffi:with-foreign-object (,,var (:array :float 2))
            (cl:setf (cffi:mem-aref ,,var ':float 0) (cl:realpart ,,value)
                     (cffi:mem-aref ,,var ':float 1) (cl:imagpart ,,value))
            ,@,body)))))

(define-foreign-complex-number-type cl-complex-float  single-float :float)
(define-foreign-complex-number-type cl-complex-double double-float :double)
