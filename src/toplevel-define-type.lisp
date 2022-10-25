(in-package #:coalton-impl)

;;;;
;;;; Handling of toplevel COALTON:DEFINE-TYPE.
;;;;

(defun process-toplevel-type-definitions (deftype-forms repr-table env)
  "Returns a list of TYPE-DEFINITIONs, a new ENVIRONMENT, and a list of INSTANCE-DEFINITIONs for Addressable instances."
  (declare (type list deftype-forms)
           (type tc:environment env)
           (values tc:type-definition-list tc:environment list &optional))

  ;; Parse type definitions into a list of TYPE-DEFINITION objects
  (multiple-value-bind (parsed-deftypes env)
      (tc:parse-type-definitions deftype-forms repr-table env)

    (values
     parsed-deftypes
     env
     (loop :for parsed-deftype :in parsed-deftypes
           :append (maybe-auto-addressable-instance parsed-deftype)
           :append (maybe-auto-runtime-repr-instance parsed-deftype)))))

;;;
;;; Compiler written instances
;;;

(defun make-auto-addressable-instance (type-def)
  (declare (type tc:type-definition type-def)
           (values list &optional))
  (let* ((name (tc:type-definition-name type-def))
         (tvars (loop :for i :below (tc:kind-arity (tc:tycon-kind (tc:type-definition-type type-def)))
                      :collect (alexandria:format-symbol :keyword "~d" i)))
         (full-type (if tvars
                        `(,name ,@tvars)
                        name))
         (addressable-class
           (alexandria:ensure-symbol "ADDRESSABLE" (find-package "COALTON-LIBRARY/CLASSES")))

         (eq?
           (alexandria:ensure-symbol "EQ?" (find-package "COALTON-LIBRARY/CLASSES"))))
    `((coalton:define-instance (,addressable-class ,full-type)
         (coalton:define (,eq? a b)
           (coalton:lisp coalton:Boolean (a b)
             (eq a b)))))))

(defun maybe-auto-addressable-instance (type-def)
  (declare (type tc:type-definition type-def)
           (values list &optional))
  (when (and (not (equalp (symbol-package (tc:type-definition-name type-def))
                          (find-package "COALTON-LIBRARY/TYPES")))
             (tc:explicit-repr-auto-addressable-p (tc:type-definition-explicit-repr type-def)))
    (make-auto-addressable-instance type-def)))

(defun make-auto-runtime-repr-instance (type-def)
  (declare (type tc:type-definition type-def)
           (values list &optional))
  (let* ((name (tc:type-definition-name type-def))
         (tvars (loop :for i :below (tc:kind-arity (tc:tycon-kind (tc:type-definition-type type-def)))
                      :collect (alexandria:format-symbol :keyword "~d" i)))
         (full-type (if tvars
                        `(,name ,@tvars)
                        name))
         (addressable-class
           (alexandria:ensure-symbol "RUNTIMEREPR" (find-package "COALTON-LIBRARY/TYPES")))

         (runtime-repr
           (alexandria:ensure-symbol "RUNTIME-REPR" (find-package "COALTON-LIBRARY/TYPES")))

         (lisp-type
           (alexandria:ensure-symbol "LISPTYPE" (find-package "COALTON-LIBRARY/TYPES"))))
    `((coalton:define-instance (,addressable-class ,full-type)
         (coalton:define (,runtime-repr p)
           (coalton:lisp ,lisp-type ()
             ',(tc:type-definition-runtime-type type-def)))))))

(defun maybe-auto-runtime-repr-instance (type-def)
  (declare (type tc:type-definition type-def)
           (values list &optional))
  (unless (equalp (symbol-package (tc:type-definition-name type-def))
                  (find-package "COALTON-LIBRARY/TYPES"))
    (make-auto-runtime-repr-instance type-def)))
