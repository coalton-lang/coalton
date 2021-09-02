(in-package #:coalton-impl)

;;; Handling of toplevel COALTON:DEFINE-TYPE.

;;
;; (DEFINE-TYPE (TUPLE :A :B)
;;     (TUPLE :A :B))
;; (DEFINE-TYPE (EITHER :A :B)
;;     (LEFT :A)
;;     (RIGHT :B))
;; (DEFINE-TYPE (MAYBE :A)
;;     NOTHING
;;     (JUST :A))
;; (DEFINE-TYPE UNIT
;;     UNIT)
;;

(defun process-toplevel-type-definitions (deftype-forms env)
  (declare (type list deftype-forms)
	   (type environment env))
  ;; Create a naive set of tcons to allow mutual recursion. If these
  ;; are incorrect it should error.
  (declare (type environment env)
	   (values type-definition-list))

  (multiple-value-bind (parsed-deftypes parsed-docstrings)
      (coalton-impl/typechecker::parse-type-definitions deftype-forms env)
    (dolist (parsed-deftype parsed-deftypes)
      (let* ((type-name (type-definition-name parsed-deftype))
             (tycon (type-definition-type parsed-deftype))
             (ctors (type-definition-constructors parsed-deftype)))

        ;; Add the type to the type environment
        (setf env (set-type env type-name tycon))

        (dolist (ctor ctors)
          ;; Add the constructors to the constructor environment
          (setf env (set-constructor env (constructor-entry-name ctor) ctor))

	  ;; Add the constructor as a value to the value environment
          (setf env (set-value-type env (constructor-entry-name ctor) (constructor-entry-scheme ctor)))

	  ;; Register the constructor in the name environment
	  (setf env (set-name env (constructor-entry-name ctor)
			      (make-name-entry :name (constructor-entry-name ctor)
					       :type :constructor
                                               :docstring nil
                                               :location (or *compile-file-pathname* *load-truename*))))

	  ;; If the constructor takes paramaters then add it to the function environment
	  (if (not (= (constructor-entry-arity ctor) 0))
	      (setf env
		    (set-function
		     env
		     (constructor-entry-name ctor)
		     (make-function-env-entry
		      :name (constructor-entry-name ctor)
		      :arity (constructor-entry-arity ctor))))
	      (setf env (unset-function env (constructor-entry-name ctor)))))))
    (values parsed-deftypes env parsed-docstrings)))
