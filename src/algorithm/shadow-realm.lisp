(in-package #:coalton-impl/algorithm)


;;
;; Shadow Realm
;; Wrapper around fset:map
;;

(serapeum:defstruct-read-only shadow-realm
  (data (fset:empty-map) :type fset:map))

(defun shadow-realm-lookup (sr key)
  "Lookup KEY in SR"
  (declare (type shadow-realm sr)
           (type symbol key)
           (values t))
  (fset:lookup (shadow-realm-data sr) key))

(defun shadow-realm-set (sr key value &optional (constructor #'make-shadow-realm))
  "Set KEY to VALUE in SR"
  (declare (type shadow-realm sr)
           (type symbol key))
  (funcall constructor :data (fset:with (shadow-realm-data sr) key value)))

(defun shadow-realm-push-frame (sr env &optional (constructor #'make-shadow-realm))
  "Create a SHADOW-REALM from sr with ENV set as bindings"
  (declare (type shadow-realm sr)
           (type list env)
           (values shadow-realm &optional))
  (let ((map (shadow-realm-data sr)))
    (loop :for (key . value) :in env
          :do (setf map (fset:with map key value)))
    (funcall constructor :data map)))

(defun shadow-realm-keys (sr)
  (declare (type shadow-realm sr)
           (values coalton-impl/ast::symbol-list &optional))
  "Return a unique list containg the keys in SR and its parents"
  (declare (type shadow-realm sr))
  (fset:convert 'list (fset:domain (shadow-realm-data sr))))

(defun shadow-realm-diff (sr1 sr2 &optional (constructor #'make-shadow-realm))
  "Returns the elements that appear in SR1 but not SR2"
  (declare (type shadow-realm sr1 sr2)
           (values shadow-realm &optional))
  (funcall constructor :data (fset:map-difference-2 (shadow-realm-data sr1) (shadow-realm-data sr2))))

(defun shadow-realm-remove (sr key &optional (constructor #'make-shadow-realm))
  (declare (type shadow-realm sr)
           (type symbol key)
           (values shadow-realm &optional))
  (funcall constructor :data (fset:less (shadow-realm-data sr) key)))
