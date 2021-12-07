(in-package #:coalton-impl/algorithm)


;;
;; Immutable Map
;; Wrapper around fset:map
;;

(defstruct immutable-map
  (data (fset:empty-map) :type fset:map :read-only t))

(defmethod make-load-form ((self immutable-map) &optional env)
  (make-load-form-saving-slots
   self
   :slot-names '(data)
   :environment env))

(defun immutable-map-lookup (m key)
  "Lookup KEY in M"
  (declare (type immutable-map m)
           (type symbol key)
           (values t))
  (fset:lookup (immutable-map-data m) key))

(defun immutable-map-set (m key value &optional (constructor #'make-immutable-map))
  "Set KEY to VALUE in M"
  (declare (type immutable-map m)
           (type symbol key))
  (funcall constructor :data (fset:with (immutable-map-data m) key value)))

(defun immutable-map-set-multiple (m items &optional (constructor #'make-immutable-map))
  "Create an IMMUTABLE-MAP from M with ITEMS set as bindings"
  (declare (type immutable-map m)
           (type list items)
           (values immutable-map &optional))
  (let ((map (immutable-map-data m)))
    (loop :for (key . value) :in items
          :do (setf map (fset:with map key value)))
    (funcall constructor :data map)))

(defun immutable-map-keys (m)
  (declare (type immutable-map m)
           (values coalton-impl/ast::symbol-list &optional))
  "Return a unique list containg the keys in M and its parents"
  (declare (type immutable-map m))
  (fset:convert 'list (fset:domain (immutable-map-data m))))

(defun immutable-map-diff (m1 m2 &optional (constructor #'make-immutable-map))
  "Returns the elements that appear in M1 but not M2"
  (declare (type immutable-map m1 m2)
           (values immutable-map &optional))
  (funcall constructor :data (fset:map-difference-2 (immutable-map-data m1)
                                                    (immutable-map-data m2))))

(defun immutable-map-remove (m key &optional (constructor #'make-immutable-map))
  (declare (type immutable-map m)
           (type symbol key)
           (values immutable-map &optional))
  (funcall constructor :data (fset:less (immutable-map-data m) key)))
