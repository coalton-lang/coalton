(in-package #:coalton-impl/algorithm)

;;
;; Shadow List
;; Wrapper around fset:map<fset:seq>
;;

(serapeum:defstruct-read-only shadow-list
  (data (fset:empty-map (fset:empty-seq)) :type fset:map))

(defun shadow-list-lookup (sl key &key no-error)
  "Lookup key in SL"
  (declare (type shadow-list sl)
           (type symbol key)
           (type boolean no-error)
           (values fset:seq))
  (multiple-value-bind (value present-p)
      (fset:lookup (shadow-list-data sl) key)
    (unless (or present-p no-error)
      (error "Undefined key ~a" key))
    value))

(defun shadow-list-push (sl key value &optional (constructor #'make-shadow-list))
  "Push value to the list at KEY in SL"
  (declare (type shadow-list sl)
           (type symbol key)
           (values shadow-list &optional))

  (let ((map (shadow-list-data sl)))
    (funcall constructor :data (fset:with map key (fset:with-first (fset:lookup map key) value)))))

(defun shadow-list-replace (sl key index value &optional (constructor #'make-shadow-list))
  "Replace value at INDEX with VALUE in the map at KEY in SL."
  (declare (type shadow-list sl)
           (type symbol key)
           (type fixnum index)
           (values shadow-list &optional))
  (let ((map (shadow-list-data sl)))
    (funcall constructor :data (fset:with map key (fset:with (fset:lookup map key) index value)))))

(defun shadow-list-diff (sl1 sl2 &optional (constructor #'make-shadow-list))
  "Return a new SHADOW-LIST with elements in S1 but not S2."
  (let ((diff-map (fset:empty-map)))
    (fset:do-map (k v1 (shadow-list-data sl1))
      (multiple-value-bind (v2 found)
          (fset:lookup (shadow-list-data sl2) k)
        (if found
            ;; If both have this key then diff the seq
            (let ((seq-diff (fset:convert 'fset:seq
                                     (fset:set-difference (fset:range v1)
                                                          (fset:range v2)))))
              (setf diff-map (fset:with diff-map k seq-diff)))
            ;; Otherwise save the whole seq
            (setf diff-map (fset:with diff-map k v1)))))
    (funcall constructor :data diff-map)))
