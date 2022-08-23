(defpackage #:coalton-impl/algorithm/immutable-listmap
  (:use #:cl)
  (:export
   #:immutable-listmap                  ; STRUCT
   #:make-immutable-listmap             ; CONSTRUCTOR
   #:immutable-listmap-data             ; ACCESSOR
   #:immutable-listmap-lookup           ; FUNCTION
   #:immutable-listmap-push             ; FUNCTION
   #:immutable-listmap-replace          ; FUNCTION
   #:immutable-listmap-diff             ; FUNCTION
   ))

(in-package #:coalton-impl/algorithm/immutable-listmap)

;;
;; Immutable Listmap
;; Wrapper around fset:map<fset:seq>
;;

(defstruct immutable-listmap
  (data (fset:empty-map (fset:empty-seq)) :type fset:map :read-only t))

(defun immutable-listmap-lookup (m key &key no-error)
  "Lookup key in M"
  (declare (type immutable-listmap m)
           (type symbol key)
           (type boolean no-error)
           (values fset:seq))
  (multiple-value-bind (value present-p)
      (fset:lookup (immutable-listmap-data m) key)
    (unless (or present-p no-error)
      (error "Undefined key ~a" key))
    value))

(defun immutable-listmap-push (m key value &optional (constructor #'make-immutable-listmap))
  "Push value to the list at KEY in M"
  (declare (type immutable-listmap m)
           (type symbol key)
           (values immutable-listmap &optional))

  (let ((map (immutable-listmap-data m)))
    (funcall constructor :data (fset:with map key (fset:with-first (fset:lookup map key) value)))))

(defun immutable-listmap-replace (m key index value &optional (constructor #'make-immutable-listmap))
  "Replace value at INDEX with VALUE in the map at KEY in M."
  (declare (type immutable-listmap m)
           (type symbol key)
           (type fixnum index)
           (values immutable-listmap &optional))
  (let ((map (immutable-listmap-data m)))
    (funcall constructor :data (fset:with map key (fset:with (fset:lookup map key) index value)))))

(defun immutable-listmap-diff (m1 m2 &optional (constructor #'make-immutable-listmap))
  "Return a new SHADOW-LIST with elements in M1 but not M2."
  (let ((diff-map (fset:empty-map)))
    (fset:do-map (k v1 (immutable-listmap-data m1))
      (multiple-value-bind (v2 found)
          (fset:lookup (immutable-listmap-data m2) k)
        (if found
            ;; If both have this key then diff the seq
            (let ((seq-diff (fset:convert 'fset:seq
                                     (fset:set-difference (fset:range v1)
                                                          (fset:range v2)))))
              (setf diff-map (fset:with diff-map k seq-diff)))
            ;; Otherwise save the whole seq
            (setf diff-map (fset:with diff-map k v1)))))
    (funcall constructor :data diff-map)))
