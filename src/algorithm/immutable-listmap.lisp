(defpackage #:coalton-impl/algorithm/immutable-listmap
  (:use #:cl)
  (:local-nicknames (#:avl #:coalton-impl/algorithm/avl-tree))
  (:export
   #:immutable-listmap                  ; STRUCT
   #:make-immutable-listmap             ; CONSTRUCTOR
   #:immutable-listmap-data             ; ACCESSOR
   #:immutable-listmap-lookup           ; FUNCTION
   #:immutable-listmap-push             ; FUNCTION
   #:immutable-listmap-replace          ; FUNCTION
   #:immutable-listmap-diff             ; FUNCTION
   #:immutable-listmap-foreach          ; FUNCTION
   ))

(in-package #:coalton-impl/algorithm/immutable-listmap)


;;; Immutable Listmap — a persistent map whose values are lists.
;;;
;;; Each key maps to a list of items.  PUSH prepends to the list.
;;; REPLACE updates the item at a specific index.
;;; Replaces FSet's map-of-seqs pattern with plain CL lists.

(defstruct immutable-listmap
  (data (avl:empty-avl-tree) :type avl:avl-tree :read-only t))

(defmethod make-load-form ((self immutable-listmap) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun immutable-listmap-lookup (m key &key no-error)
  "Look up KEY in M.  Returns the list of values for KEY.
Signals an error if KEY is absent unless NO-ERROR is true."
  (declare (type immutable-listmap m)
           (type boolean no-error)
           (values list))
  (multiple-value-bind (value present-p)
      (avl:avl-lookup (immutable-listmap-data m) key)
    (unless (or present-p no-error)
      (error "Undefined key ~a" key))
    (if present-p value nil)))

(defun immutable-listmap-push (m key value &optional (constructor #'make-immutable-listmap))
  "Return a new listmap with VALUE prepended to the list at KEY."
  (declare (type immutable-listmap m)
           (values immutable-listmap &optional))
  (let* ((tree (immutable-listmap-data m))
         (old-list (multiple-value-bind (v found) (avl:avl-lookup tree key)
                     (if found v nil))))
    (funcall constructor :data (avl:avl-set tree key (cons value old-list)))))

(defun immutable-listmap-replace (m key index value &optional (constructor #'make-immutable-listmap))
  "Return a new listmap with the element at INDEX in KEY's list replaced by VALUE."
  (declare (type immutable-listmap m)
           (type fixnum index)
           (values immutable-listmap &optional))
  (let* ((tree (immutable-listmap-data m))
         (old-list (multiple-value-bind (v found) (avl:avl-lookup tree key)
                     (if found v nil)))
         (new-list (loop :for elt :in old-list
                         :for i :of-type fixnum :from 0
                         :collect (if (= i index) value elt))))
    (funcall constructor :data (avl:avl-set tree key new-list))))

(defun immutable-listmap-diff (m1 m2 &optional (constructor #'make-immutable-listmap))
  "Return entries from M1 whose elements are absent from the corresponding M2 lists."
  (let ((result (avl:empty-avl-tree)))
    (avl:avl-foreach
     (lambda (k v1)
       (multiple-value-bind (v2 found) (avl:avl-lookup (immutable-listmap-data m2) k)
         (if found
             ;; Both have this key: diff the lists (treat as sets)
             (let ((diff (set-difference v1 v2 :test #'equal)))
               (when diff
                 (setf result (avl:avl-set result k diff))))
             ;; Only in m1: keep entire list
             (setf result (avl:avl-set result k v1)))))
     (immutable-listmap-data m1))
    (funcall constructor :data result)))

(defun immutable-listmap-foreach (fn m)
  "Call (funcall FN key list) for each entry in M."
  (declare (type function fn)
           (type immutable-listmap m))
  (avl:avl-foreach fn (immutable-listmap-data m)))
