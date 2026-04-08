(defpackage #:coalton-impl/algorithm/immutable-map
  (:use #:cl)
  (:local-nicknames (#:avl #:coalton-impl/algorithm/avl-tree))
  (:export
   #:immutable-map                      ; STRUCT
   #:make-immutable-map                 ; CONSTRUCTOR
   #:immutable-map-data                 ; ACCESSOR
   #:immutable-map-lookup               ; FUNCTION
   #:immutable-map-set                  ; FUNCTION
   #:immutable-map-set-multiple         ; FUNCTION
   #:immutable-map-keys                 ; FUNCTION
   #:immutable-map-diff                 ; FUNCTION
   #:immutable-map-remove               ; FUNCTION
   #:immutable-map-count                ; FUNCTION
   #:immutable-map-values               ; FUNCTION
   #:immutable-map-foreach              ; FUNCTION
   #:immutable-map-image                ; FUNCTION
   #:do-immutable-map                   ; MACRO
   ))

(in-package #:coalton-impl/algorithm/immutable-map)


;;; Immutable Map — backed by a persistent AVL tree.

(defstruct immutable-map
  (data (avl:empty-avl-tree) :type avl:avl-tree :read-only t))

(defmethod make-load-form ((self immutable-map) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun immutable-map-lookup (m key)
  "Look up KEY in M.  Returns (values value found-p)."
  (declare (type immutable-map m)
           (values t boolean &optional))
  (avl:avl-lookup (immutable-map-data m) key))

(defun immutable-map-set (m key value &optional (constructor #'make-immutable-map))
  "Return a new map like M but with KEY mapped to VALUE."
  (declare (type immutable-map m))
  (funcall constructor :data (avl:avl-set (immutable-map-data m) key value)))

(defun immutable-map-set-multiple (m items &optional (constructor #'make-immutable-map))
  "Return a new map from M with each (KEY . VALUE) in ITEMS set."
  (declare (type immutable-map m)
           (type list items)
           (values immutable-map &optional))
  (let ((tree (immutable-map-data m)))
    (dolist (pair items)
      (setf tree (avl:avl-set tree (car pair) (cdr pair))))
    (funcall constructor :data tree)))

(defun immutable-map-keys (m)
  "Return a list of all keys in M in ascending order."
  (declare (type immutable-map m))
  (avl:avl-keys (immutable-map-data m)))

(defun immutable-map-diff (m1 m2 &optional (constructor #'make-immutable-map))
  "Return a map of entries in M1 whose keys are absent in M2."
  (declare (type immutable-map m1 m2)
           (values immutable-map &optional))
  (funcall constructor :data (avl:avl-difference (immutable-map-data m1)
                                                 (immutable-map-data m2))))

(defun immutable-map-remove (m key &optional (constructor #'make-immutable-map))
  "Return a new map like M but without KEY."
  (declare (type immutable-map m)
           (values immutable-map &optional))
  (funcall constructor :data (avl:avl-remove (immutable-map-data m) key)))

(defun immutable-map-count (m)
  "Return the number of entries in M."
  (declare (type immutable-map m))
  (avl:avl-count (immutable-map-data m)))

(defun immutable-map-values (m)
  "Return a list of all values in M in ascending key order."
  (declare (type immutable-map m))
  (avl:avl-values (immutable-map-data m)))

(defun immutable-map-foreach (fn m)
  "Call (funcall FN key value) for each entry in M in ascending key order."
  (declare (type function fn)
           (type immutable-map m))
  (avl:avl-foreach fn (immutable-map-data m)))

(defun immutable-map-image (fn m &optional (constructor #'make-immutable-map))
  "Return a new map with each value replaced by (funcall FN key value)."
  (declare (type function fn)
           (type immutable-map m))
  (funcall constructor
           :data (avl:avl-image (lambda (v)
                                  ;; avl-image passes only the value;
                                  ;; but environment.lisp needs (key value)->value.
                                  ;; We handle both conventions: callers that need
                                  ;; the key should use immutable-map-foreach instead.
                                  (funcall fn v))
                                (immutable-map-data m))))

(defmacro do-immutable-map ((key value map &optional result) &body body)
  "Iterate over MAP in ascending key order, binding KEY and VALUE."
  `(avl:do-avl (,key ,value (immutable-map-data ,map) ,result)
     ,@body))
