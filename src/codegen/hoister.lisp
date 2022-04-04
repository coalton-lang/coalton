(defpackage #:coalton-impl/codegen/hoister
  (:use
   #:cl
   #:coalton-impl/util
   #:coalton-impl/codegen/ast)
  (:export
   #:hoister
   #:make-hoister
   #:push-hoist-point
   #:pop-hoist-point
   #:pop-final-hoist-point
   #:hoist-definition))

(in-package #:coalton-impl/codegen/hoister)

(defstruct (hoist-point (:constructor make-hoist-point (bound-variables)))
  (bound-variables (required 'bound-variables)                :type symbol-list :read-only nil)
  (definitions     (make-hash-table :test #'equalp)           :type hash-table  :read-only t))

(defun hoist-point-blocks (node hoist-point)
  (declare (type node node)
           (type hoist-point hoist-point))
  (let ((node-variables (node-variables node)))
    (when (intersection node-variables (hoist-point-bound-variables hoist-point))
      (return-from hoist-point-blocks t))

    (loop :for var :being :the :hash-values :of (hoist-point-definitions hoist-point)
          :if (find (node-variable-value var) node-variables :test #'equalp)
            :do (return-from hoist-point-blocks t))

    nil))

(defun hoist-point-add (node package hoist-point)
  (declare (values node-variable))
  (if (gethash node (hoist-point-definitions hoist-point))
      (values (gethash node (hoist-point-definitions hoist-point)))
      (progn
        (setf (gethash node (hoist-point-definitions hoist-point))
              (node-variable (node-type node)
                             (alexandria:ensure-symbol (gensym "hoisted_") package)))
        (values (gethash node (hoist-point-definitions hoist-point))))))

(defun hoist-point-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'hoist-point-p x)))

(deftype hoist-point-list ()
  `(satisfies hoist-point-list-p))

(defstruct (hoister (:constructor make-hoister ()))
  (hoist-points    nil                    :type hoist-point-list :read-only nil)
  (top-hoist-point (make-hoist-point nil) :type hoist-point      :read-only t))

(defun definitions-to-bindings (definitions)
  (declare (type hash-table definitions))
  (loop :for node :being :the :hash-keys :of definitions
        :for var :being :the :hash-values :of definitions
        :collect (cons (node-variable-value var) node)))

(defun push-hoist-point (bound-variables hoister)
  (declare (type symbol-list bound-variables)
           (type hoister hoister))
  (push (make-hoist-point bound-variables) (hoister-hoist-points hoister)))

(defun pop-hoist-point (hoister)
  (declare (type hoister hoister))
  (definitions-to-bindings (hoist-point-definitions (pop (hoister-hoist-points hoister)))))

(defun pop-final-hoist-point (hoister)
  (declare (type hoister hoister))
  (assert (null (hoister-hoist-points hoister)))
  (definitions-to-bindings (hoist-point-definitions (hoister-top-hoist-point hoister))))


(defun hoist-definition (node package hoister)
  (declare (type node node)
           (type package package)
           (type hoister hoister)
           (values node-variable &optional))
  (loop :for hoist-point :in (hoister-hoist-points hoister)
        :if (hoist-point-blocks node hoist-point)
          :do (return-from hoist-definition (hoist-point-add node package hoist-point)))
  (hoist-point-add node package (hoister-top-hoist-point hoister)))
