(cl:in-package #:coalton-impl)

(defclass coalton-cst-client (eclector.parse-result:parse-result-client)
  ())

(defvar *coalton-cst-client* (make-instance 'coalton-cst-client))

(defmethod eclector.parse-result:make-expression-result ((client coalton-cst-client) expression children source)
  (labels ((make-atom-cst (expression &optional source)
             (make-instance 'cst:atom-cst
                            :raw expression
                            :source source))
           (make-cons-cst (expression children &optional source)
             (destructuring-bind (car . cdr) expression
               (declare (ignore car))
               (destructuring-bind (car-children . cdr-children) children
                 (make-instance 'cst:cons-cst
                                :raw expression
                                :first car-children
                                :rest (if (atom cdr)
                                          (make-atom-cst cdr)
                                          (make-cons-cst cdr cdr-children))
                                :source source)))))
    (cond
      ((atom expression)
       (make-atom-cst expression source))
      ;; List structure with corresponding elements.
      ((and (eql (list-length expression) (length children))
            (every (lambda (sub-expression child)
                     (eql sub-expression (cst:raw child)))
                   expression children))
       (make-cons-cst expression children source))
      ;; Structure mismatch, try heuristic reconstruction.
      (t
       ;(cst:reconstruct expression children client :default-source source)
       ;; We don't use
       ;;
       ;;   (cst:reconstruct expression children client)
       ;;
       ;; because we want SOURCE for the outer CONS-CST but not any of
       ;; its children.
       (destructuring-bind (car . cdr) expression
         (make-instance 'cst:cons-cst
                        :raw expression
                        :first (cst:reconstruct car children client)
                        :rest (cst:reconstruct cdr children client)
                        :source source))))))

(defun cst-read (&rest arguments)
  (let* ((eclector.reader:*client* (or eclector.reader:*client* *coalton-cst-client*)))
    (apply #'eclector.parse-result:read arguments)))
