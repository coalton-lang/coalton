(defpackage #:coalton-impl/codegen/specializer
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/traverse
   #:action
   #:traverse)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:apply-specializations))            ; FUNCTION

(in-package #:coalton-impl/codegen/specializer)

(defun apply-specializations (node env)
  (declare (type node node)
           (type tc:environment env)
           (values node &optional))

  (when settings:*coalton-disable-specialization*
    (return-from apply-specializations node))

  (labels ((apply-specialization (node)
             (let ((rator-name (node-rator-name node)))
               (unless rator-name
                 (return-from apply-specialization))

               (let ((from-ty (tc:lookup-value-type env rator-name :no-error t)))
                 (unless from-ty
                   (return-from apply-specialization))

                 (let* ((from-ty (tc:fresh-inst from-ty))

                        (preds (tc:qualified-ty-predicates from-ty))

                        (num-preds (length preds))

                        (rator-type
                          (tc:make-function-type*
                           (subseq (tc:function-type-arguments (node-rator-type node)) num-preds)
                           (tc:function-return-type (node-rator-type node))))

                        (specialization (tc:lookup-specialization-by-type env rator-name rator-type :no-error t)))
                   (unless specialization
                     (return-from apply-specialization))

                   (unless (>= (length (node-rands node)) num-preds)
                     (util:coalton-bug "Expected function ~A to have at least ~A args when applying specialization." rator-name (length preds)))

                   (when settings:*print-specialization-occurrences*
                     (format t
                             "~&;; Specializing ~S to ~S~%"
                             (tc:specialization-entry-from specialization)
                             (tc:specialization-entry-to specialization)))
                   (cond
                     ((= num-preds (length (node-rands node)))
                      (make-node-variable
                       :type rator-type
                       :value (tc:specialization-entry-to specialization)))

                     ((< num-preds (length (node-rands node)))
                      (make-node-application
                       :type (node-type node)
                       :properties (node-properties node)
                       :rator (make-node-variable
                               :type rator-type
                               :value (tc:specialization-entry-to specialization))
                       :rands (subseq (node-rands node) num-preds)))

                     (t
                      (util:coalton-bug "Invalid specialization ~A~%" specialization))))))))
    (traverse
     node
     (list
      (action (:after node-application) #'apply-specialization)
      (action (:after node-direct-application) #'apply-specialization)))))

