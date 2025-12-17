(defpackage #:coalton-impl/codegen/tail-analysis
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:local-nicknames
)
  (:export
   ))

(in-package #:coalton-impl/codegen/tail-analysis)

(defun map-tail-nodes (f top-node)
  (declare (type node top-node))
  (labels ((descend (node tail? tail-blocks)
             (etypecase node
               (node-literal
                (when tail?
                  (funcall f node)))

               (node-variable
                (when tail?
                  (funcall f node)))

               (node-application
                (when tail?
                  (funcall f node))
                (descend (node-application-rator node) nil tail-blocks)
                (dolist (subnode (node-application-rands node))
                  (descend subnode nil tail-blocks)))

               (node-direct-application
                (when tail?
                  (funcall f node))
                (dolist (subnode (node-direct-application-rands node))
                  (descend subnode nil tail-blocks)))

               (node-multiple-values-application
                (when tail?
                  (funcall f node))
                (dolist (subnode (node-multiple-values-application-rands node))
                  (descend subnode nil tail-blocks)))

               (node-multiple-values
                (when tail?
                  (funcall f node))
                (dolist (subnode (node-multiple-values-exprs node))
                  (descend subnode nil tail-blocks)))

               (node-abstraction
                (when tail?
                  (funcall f node)))

               (node-lisp
                (when tail?
                  (funcall f node)))
               
               (node-locally
                (descend (node-locally-subexpr node) tail? tail-blocks))

               (node-let
                ;; Bindings aren't in tail position.
                (loop :for binding :in (node-let-bindings node)
                      :do (descend (cdr binding) nil tail-blocks))
                ;; The body inherits the tail position of the parent.
                (descend (node-let-subexpr node) tail? tail-blocks))

               (node-match
                ;; Expression being matched is not in tail position.
                (descend (node-match-expr node) nil tail-blocks)
                ;; The branches inherit the tail position of the parents.
                (loop :for branch :in (node-match-branches node)
                      :for body := (match-branch-body branch)
                      :do (descend body tail? tail-blocks)))

               (node-catch
                (descend (node-catch-expr node) tail? tail-blocks)
                (dolist (branch (node-catch-branches node))
                  (descend (catch-branch-body branch) tail? tail-blocks)))
               
               (node-resumable
                (descend (node-resumable-expr node) tail? tail-blocks)
                (dolist (branch (node-resumable-branches node))
                  (descend (resumable-branch-body branch) tail? tail-blocks)))

               (node-while
                (when tail?
                  (funcall f node))
                (descend (node-while-expr node) nil tail-blocks)
                (descend (node-while-body node) nil tail-blocks))

               (node-while-let
                (when tail?
                  (funcall f node))
                (descend (node-while-let-expr node) nil tail-blocks)
                (descend (node-while-let-body node) nil tail-blocks))

               (node-loop
                (when tail?
                  (funcall f node))
                (descend (node-loop-body node) nil tail-blocks))

               (node-break
                nil)

               (node-continue
                nil)
               
               (node-seq
                ;; Only the last subnode is in tail position.
                (let ((subnodes (node-seq-nodes node)))
                  (loop :until (endp subnodes)
                        :if (endp (rest subnodes))
                          :do (descend (pop subnodes) tail? tail-blocks)
                        :else
                          :do (descend (pop subnodes) nil tail-blocks))))
               
               (node-return-from
                ;; It doesn't matter if TAIL? is NIL. If we are
                ;; returning from a tail block, we are in tail
                ;; position.
                (let ((tail-block? (member (node-return-from-name node) tail-blocks)))
                  (descend (node-return-from-expr node) tail-block? tail-blocks)))
               
               (node-throw
                ;; Non-local exit. Only possibility is if we have a
                ;; block return within.
                (descend (node-throw-expr node) nil tail-blocks))

               (node-resume-to
                ;; Non-local exit. Only possibility is if we have a
                ;; block return within.
                (descend (node-resume-to-expr node) nil tail-blocks))

               (node-block
                (descend (node-block-body node)
                         tail?
                         (if tail?
                             (cons (node-block-name node) tail-blocks)
                             tail-blocks)))

               (node-field
                (when tail?
                  (funcall f node))
                (descend (node-field-dict node) nil tail-blocks))
               
               (node-dynamic-extent
                (descend (node-dynamic-extent-node node) nil tail-blocks)
                (descend (node-dynamic-extent-body node) tail? tail-blocks))

               (node-bind
                (descend (node-bind-expr node) nil tail-blocks)
                (descend (node-bind-body node) tail? tail-blocks)))))
    (if (node-abstraction-p top-node)
        (descend (node-abstraction-subexpr top-node) t nil)
        (descend top-node t nil))))

(defun tuple-application-p (node)
  (and (node-direct-application-p node)
       (find-package "COALTON-LIBRARY/CLASSES")
       (eq (find-symbol "TUPLE" "COALTON-LIBRARY/CLASSES")
           (node-direct-application-rator node))))

(defun tail-is-always-tuple? (node name)
  (let ((tuple-nodes nil))
    (flet ((check (node)
             (cond
               ((tuple-application-p node)
                (push node tuple-nodes))
               ((and (node-direct-application-p node)
                     (eq name (node-direct-application-rator node)))
                nil)
               (t
                (return-from tail-is-always-tuple? (values nil nil))))))
      (map-tail-nodes #'check node)
      (values t tuple-nodes))))


(defun rewrite-tail-nodes (f top-node)
  (declare (type node top-node))
  (labels ((descend (node tail? tail-blocks)
             (etypecase node
               (node-literal
                (if tail?
                    (funcall f node)
                    node))

               (node-variable
                (if tail?
                    (funcall f node)
                    node))

               (node-application
                (if tail?
                    (let ((new-node (funcall f node)))
                      (if (not (eq node new-node))
                          new-node
                          (make-node-application
                           :type (node-type node)
                           :properties (node-properties node)
                           :rator (descend (node-application-rator node) nil tail-blocks)
                           :rands (mapcar (lambda (subnode) (descend subnode nil tail-blocks))
                                          (node-application-rands node)))))
                    (make-node-application
                     :type (node-type node)
                     :properties (node-properties node)
                     :rator (descend (node-application-rator node) nil tail-blocks)
                     :rands (mapcar (lambda (subnode) (descend subnode nil tail-blocks))
                                    (node-application-rands node)))))

               (node-direct-application
                (if tail?
                    (let ((new-node (funcall f node)))
                      (if (not (eq node new-node))
                          new-node
                          (make-node-direct-application
                           :type (node-type node)
                           :properties (node-properties node)
                           :rator-type (node-direct-application-rator-type node)
                           :rator (node-direct-application-rator node)
                           :rands (mapcar (lambda (subnode) (descend subnode nil tail-blocks))
                                          (node-direct-application-rands node)))))
                    (make-node-direct-application
                     :type (node-type node)
                     :properties (node-properties node)
                     :rator-type (node-direct-application-rator-type node)
                     :rator (node-direct-application-rator node)
                     :rands (mapcar (lambda (subnode) (descend subnode nil tail-blocks))
                                    (node-direct-application-rands node)))))

               (node-multiple-values-application
                (if tail?
                    (let ((new-node (funcall f node)))
                      (if (not (eq node new-node))
                          new-node
                          (make-node-multiple-values-application
                           :type (node-type node)
                           :properties (node-properties node)
                           :rator-type (node-multiple-values-application-rator-type node)
                           :rator (node-multiple-values-application-rator node)
                           :rands (mapcar (lambda (subnode) (descend subnode nil tail-blocks))
                                          (node-multiple-values-application-rands node)))))
                    (make-node-multiple-values-application
                     :type (node-type node)
                     :properties (node-properties node)
                     :rator-type (node-multiple-values-application-rator-type node)
                     :rator (node-multiple-values-application-rator node)
                     :rands (mapcar (lambda (subnode) (descend subnode nil tail-blocks))
                                    (node-multiple-values-application-rands node)))))
               

               (node-multiple-values
                (if tail?
                    (let ((new-node (funcall f node)))
                      (if (not (eq node new-node))
                          new-node
                          (make-node-multiple-values
                           :type (node-type node)
                           :exprs (mapcar (lambda (subnode) (descend subnode nil tail-blocks))
                                          (node-multiple-values-exprs node)))))
                    (make-node-multiple-values
                     :type (node-type node)
                     :exprs (mapcar (lambda (subnode) (descend subnode nil tail-blocks))
                                    (node-multiple-values-exprs node)))))

               (node-abstraction
                (if tail?
                    (funcall f node)
                    node))

               (node-lisp
                (if tail?
                    (funcall f node)
                    node))
               
               (node-locally
                (make-node-locally
                 :type (node-type node)
                 :noinline-functions (node-locally-noinline-functions node)
                 :subexpr (descend (node-locally-subexpr node) tail? tail-blocks)))

               (node-let
                (make-node-let
                 :type (node-type node)
                 :bindings (loop :for binding :in (node-let-bindings node)
                                 :collect (cons (car binding)
                                                (descend (cdr binding) nil tail-blocks)))
                 :subexpr (descend (node-let-subexpr node) tail? tail-blocks)))

               (node-match
                (make-node-match
                 :type (node-type node)
                 :branches (loop :for branch :in (node-match-branches node)
                                 :for pattern := (match-branch-pattern branch)
                                 :for body := (match-branch-body branch)
                                 :collect (make-match-branch
                                           :pattern pattern
                                           :body (descend body tail? tail-blocks)))
                 :expr (descend (node-match-expr node) nil tail-blocks)))

               (node-catch
                (make-node-catch
                 :type (node-type node)
                 :expr (descend (node-catch-expr node) tail? tail-blocks)
                 :branches (loop :for branch :in (node-catch-branches node)
                                 :for pattern := (catch-branch-pattern branch)
                                 :for body := (catch-branch-body branch)
                                 :collect (make-catch-branch
                                           :pattern pattern
                                           :body (descend body tail? tail-blocks)))))
               
               (node-resumable
                (make-node-resumable
                 :type (node-type node)
                 :expr (descend (node-resumable-expr node) tail? tail-blocks)
                 :branches (loop :for branch :in (node-resumable-branches node)
                                 :for pattern := (resumable-branch-pattern branch)
                                 :for body := (resumable-branch-body branch)
                                 :collect (make-resumable-branch
                                           :pattern pattern
                                           :body (descend body tail? tail-blocks)))))

               (node-while
                (if tail?
                    (let ((new-node (funcall f node)))
                      (if (not (eq node new-node))
                          new-node
                          (make-node-while
                           :type (node-type node)
                           :label (node-while-label node)
                           :expr (descend (node-while-expr node) nil tail-blocks)
                           :body (descend (node-while-body node) nil tail-blocks))))
                    (make-node-while
                     :type (node-type node)
                     :label (node-while-label node)
                     :expr (descend (node-while-expr node) nil tail-blocks)
                     :body (descend (node-while-body node) nil tail-blocks))))

               (node-while-let
                (if tail?
                    (let ((new-node (funcall f node)))
                      (if (not (eq node new-node))
                          new-node
                          (make-node-while-let
                           :type (node-type node)
                           :label (node-while-let-label node)
                           :pattern (node-while-let-pattern node)
                           :expr (descend (node-while-let-expr node) nil tail-blocks)
                           :body (descend (node-while-let-body node) nil tail-blocks))))
                    (make-node-while-let
                     :type (node-type node)
                     :label (node-while-let-label node)
                     :pattern (node-while-let-pattern node)
                     :expr (descend (node-while-let-expr node) nil tail-blocks)
                     :body (descend (node-while-let-body node) nil tail-blocks))))

               (node-loop
                (if tail?
                    (let ((new-node (funcall f node)))
                      (if (not (eq node new-node))
                          new-node
                          (make-node-loop
                           :type (node-type node)
                           :label (node-loop-label node)
                           :body (descend (node-loop-body node) nil tail-blocks))))
                    (make-node-loop
                     :type (node-type node)
                     :label (node-loop-label node)
                     :body (descend (node-loop-body node) nil tail-blocks))))

               (node-break
                node)

               (node-continue
                node)
               
               (node-seq
                (make-node-seq
                 :type (node-type node)
                 :nodes (loop :with subnodes := (node-seq-nodes node)
                              :until (endp subnodes)
                              :if (endp (rest subnodes))
                                :collect (descend (pop subnodes) tail? tail-blocks)
                              :else
                                :collect (descend (pop subnodes) nil tail-blocks))))
               
               (node-return-from
                (make-node-return-from
                 :type (node-type node)
                 :name (node-return-from-name node)
                 :expr (let ((tail-block? (member (node-return-from-name node) tail-blocks)))
                         (descend (node-return-from-expr node) tail-block? tail-blocks))))
               
               (node-throw
                (make-node-throw
                 :type (node-type node)
                 :expr (descend (node-throw-expr node) nil tail-blocks)))

               (node-resume-to
                (make-node-resume-to
                 :type (node-type node)
                 :expr (descend (node-resume-to-expr node) nil tail-blocks)))

               (node-block
                (make-node-block
                 :type (node-type node)
                 :name (node-block-name node)
                 :body (descend (node-block-body node)
                                tail?
                                (if tail?
                                    (cons (node-block-name node) tail-blocks)
                                    tail-blocks))))

               (node-field
                (if tail?
                    (let ((new-node (funcall f node)))
                      (if (not (eq node new-node))
                          new-node
                          (make-node-field
                           :type (node-type node)
                           :name (node-field-name node)
                           :dict (descend (node-field-dict node) nil tail-blocks))))
                    (make-node-field
                     :type (node-type node)
                     :name (node-field-name node)
                     :dict (descend (node-field-dict node) nil tail-blocks))))
               
               (node-dynamic-extent
                (make-node-dynamic-extent
                 :type (node-type node)
                 :name (node-dynamic-extent-name node)
                 :node (descend (node-dynamic-extent-node node) nil tail-blocks)
                 :body (descend (node-dynamic-extent-body node) tail? tail-blocks)))

               (node-bind
                (make-node-bind
                 :type (node-type node)
                 :name (node-bind-name node)
                 :expr (descend (node-bind-expr node) nil tail-blocks)
                 :body (descend (node-bind-body node) tail? tail-blocks))
                
                ))))
    (if (node-abstraction-p top-node)
        (make-node-abstraction
         :type (node-type top-node)
         :vars (node-abstraction-vars top-node)
         :subexpr (descend (node-abstraction-subexpr top-node) t nil))
        (descend top-node t nil))))
