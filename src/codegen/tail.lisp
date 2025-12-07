(defpackage #:coalton-impl/codegen/tail
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:local-nicknames
)
  (:export
   ))

(in-package #:coalton-impl/codegen/tail)

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


;; example

;; (coalton-toplevel
;;   (define (counts lis even odd)
;;     (match lis
;;       ((Nil) (Tuple even odd))
;;       ((Cons x xs)
;;        (cond
;;          ((even? x) (counts xs (1+ even) odd))
;;          (True      (counts xs even (1+ odd))))))))

;; (coalton-toplevel
;;   (define (even-count lis)
;;     (fst (counts lis)))

;;   (define (odd-count lis)
;;     (snd (counts lis)))

;;   (define (len lis)
;;     (match (counts lis)
;;       ((Tuple even odd) (+ even odd)))))
