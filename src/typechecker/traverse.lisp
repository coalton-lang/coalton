(defpackage #:coalton-impl/typechecker/traverse
  (:use
   #:cl
   #:coalton-impl/typechecker/expression)
  (:export
   #:traverse-block                     ; STRUCT
   #:make-traverse-block                ; CONSTRUCTOR
   #:traverse                           ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/traverse)

(defstruct (traverse-block
            (:conc-name traverse-))
  (variable        #'identity :type function :read-only t)
  (literal         #'identity :type function :read-only t)
  (integer-literal #'identity :type function :read-only t)
  (bind            #'identity :type function :read-only t)
  (body            #'identity :type function :read-only t)
  (abstraction     #'identity :type function :read-only t)
  (let-binding     #'identity :type function :read-only t)
  (let             #'identity :type function :read-only t)
  (lisp            #'identity :type function :read-only t)
  (match-branch    #'identity :type function :read-only t)
  (match           #'identity :type function :read-only t)
  (progn           #'identity :type function :read-only t)
  (return          #'identity :type function :read-only t)
  (application     #'identity :type function :read-only t)
  (or              #'identity :type function :read-only t)
  (and             #'identity :type function :read-only t)
  (if              #'identity :type function :read-only t)
  (when            #'identity :type function :read-only t)
  (unless          #'identity :type function :read-only t)
  (cond-clause     #'identity :type function :read-only t)
  (cond            #'identity :type function :read-only t)
  (do-bind         #'identity :type function :read-only t)
  (do              #'identity :type function :read-only t))

(defgeneric traverse (node block)
  (:method ((node node-variable) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall (traverse-variable block) node))

  (:method ((node node-literal) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall (traverse-literal block) node))

  (:method ((node node-integer-literal) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall (traverse-integer-literal block) node))

  (:method ((node node-bind) block)
    (declare (type traverse-block block)
             (values node-bind &optional))

    (funcall
     (traverse-bind block)
     (make-node-bind
      :pattern (node-bind-pattern node)
      :expr (traverse (node-bind-expr node) block)
      :source (node-bind-source node))))

  (:method ((node node-body) block)
    (declare (type traverse-block block)
             (values node-body &optional))

    (funcall
     (traverse-body block)
     (make-node-body
      :nodes (traverse (node-body-nodes node) block)
      :last-node (traverse (node-body-last-node node) block))))

  (:method ((node node-abstraction) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-abstraction block)
     (make-node-abstraction
      :type (node-type node)
      :source (node-source node)
      :vars (traverse (node-abstraction-vars node) block)
      :body (traverse (node-abstraction-body node) block))))

  (:method ((node node-let-binding) block)
    (declare (type traverse-block block)
             (values node-let-binding &optional))

    (funcall
     (traverse-let-binding block)
     (make-node-let-binding
      :name (traverse (node-let-binding-name node) block)
      :value (traverse (node-let-binding-value node) block)
      :source (node-let-binding-source node))))

  (:method ((node node-let) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-let block)
     (make-node-let
      :type (node-type node)
      :source (node-source node)
      :bindings (traverse (node-let-bindings node) block)
      :body (traverse (node-let-body node) block))))

  (:method ((node node-lisp) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-lisp block)
     (make-node-lisp
      :type (node-type node)
      :source (node-source node)
      :vars (traverse (node-lisp-vars node) block)
      :var-names (node-lisp-var-names node)
      :body (node-lisp-body node))))

  (:method ((node node-match-branch) block)
    (declare (type traverse-block block)
             (values node-match-branch &optional))

    (funcall
     (traverse-match-branch block)
     (make-node-match-branch
      :pattern (node-match-branch-pattern node)
      :body (traverse (node-match-branch-body node) block)
      :source (node-match-branch-source node))))

  (:method ((node node-match) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-match block)
     (make-node-match
      :type (node-type node)
      :source (node-source node)
      :expr (traverse (node-match-expr node) block)
      :branches (traverse (node-match-branches node) block))))

  (:method ((node node-progn) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-progn block)
     (make-node-progn
      :type (node-type node)
      :source (node-source node)
      :body (traverse (node-progn-body node) block))))

  (:method ((node node-return) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-return block)
     (make-node-return
      :type (node-type node)
      :source (node-source node)
      :expr (traverse (node-return-expr node) block) ; the nil case is handled by the list instance
      )))

  (:method ((node node-application) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-application block)
     (make-node-application
      :type (node-type node)
      :source (node-source node)
      :rator (traverse (node-application-rator node) block)
      :rands (traverse (node-application-rands node) block))))

  (:method ((node node-or) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-or block)
     (make-node-or
      :type (node-type node)
      :source (node-source node)
      :nodes (traverse (node-or-nodes node) block))))

  (:method ((node node-and) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-and block)
     (make-node-and
      :type (node-type node)
      :source (node-source node)
      :nodes (traverse (node-and-nodes node) block))))

  (:method ((node node-if) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-if block)
     (make-node-if
      :type (node-type node)
      :source (node-source node)
      :expr (traverse (node-if-expr node) block)
      :then (traverse (node-if-then node) block)
      :else (traverse (node-if-else node) block))))

  (:method ((node node-when) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-when block)
     (make-node-when
      :type (node-type node)
      :source (node-source node)
      :expr (traverse (node-when-expr node) block)
      :body (traverse (node-when-body node) block))))

  (:method ((node node-unless) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-unless block)
     (make-node-unless
      :type (node-type node)
      :source (node-source node)
      :expr (node-unless-expr node)
      :body (node-unless-body node))))

  (:method ((node node-cond-clause) block)
    (declare (type traverse-block block)
             (values node-cond-clause &optional))

    (funcall
     (traverse-cond-clause block)
     (make-node-cond-clause
      :expr (traverse (node-cond-clause-expr node) block)
      :body (traverse (node-cond-clause-body node) block)
      :source (node-cond-clause-source node))))

  (:method ((node node-cond) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-cond block)
     (make-node-cond
      :type (node-type node)
      :source (node-source node)
      :clauses (traverse (node-cond-clauses node) block))))

  (:method ((node node-do-bind) block)
    (declare (type traverse-block block)
             (values node-do-bind &optional))

    (funcall
     (traverse-do-bind block)
     (make-node-do-bind
      :pattern (node-do-bind-pattern node)
      :expr (traverse (node-do-bind-expr node) block)
      :source (node-do-bind-source node))))

  (:method ((node node-do) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-do block)
     (make-node-do
      :type (node-type node)
      :source (node-source node)
      :nodes (traverse (node-do-nodes node) block)
      :last-node (traverse (node-do-last-node node) block))))

  (:method ((list list) block)
    (declare (type traverse-block block)
             (values list))

    (loop :for node :in list
          :collect (traverse node block))))
