(defpackage #:coalton-impl/typechecker/traverse
  (:use
   #:cl
   #:coalton-impl/typechecker/expression)
  (:local-nicknames
   (#:source #:coalton-impl/source))
  (:export
   #:traverse-block                     ; STRUCT
   #:make-traverse-block                ; CONSTRUCTOR
   #:traverse                           ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/traverse)

(defstruct (traverse-block
            (:conc-name traverse-))
  (variable           #'identity :type function :read-only t)
  (accessor           #'identity :type function :read-only t)
  (literal            #'identity :type function :read-only t)
  (integer-literal    #'identity :type function :read-only t)
  (bind               #'identity :type function :read-only t)
  (values-bind        #'identity :type function :read-only t)
  (body               #'identity :type function :read-only t)
  (abstraction        #'identity :type function :read-only t)
  (let-binding        #'identity :type function :read-only t)
  (dynamic-binding    #'identity :type function :read-only t)
  (let                #'identity :type function :read-only t)
  (dynamic-let        #'identity :type function :read-only t)
  (lisp               #'identity :type function :read-only t)
  (match-branch       #'identity :type function :read-only t)
  (match              #'identity :type function :read-only t)
  (catch-branch       #'identity :type function :read-only t)
  (catch              #'identity :type function :read-only t)
  (progn              #'identity :type function :read-only t)
  (unsafe             #'identity :type function :read-only t)
  (block-node         #'identity :type function :read-only t)
  (return-from-node   #'identity :type function :read-only t)
  (values             #'identity :type function :read-only t)
  (throw              #'identity :type function :read-only t)
  (resume-to          #'identity :type function :read-only t)
  (resumable-branch   #'identity :type function :read-only t)
  (resumable          #'identity :type function :read-only t)
  (application        #'identity :type function :read-only t)
  (or                 #'identity :type function :read-only t)
  (and                #'identity :type function :read-only t)
  (if                 #'identity :type function :read-only t)
  (when               #'identity :type function :read-only t)
  (unless             #'identity :type function :read-only t)
  (cond-clause        #'identity :type function :read-only t)
  (cond               #'identity :type function :read-only t)
  (loop               #'identity :type function :read-only t)
  (break              #'identity :type function :read-only t)
  (continue           #'identity :type function :read-only t)
  (do-bind            #'identity :type function :read-only t)
  (do                 #'identity :type function :read-only t))

(defgeneric traverse (node block)
  (:method ((node node-variable) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall (traverse-variable block) node))

  (:method ((node node-accessor) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall (traverse-accessor block) node))

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
      :location (source:location node))))

  (:method ((node node-values-bind) block)
    (declare (type traverse-block block)
             (values node-values-bind &optional))

    (funcall
     (traverse-values-bind block)
     (make-node-values-bind
      :patterns (node-values-bind-patterns node)
      :expr (traverse (node-values-bind-expr node) block)
      :location (source:location node))))

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
      :location (source:location node)
      :params (node-abstraction-params node)
      :keyword-params (node-abstraction-keyword-params node)
      :body (traverse (node-abstraction-body node) block))))

  (:method ((node node-let-binding) block)
    (declare (type traverse-block block)
             (values node-let-binding &optional))

    (funcall
     (traverse-let-binding block)
     (make-node-let-binding
      :name (node-let-binding-name node)
      :value (traverse (node-let-binding-value node) block)
      :location (source:location node))))

  (:method ((node node-dynamic-binding) block)
    (declare (type traverse-block block)
             (values node-dynamic-binding &optional))

    (funcall
     (traverse-dynamic-binding block)
     (make-node-dynamic-binding
      :name (node-dynamic-binding-name node)
      :value (traverse (node-dynamic-binding-value node) block)
      :location (source:location node))))

  (:method ((node node-for-binding) block)
    (declare (type traverse-block block)
             (values node-for-binding &optional))

    (make-node-for-binding
     :name (node-for-binding-name node)
     :init (traverse (node-for-binding-init node) block)
     :step (traverse (node-for-binding-step node) block)
     :location (source:location node)))

  (:method ((node node-let) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-let block)
     (make-node-let
      :type (node-type node)
      :location (source:location node)
      :bindings (traverse (node-let-bindings node) block)
      :body (traverse (node-let-body node) block))))

  (:method ((node node-dynamic-let) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-dynamic-let block)
     (make-node-dynamic-let
      :type (node-type node)
      :location (source:location node)
      :bindings (traverse (node-dynamic-let-bindings node) block)
      :subexpr (traverse (node-dynamic-let-subexpr node) block))))

  (:method ((node node-lisp) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-lisp block)
     (make-node-lisp
      :type (node-type node)
      :location (source:location node)
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
      :location (source:location node))))

  (:method ((node node-match) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-match block)
     (make-node-match
      :type (node-type node)
      :location (source:location node)
      :expr (traverse (node-match-expr node) block)
      :branches (traverse (node-match-branches node) block))))

  (:method ((node node-catch-branch) block)
    (declare (type traverse-block block)
             (values node-catch-branch &optional))

    (funcall
     (traverse-catch-branch block)
     (make-node-catch-branch
      :pattern (node-catch-branch-pattern node)
      :body (traverse (node-catch-branch-body node) block)
      :location (source:location node))))

  (:method ((node node-catch) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-catch block)
     (make-node-catch
      :type (node-type node)
      :location (source:location node)
      :expr (traverse (node-catch-expr node) block)
      :branches (traverse (node-catch-branches node) block))))

  (:method ((node node-resumable-branch) block)
    (declare (type traverse-block block)
             (values node-resumable-branch &optional))

    (funcall
     (traverse-resumable-branch block)
     (make-node-resumable-branch
      :pattern (node-resumable-branch-pattern node)
      :body (traverse (node-resumable-branch-body node) block)
      :location (source:location node))))

  (:method ((node node-resumable) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-resumable block)
     (make-node-resumable
      :type (node-type node)
      :location (source:location node)
      :expr (traverse (node-resumable-expr node) block)
      :branches (traverse (node-resumable-branches node) block))))


  (:method ((node node-progn) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-progn block)
     (make-node-progn
      :type (node-type node)
      :location (source:location node)
      :body (traverse (node-progn-body node) block))))

  (:method ((node node-unsafe) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-unsafe block)
     (make-node-unsafe
      :type (node-type node)
      :location (source:location node)
      :body (traverse (node-unsafe-body node) block))))

  (:method ((node node-block) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-block-node block)
     (make-node-block
      :type (node-type node)
      :location (source:location node)
      :name (node-block-name node)
      :body (traverse (node-block-body node) block))))

  (:method ((node node-return-from) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-return-from-node block)
     (make-node-return-from
      :type (node-type node)
      :location (source:location node)
      :name (node-return-from-name node)
      :expr (traverse (node-return-from-expr node) block))))

  (:method ((node node-values) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-values block)
     (make-node-values
      :type (node-type node)
      :location (source:location node)
      :nodes (traverse (node-values-nodes node) block))))

  (:method ((node node-throw) block)
    (declare (type traverse-block block)
             (values node &optional))
    (funcall 
     (traverse-throw block)
     (make-node-throw
      :type (node-type node)
      :location (source:location node)
      :expr (traverse (node-throw-expr node) block))))

  (:method ((node node-resume-to) block)
    (declare (type traverse-block block)
             (values node &optional))
    (funcall 
     (traverse-resume-to block)
     (make-node-resume-to
      :type (node-type node)
      :location (source:location node)
      :expr (traverse (node-resume-to-expr node) block))))

  (:method ((node node-application) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-application block)
     (make-node-application
      :type (node-type node)
      :location (source:location node)
      :rator (traverse (node-application-rator node) block)
      :rands (traverse (node-application-rands node) block)
      :keyword-rands (traverse (node-application-keyword-rands node) block))))

  (:method ((node node-application-keyword-arg) block)
    (declare (type traverse-block block)
             (values node-application-keyword-arg &optional))

    (make-node-application-keyword-arg
     :keyword (node-application-keyword-arg-keyword node)
     :value (traverse (node-application-keyword-arg-value node) block)))

  (:method ((node node-or) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-or block)
     (make-node-or
      :type (node-type node)
      :location (source:location node)
      :nodes (traverse (node-or-nodes node) block))))

  (:method ((node node-and) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-and block)
     (make-node-and
      :type (node-type node)
      :location (source:location node)
      :nodes (traverse (node-and-nodes node) block))))

  (:method ((node node-if) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-if block)
     (make-node-if
      :type (node-type node)
      :location (source:location node)
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
      :location (source:location node)
      :expr (traverse (node-when-expr node) block)
      :body (traverse (node-when-body node) block))))

  (:method ((node node-unless) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-unless block)
     (make-node-unless
      :type (node-type node)
      :location (source:location node)
      :expr (traverse (node-unless-expr node) block)
      :body (traverse (node-unless-body node) block))))

  (:method ((node node-cond-clause) block)
    (declare (type traverse-block block)
             (values node-cond-clause &optional))

    (funcall
     (traverse-cond-clause block)
     (make-node-cond-clause
      :expr (traverse (node-cond-clause-expr node) block)
      :body (traverse (node-cond-clause-body node) block)
      :location (source:location node))))

  (:method ((node node-cond) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-cond block)
     (make-node-cond
      :type (node-type node)
      :location (source:location node)
      :clauses (traverse (node-cond-clauses node) block))))

  (:method ((node node-for) block)
    (declare (type traverse-block block)
             (values node &optional))
    (funcall
     (traverse-loop block)
     (make-node-for
      :type (node-type node)
      :location (source:location node)
      :label (node-for-label node)
      :bindings (traverse (node-for-bindings node) block)
      :sequential-p (node-for-sequential-p node)
      :returns (and (node-for-returns node)
                    (traverse (node-for-returns node) block))
      :termination-kind (node-for-termination-kind node)
      :termination-expr (and (node-for-termination-expr node)
                             (traverse (node-for-termination-expr node) block))
      :body (traverse (node-for-body node) block))))

  (:method ((node node-break) block)
    (declare (type traverse-block block)
             (values node &optional))
    (funcall
     (traverse-break block)
     node))

  (:method ((node node-continue) block)
    (declare (type traverse-block block)
             (values node &optional))
    (funcall
     (traverse-continue block)
     node))

  (:method ((node node-do-bind) block)
    (declare (type traverse-block block)
             (values node-do-bind &optional))

    (funcall
     (traverse-do-bind block)
     (make-node-do-bind
      :pattern (node-do-bind-pattern node)
      :expr (traverse (node-do-bind-expr node) block)
      :location (source:location node))))

  (:method ((node node-do) block)
    (declare (type traverse-block block)
             (values node &optional))

    (funcall
     (traverse-do block)
     (make-node-do
      :type (node-type node)
      :location (source:location node)
      :nodes (traverse (node-do-nodes node) block)
      :last-node (traverse (node-do-last-node node) block))))

  (:method ((list list) block)
    (declare (type traverse-block block)
             (values list))

    (loop :for node :in list
          :collect (traverse node block))))
