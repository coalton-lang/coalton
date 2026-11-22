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
   #:visit                              ; FUNCTION
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
  (protect            #'identity :type function :read-only t)
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
  (:documentation "Rewrite NODE bottom-up using BLOCK. Callbacks receive the rebuilt
node (the original for leaves) and return its replacement.")
  (:method ((nodes list) block)
    (declare (type traverse-block block)
             (values list))
    (loop :for node :in nodes :collect (traverse node block))))

(defgeneric visit (node block)
  (:documentation "Visit the original nodes in TRAVERSE order without rebuilding the tree.
Callback results are ignored. Returns no values.")
  (:method ((nodes list) block)
    (declare (type traverse-block block))
    (dolist (node nodes (values))
      (visit node block))))

(defmacro define-traversal ((type callback &optional (result-type 'node))
                            &optional constructor &rest children)
  "CONSTRUCTOR names the constructor and the initarg/reader pairs copied verbatim.
CHILDREN lists the remaining pairs in traversal order. The list methods also
handle absent (NIL) children.
Leaves need no constructor; wrapper records need no callback."
  (let ((rebuilt
          (if constructor
              `(,(first constructor)
                ,@(loop :for (key reader) :on (rest constructor) :by #'cddr
                        :append (list key `(,reader node)))
                ,@(loop :for (key reader) :on children :by #'cddr
                        :append (list key `(traverse (,reader node) block))))
              'node)))
    `(progn
       (defmethod traverse ((node ,type) block)
         (declare (type traverse-block block)
                  (values ,result-type &optional))
         ,(if callback `(funcall (,callback block) ,rebuilt) rebuilt))
       (defmethod visit ((node ,type) block)
         (declare (type traverse-block block))
         ,@(loop :for (nil reader) :on children :by #'cddr
                 :collect `(visit (,reader node) block))
         ,@(when callback `((funcall (,callback block) node)))
         (values)))))

(define-traversal (node-variable traverse-variable))
(define-traversal (node-accessor traverse-accessor))
(define-traversal (node-literal traverse-literal))
(define-traversal (node-integer-literal traverse-integer-literal))

(define-traversal (node-bind traverse-bind node-bind)
  (make-node-bind :pattern node-bind-pattern :location source:location)
  :expr node-bind-expr)

(define-traversal (node-values-bind traverse-values-bind node-values-bind)
  (make-node-values-bind :patterns node-values-bind-patterns :location source:location)
  :expr node-values-bind-expr)

(define-traversal (node-body traverse-body node-body)
  (make-node-body)
  :nodes node-body-nodes
  :last-node node-body-last-node)

(define-traversal (node-abstraction traverse-abstraction)
  (make-node-abstraction :type node-type :location source:location
                         :params node-abstraction-params :keyword-params node-abstraction-keyword-params)
  :body node-abstraction-body)

(define-traversal (node-let-binding traverse-let-binding node-let-binding)
  (make-node-let-binding :name node-let-binding-name :location source:location)
  :value node-let-binding-value)

(define-traversal (node-dynamic-binding traverse-dynamic-binding node-dynamic-binding)
  (make-node-dynamic-binding :name node-dynamic-binding-name :location source:location)
  :value node-dynamic-binding-value)

(define-traversal (node-for-binding nil node-for-binding)
  (make-node-for-binding :name node-for-binding-name :location source:location)
  :init node-for-binding-init
  :step node-for-binding-step)

(define-traversal (node-let traverse-let)
  (make-node-let :type node-type :location source:location)
  :bindings node-let-bindings
  :body node-let-body)

(define-traversal (node-dynamic-let traverse-dynamic-let)
  (make-node-dynamic-let :type node-type :location source:location)
  :bindings node-dynamic-let-bindings
  :subexpr node-dynamic-let-subexpr)

(define-traversal (node-lisp traverse-lisp)
  (make-node-lisp :type node-type :location source:location
                  :var-names node-lisp-var-names :body node-lisp-body)
  :vars node-lisp-vars)

(define-traversal (node-match-branch traverse-match-branch node-match-branch)
  (make-node-match-branch :pattern node-match-branch-pattern :location source:location)
  :body node-match-branch-body)

(define-traversal (node-match traverse-match)
  (make-node-match :type node-type :location source:location)
  :expr node-match-expr
  :branches node-match-branches)

(define-traversal (node-catch-branch traverse-catch-branch node-catch-branch)
  (make-node-catch-branch :exception-type node-catch-branch-exception-type
                          :pattern node-catch-branch-pattern :location source:location)
  :body node-catch-branch-body)

(define-traversal (node-catch traverse-catch)
  (make-node-catch :type node-type :location source:location :in-place-p node-catch-in-place-p)
  :expr node-catch-expr
  :branches node-catch-branches)

(define-traversal (node-resumable-branch traverse-resumable-branch node-resumable-branch)
  (make-node-resumable-branch :pattern node-resumable-branch-pattern :location source:location)
  :body node-resumable-branch-body)

(define-traversal (node-resumable traverse-resumable)
  (make-node-resumable :type node-type :location source:location)
  :expr node-resumable-expr
  :branches node-resumable-branches)

(define-traversal (node-progn traverse-progn)
  (make-node-progn :type node-type :location source:location)
  :body node-progn-body)

(define-traversal (node-unsafe traverse-unsafe)
  (make-node-unsafe :type node-type :location source:location)
  :body node-unsafe-body)

(define-traversal (node-protect traverse-protect)
  (make-node-protect :type node-type :location source:location)
  :expr node-protect-expr
  :cleanup node-protect-cleanup)

(define-traversal (node-block traverse-block-node)
  (make-node-block :type node-type :location source:location :name node-block-name)
  :body node-block-body)

(define-traversal (node-return-from traverse-return-from-node)
  (make-node-return-from :type node-type :location source:location :name node-return-from-name)
  :expr node-return-from-expr)

(define-traversal (node-values traverse-values)
  (make-node-values :type node-type :location source:location)
  :nodes node-values-nodes)

(define-traversal (node-throw traverse-throw)
  (make-node-throw :type node-type :location source:location)
  :expr node-throw-expr)

(define-traversal (node-resume-to traverse-resume-to)
  (make-node-resume-to :type node-type :location source:location)
  :expr node-resume-to-expr)

(define-traversal (node-application traverse-application)
  (make-node-application :type node-type :location source:location)
  :rator node-application-rator
  :rands node-application-rands
  :keyword-rands node-application-keyword-rands)

(define-traversal (node-application-keyword-arg nil node-application-keyword-arg)
  (make-node-application-keyword-arg :keyword node-application-keyword-arg-keyword)
  :value node-application-keyword-arg-value)

(define-traversal (node-or traverse-or)
  (make-node-or :type node-type :location source:location)
  :nodes node-or-nodes)

(define-traversal (node-and traverse-and)
  (make-node-and :type node-type :location source:location)
  :nodes node-and-nodes)

(define-traversal (node-if traverse-if)
  (make-node-if :type node-type :location source:location)
  :expr node-if-expr
  :then node-if-then
  :else node-if-else)

(define-traversal (node-when traverse-when)
  (make-node-when :type node-type :location source:location)
  :expr node-when-expr
  :body node-when-body)

(define-traversal (node-unless traverse-unless)
  (make-node-unless :type node-type :location source:location)
  :expr node-unless-expr
  :body node-unless-body)

(define-traversal (node-cond-clause traverse-cond-clause node-cond-clause)
  (make-node-cond-clause :location source:location)
  :expr node-cond-clause-expr
  :body node-cond-clause-body)

(define-traversal (node-cond traverse-cond)
  (make-node-cond :type node-type :location source:location)
  :clauses node-cond-clauses)

(define-traversal (node-for traverse-loop)
  (make-node-for :type node-type :location source:location
                 :label node-for-label :sequential-p node-for-sequential-p
                 :termination-kind node-for-termination-kind)
  :bindings node-for-bindings
  :returns node-for-returns
  :termination-expr node-for-termination-expr
  :body node-for-body)

(define-traversal (node-break traverse-break))
(define-traversal (node-continue traverse-continue))

(define-traversal (node-do-bind traverse-do-bind node-do-bind)
  (make-node-do-bind :pattern node-do-bind-pattern :location source:location)
  :expr node-do-bind-expr)

(define-traversal (node-do traverse-do)
  (make-node-do :type node-type :location source:location)
  :nodes node-do-nodes
  :last-node node-do-last-node)
