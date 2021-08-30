(in-package #:coalton-impl/codegen)

(defun match-constructor-lift-transform (node optimizer)
  (declare (type typed-node node)
	   (type optimizer optimizer))
  (match-constructor-lift node (optimizer-env optimizer)))

(defgeneric match-constructor-lift (node env)
  (:documentation "Mark variables which are constructed in a match
  expression as dynamic-extent to avoid unnecissary allocation.")

  (:method ((node typed-node-literal) env)
    (declare (type environment env)
	     (ignore env))
    node)

  (:method ((node typed-node-variable) env)
    (declare (type environment env)
	     (ignore env))
    node)

  (:method ((node typed-node-application) env)
    (declare (type environment env))
    (typed-node-application
     (typed-node-type node)
     (typed-node-unparsed node)
     (match-constructor-lift (typed-node-application-rator node) env)
     (mapcar
      (lambda (rand)
	(match-constructor-lift rand env))
      (typed-node-application-rands node))))

  (:method ((node typed-node-direct-application) env)
    (typed-node-direct-application
     (typed-node-type node)
     (typed-node-unparsed node)
     (typed-node-direct-application-rator-type node)
     (typed-node-direct-application-rator node)
     (mapcar
      (lambda (rand)
	(match-constructor-lift rand env))
      (typed-node-direct-application-rands node))))

  (:method ((node typed-node-abstraction) env)
    (declare (type environment env))
    (typed-node-abstraction
     (typed-node-type node)
     (typed-node-unparsed node)
     (typed-node-abstraction-vars node)
     (match-constructor-lift (typed-node-abstraction-subexpr node) env)
     (typed-node-abstraction-name-map node)))

  (:method ((node typed-node-let) env)
    (declare (type environment env))
    (typed-node-let
     (typed-node-type node)
     (typed-node-unparsed node)
     (mapcar
      (lambda (b)
	(cons (car b)
	      (match-constructor-lift (cdr b) env)))
      (typed-node-let-bindings node))
     (match-constructor-lift (typed-node-let-subexpr node) env)
     (typed-node-let-sorted-bindings node)
     (typed-node-let-dynamic-extent-bindings node)
     (typed-node-let-name-map node)))

  (:method ((node typed-node-lisp) env)
    (declare (type environment env)
	     (ignore env))
    node)

  (:method ((branch typed-match-branch) env)
    (declare (type environment env))
    (typed-match-branch
     (typed-match-branch-unparsed branch)
     (typed-match-branch-pattern branch)
     (match-constructor-lift (typed-match-branch-subexpr branch) env)
     (typed-match-branch-bindings branch)
     (typed-match-branch-name-map branch)))

  (:method ((node typed-node-match) env)
    (declare (type environment env))
    (let ((expr (typed-node-match-expr node)))

      (if (and
	   ;; The function is fully applied
	   (coalton-impl/typechecker::typed-node-direct-application-p expr)

	   ;; The function is a constructor
	   (lookup-constructor env (typed-node-direct-application-rator expr) :no-error t)

	   ;; The constructed value is not captured by a variable pattern
	   (notany
	    #'coalton-impl/ast::pattern-var-p
	    (mapcar #'typed-match-branch-pattern
		    (typed-node-match-branches node))))
	  (generate-lifted-constructor node)

	  (typed-node-match
	   (typed-node-type node)
	   (typed-node-unparsed node)
	   (match-constructor-lift expr env)
	   (mapcar
	    (lambda (b)
	      (match-constructor-lift b env))
	    (typed-node-match-branches node))))))

  (:method ((node typed-node-seq) env)
    (declare (type environment env))
    (typed-node-seq
     (typed-node-type node)
     (typed-node-unparsed node)
     (mapcar
      (lambda (subnode)
	(match-constructor-lift subnode env))
      (typed-node-seq-subnodes node)))))


(defun generate-lifted-constructor (node)
  (declare (type typed-node-match node))
  (let ((sym (gensym))
	(expr (typed-node-match-expr node)))
    (typed-node-let
     (typed-node-type node)
     '@@SOURCE-UNKNOWN@@
     (list (cons sym expr))
     (typed-node-match
      (typed-node-type node)
      (typed-node-unparsed node)
      (typed-node-variable (typed-node-type expr) (typed-node-unparsed expr) sym)
      (typed-node-match-branches node))
     (list (list sym))
     (list sym)
     nil)))

