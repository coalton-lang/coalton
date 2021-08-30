(in-package #:coalton-impl/typechecker)

(defun check-variable-node-types (bindings)
  (loop :for (name . node) :in bindings
	:do (check-variables node (type-variables (typed-node-type node)))))

(defgeneric check-variables (node variables)
  (:method ((node typed-node-literal) variables)
    (declare (type typed-node node))
    (unless (subsetp (type-variables (typed-node-type node))
		     variables
		     :test #'equalp)
      (error 'unknown-type-variable
	     :node node
	     :variables variables))
    nil)

  (:method ((node typed-node-variable) variables)
    (declare (type list variables))
    (unless (subsetp (type-variables (typed-node-type node))
		     variables
		     :test #'equalp)
      (error 'unknown-type-variable
	     :node node
	     :variables variables))
    nil)

  (:method ((node typed-node-application) variables)
    (declare (type list variables))
    (unless (subsetp (type-variables (typed-node-type node))
		     variables
		     :test #'equalp)
      (error 'unknown-type-variable
	     :node node
	     :variables variables))

    (check-variables (typed-node-application-rator node) variables)

    (dolist (rand (typed-node-application-rands node))
      (check-variables rand variables))
    nil)

  (:method ((node typed-node-direct-application) variables)
    (declare (type list variables))
    (unless (subsetp (type-variables (typed-node-type node))
		     variables
		     :test #'equalp)
      (error 'unknown-type-variable
	     :node node
	     :variables variables))

    (dolist (rand (typed-node-direct-application-rands node))
      (check-variables rand variables))
    nil)

  (:method ((node typed-node-abstraction) variables)
    (declare (type list variables))
    (unless (subsetp (type-variables (typed-node-type node))
		     variables
		     :test #'equalp)
      (error 'unknown-type-variable
	     :node node
	     :variables variables))

    (check-variables (typed-node-abstraction-subexpr node) variables)
    nil)

  (:method ((node typed-node-let) variables)
    (declare (type list variables))
    (unless (subsetp (type-variables (typed-node-type node))
		     variables
		     :test #'equalp)
      (error 'unknown-type-variable
	     :node node
	     :variables variables))

    (loop :for (name . node) :in (typed-node-let-bindings node)
	  :do
             (setf variables
	           (append variables (type-variables (typed-node-type node))))
             (check-variables
	       node
	       variables))

    (check-variables (typed-node-let-subexpr node) variables)
    nil)

  (:method ((node typed-node-lisp) variables)
    (declare (type list variables))
    (unless (subsetp (type-variables (typed-node-type node))
		     variables
		     :test #'equalp)
      (error 'unknown-type-variable
	     :node node
	     :variables variables))
    nil)

  (:method ((node typed-node-match) variables)
    (declare (type list variables))
    (unless (subsetp (type-variables (typed-node-type node))
		     variables
		     :test #'equalp)
      (error 'unknown-type-variable
	     :node node
	     :variables variables))

    (dolist (branch (typed-node-match-branches node))
      (check-variables branch variables))
    nil)

  (:method ((node typed-node-seq) variables)
    (declare (type list variables))
    (unless (subsetp (type-variables node)
		     variables
		     :test #'equalp)
      (error 'unknown-type-variable
	     :node node
	     :variables variables))

    (dolist (subnode (typed-node-seq-subnodes node))
      (check-variables subnode variables)))

  (:method ((branch typed-match-branch) variables)
    (declare (type list variables))
    (check-variables (typed-match-branch-subexpr branch) variables)
    nil))
 
