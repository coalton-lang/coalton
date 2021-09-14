(in-package #:coalton-impl/codegen)

(defun direct-application-transform (node optimizer)
  (direct-application node
                      (optimizer-toplevel-functions optimizer)
                      (optimizer-env optimizer)))

(defgeneric direct-application (node inlineable-functions env)
  (:documentation "Mark fully applied function calls to statically
  known functions as such to avoid symbol lookups and the curring
  machinery. This also enables further optimisations.")

  (:method ((node typed-node-literal) inlineable-functions env)
    (declare (type list inlineable-functions)
             (type environment env)
             (values typed-node)
             (ignore inlineable-functions env))
    node)

  (:method ((node typed-node-variable) inlineable-functions env)
    (declare (type list inlineable-functions)
             (type environment env)
             (values typed-node)
             (ignore inlineable-functions env))
    node)

  (:method ((node typed-node-application) inlineable-functions env)
    (declare (type list inlineable-functions)
             (type environment env)
             (values typed-node))

    (let ((f (typed-node-application-rator node)))
      (etypecase (typed-node-application-rator node)
        (typed-node-variable
         (if (and (member (typed-node-variable-name f) (mapcar #'car inlineable-functions))
                  (= (length (typed-node-application-rands node))
                     (cdr (assoc (typed-node-variable-name f) inlineable-functions))))
             (typed-node-direct-application
              (typed-node-type node)
              (typed-node-unparsed node)
              (typed-node-type f)
              (typed-node-variable-name f)
              (mapcar (lambda (expr) (direct-application expr inlineable-functions env))
                      (typed-node-application-rands node)))

             (typed-node-application
              (typed-node-type node)
              (typed-node-unparsed node)
              (typed-node-application-rator node)
              (mapcar (lambda (expr) (direct-application expr inlineable-functions env))
                      (typed-node-application-rands node)))))

        (t (typed-node-application
            (typed-node-type node)
            (typed-node-unparsed node)
            (typed-node-application-rator node)
            (mapcar (lambda (expr) (direct-application expr inlineable-functions env))
                    (typed-node-application-rands node)))))))

  (:method ((node typed-node-direct-application) inlineable-functions env)
    (declare (type list inlineable-functions)
             (type environment env))
    (typed-node-direct-application
     (typed-node-type node)
     (typed-node-unparsed node)
     (typed-node-direct-application-rator-type node)
     (typed-node-direct-application-rator node)
     (mapcar
      (lambda (node)
        (direct-application node inlineable-functions env))
      (typed-node-direct-application-rands node))))

  (:method ((node typed-node-abstraction) inlineable-functions env)
    (declare (type list inlineable-functions)
             (type environment env)
             (values typed-node))

    (let ((new-inlineable-functions
            (remove-inlinable-functions
             inlineable-functions
             (mapcar #'car (typed-node-abstraction-vars node)))))

      (typed-node-abstraction
       (typed-node-type node)
       (typed-node-unparsed node)
       (typed-node-abstraction-vars node)
       (direct-application
        (typed-node-abstraction-subexpr node)
        new-inlineable-functions
        env)
       (typed-node-abstraction-name-map node))))

  (:method ((node typed-node-let) inlineable-functions env)
    (declare (type list inlineable-functions)
             (type environment env)
             (values typed-node))

    (let ((new-inlineable-functions (update-inlineable-functions inlineable-functions node)))
      (typed-node-let
       (typed-node-type node)
       (typed-node-unparsed node)
       (mapcar
        (lambda (binding)
          (cons
           (car binding)
           (direct-application (cdr binding) new-inlineable-functions env)))
        (typed-node-let-bindings node))
       (direct-application (typed-node-let-subexpr node) new-inlineable-functions  env)
       (typed-node-let-sorted-bindings node)
       (typed-node-let-dynamic-extent-bindings node)
       (typed-node-let-name-map node))))

  (:method ((node typed-node-lisp) inlineable-functions env)
    (declare (type list inlineable-functions)
             (type environment env)
             (values typed-node)
             (ignore inlineable-functions))
    node)

  (:method ((node typed-node-match) inlineable-functions env)
    (declare (type list inlineable-functions)
             (type environment env)
             (values typed-node))

    (typed-node-match
     (typed-node-type node)
     (typed-node-unparsed node)
     (direct-application (typed-node-match-expr node) inlineable-functions env)
     (mapcar (lambda (branch) (direct-application branch inlineable-functions env))
             (typed-node-match-branches node))))

  (:method ((branch typed-match-branch) inlineable-functions env)
    (declare (type list inlineable-functions)
             (type environment env)
             (values typed-match-branch))

    (let* ((p-vars (pattern-variables (typed-match-branch-pattern branch)))
           (new-inlineable-functions (remove-inlinable-functions inlineable-functions p-vars)))
      (typed-match-branch
       (typed-match-branch-unparsed branch)
       (typed-match-branch-pattern branch)
       (direct-application (typed-match-branch-subexpr branch) new-inlineable-functions env)
       (typed-match-branch-bindings branch)
       (typed-match-branch-name-map branch))))

  (:method ((node typed-node-seq) inlineable-functions env)
    (declare (type list inlineable-functions)
             (type environment env))
    (typed-node-seq
     (typed-node-type node)
     (typed-node-unparsed node)
     (mapcar
      (lambda (subnode)
        (direct-application subnode inlineable-functions env))
      (typed-node-seq-subnodes node))))

  (:method ((node typed-node-if) inlineable-functions env)
    (declare (type list inlineable-functions)
             (type environment env))
    (typed-node-if
     (typed-node-type node)
     (typed-node-unparsed node)
     (direct-application (typed-node-if-predicate node) inlineable-functions env)
     (direct-application (typed-node-if-true node) inlineable-functions env)
     (direct-application (typed-node-if-false node) inlineable-functions env))))


(defun update-inlineable-functions (inlineable-functions let-node)
  (declare (type list inlineable-functions)
           (type typed-node-let let-node)
           (values list))

  (multiple-value-bind (functions variables)
      (split-binding-definitions (typed-node-let-bindings let-node))
    (union functions (remove-inlinable-functions inlineable-functions variables) :key #'car)))

(defun remove-inlinable-functions (inlineable-functions variables)
  (declare (type list inlineable-functions)
           (type symbol-list variables))
  (remove-if
   (lambda (binding) (member (car binding) variables))
   inlineable-functions))

(defun split-binding-definitions (toplevel-bindings)
  (let ((functions nil)
        (variables nil))
    (loop :for (name . node) :in toplevel-bindings
          :for is_function
            := (coalton-impl/typechecker::typed-node-abstraction-p node)
          :do (if is_function
                (let ((arity (length (typed-node-abstraction-vars node))))
                  (push (cons name arity) functions))
                (push name variables)))
    (values
     functions
     variables)))
