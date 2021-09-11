(in-package #:coalton-impl/codegen)

(defun pointfree-transform (node optimizer)
  (pointfree node (optimizer-env optimizer)))

(defgeneric pointfree (node env)
  (:documentation "Transform pointfree definitions to their equivelent
  pointful style to reduce currying at runtime. This transform is only
  run on toplevel bindings and let bindings.")
  (:method ((node typed-node-variable) env)
    (let*  ((qualified-type (coalton-impl/typechecker::fresh-inst (typed-node-type node)))
            (type (coalton-impl/typechecker::qualified-ty-type qualified-type))
            (arguments (coalton-impl/typechecker::function-type-arguments type))
            (argument-schemes
              (loop :for arg :in arguments
                    :collect (cons
                              (gensym)
                              (coalton-impl/typechecker::to-scheme
                               (coalton-impl/typechecker::qualify nil arg)))))
            (argument-name-map
              (loop :for arg :in arguments
                    :collect (cons arg arg)))
            (argument-nodes
              (loop :for (name . type) :in argument-schemes
                    :collect
                    (typed-node-variable
                     type
                     '@@SOURCE-UNKNOWN@@
                     name))))

      (unless (function-type-p type)
        (return-from pointfree node))

      (typed-node-abstraction
       (typed-node-type node)
       '@@SOURCE-UNKNOWN@@
       argument-schemes
       (typed-node-application
        (coalton-impl/typechecker::to-scheme
         (coalton-impl/typechecker::qualify
          nil
          (coalton-impl/typechecker::function-return-type type)))
        '@@SOURCE-UNKNOWN@@
        node
        argument-nodes)
       argument-name-map)))

  (:method ((node typed-node-abstraction) env)
    (unless (coalton-impl/typechecker::typed-node-application-p
             (typed-node-abstraction-subexpr node))
      (return-from pointfree
        (typed-node-abstraction
         (typed-node-type node)
         (typed-node-unparsed node)
         (typed-node-abstraction-vars node)
         (pointfree-traverse (typed-node-abstraction-subexpr node) env)
         (typed-node-abstraction-name-map node))))

    (unless (function-type-p (typed-node-type (typed-node-abstraction-subexpr node)))
      (return-from pointfree
        (typed-node-abstraction
         (typed-node-type node)
         (typed-node-unparsed node)
         (typed-node-abstraction-vars node)
         (pointfree-traverse (typed-node-abstraction-subexpr node) env)
         (typed-node-abstraction-name-map node))))


    (let* ((subexpr (typed-node-abstraction-subexpr node))
           (subexpr-scheme (typed-node-type subexpr))
           (subexpr-type
             (coalton-impl/typechecker::qualified-ty-type
              (coalton-impl/typechecker::fresh-inst subexpr-scheme)))
           (arguments (coalton-impl/typechecker::function-type-arguments subexpr-type))
           (argument-schemes
             (loop :for arg :in arguments
                   :collect (cons (gensym)
                                  (coalton-impl/typechecker::to-scheme
                                   (coalton-impl/typechecker::qualify nil arg)))))
           (argument-name-map
             (loop
               :for arg :in arguments
               :collect (cons arg arg)))
           (argument-nodes
             (loop :for (name . type) :in argument-schemes
                   :collect (typed-node-variable
                             type
                             '@@SOURCE-UNKNOWN@@
                             name))))

      (typed-node-abstraction
       (typed-node-type node)
       (typed-node-unparsed node)
       (append
        (typed-node-abstraction-vars node)
        argument-schemes)
       (typed-node-application
        (coalton-impl/typechecker::function-return-type subexpr-scheme)
        (typed-node-unparsed subexpr)
        (pointfree-traverse (typed-node-application-rator subexpr) node)
        (append
         (mapcar
          (lambda (node)
            (pointfree-traverse node env))
          (typed-node-application-rands subexpr))
         argument-nodes))
       (append
        (typed-node-abstraction-name-map node)
        argument-name-map))))

  (:method ((node typed-node-application) env)
    (unless (function-type-p (typed-node-type node))
      (return-from pointfree
        (typed-node-application
         (typed-node-type node)
         (typed-node-unparsed node)
         (pointfree-traverse (typed-node-application-rator node) env)
         (mapcar
          (lambda (node)
            (pointfree-traverse node env))
          (typed-node-application-rands node)))))

    (let* ((return-type
             (coalton-impl/typechecker::function-return-type (typed-node-type node)))
           (arguments
             (coalton-impl/typechecker::function-type-arguments
              (typed-node-type node)))
           (argument-schemes
             (loop :for arg :in arguments
                   :collect (cons (gensym)
                                  (coalton-impl/typechecker::to-scheme
                                   (coalton-impl/typechecker::qualify nil arg)))))
           (argument-name-map
             (loop
               :for arg :in arguments
               :collect (cons arg arg)))
           (argument-nodes
             (loop :for (name . type) :in argument-schemes
                   :collect (typed-node-variable
                             type
                             '@@SOURCE-UNKNOWN@@
                             name))))

      (typed-node-abstraction
       (typed-node-type node)
       '@@SOURCE-UNKNOWN@@
       argument-schemes
       (typed-node-application
        return-type
        (typed-node-unparsed node)
        (pointfree-traverse (typed-node-application-rator node) env)
        (append
         (mapcar
          (lambda (node)
            (pointfree-traverse node env))
          (typed-node-application-rands node))
         argument-nodes))
       argument-name-map)))

  (:method ((node typed-node) env)
    (pointfree-traverse node env)))

(defgeneric pointfree-traverse (node env)
  (:method ((node typed-node-application) env)
    (typed-node-application
     (typed-node-type node)
     (typed-node-unparsed node)
     (pointfree-traverse (typed-node-application-rator node) env)
     (mapcar
      (lambda (node)
        (pointfree-traverse node env))
      (typed-node-application-rands node))))

  (:method ((node typed-node-direct-application) env)
    (typed-node-direct-application
     (typed-node-type node)
     (typed-node-unparsed node)
     (typed-node-direct-application-rator-type node)
     (typed-node-direct-application-rator node)
     (mapcar
      (lambda (node)
        (pointfree-traverse node env))
      (typed-node-direct-application-rands node))))

  (:method ((node typed-node-abstraction) env)
    (typed-node-abstraction
     (typed-node-type node)
     (typed-node-unparsed node)
     (typed-node-abstraction-vars node)
     (pointfree-traverse (typed-node-abstraction-subexpr node) env)
     (typed-node-abstraction-name-map node)))

  (:method ((node typed-node-let) env)
    (typed-node-let
     (typed-node-type node)
     (typed-node-unparsed node)
     (loop :for (name . node) :in (typed-node-let-bindings node)
           :collect (cons name (pointfree node env)))
     (pointfree-traverse (typed-node-let-subexpr node) env)
     (typed-node-let-sorted-bindings node)
     (typed-node-let-dynamic-extent-bindings node)
     (typed-node-let-name-map node)))

  (:method ((node typed-node-match) env)
    (typed-node-match
     (typed-node-type node)
     (typed-node-unparsed node)
     (pointfree-traverse (typed-node-match-expr node) env)
     (mapcar
      (lambda (node)
        (pointfree-traverse node env))
      (typed-node-match-branches node))))

  (:method ((branch typed-match-branch) env)
    (typed-match-branch
     (typed-match-branch-unparsed branch)
     (typed-match-branch-pattern branch)
     (pointfree-traverse (typed-match-branch-subexpr branch) env)
     (typed-match-branch-bindings branch)
     (typed-match-branch-name-map branch)))

  (:method ((node typed-node-seq) env)
    (typed-node-seq
     (typed-node-type node)
     (typed-node-unparsed node)
     (mapcar
      (lambda (node)
        (pointfree-traverse node env))
      (typed-node-seq-subnodes node))))

  (:method ((node typed-node) env)
    (declare (ignore env))
    node))

