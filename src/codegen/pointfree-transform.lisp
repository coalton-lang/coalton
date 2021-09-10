(in-package #:coalton-impl/codegen)

(defun pointfree-transform (node optimizer)
  (pointfree node (optimizer-env optimizer)))

(defgeneric pointfree (node env)
  (:documentation "Transform pointfree definitions to their equivelent pointful style to reduce currying at runtime.")
  (:method ((node typed-node-variable) env)
    (let*  ((qualified-type (coalton-impl/typechecker::fresh-inst (typed-node-type node)))
            (type (coalton-impl/typechecker::qualified-ty-type qualified-type))
            (arguments (coalton-impl/typechecker::function-type-arguments type))
            (named-argument-schemes
              (loop :for arg :in arguments
                    :collect (cons
                              (gensym)
                              (coalton-impl/typechecker::to-scheme
                               (coalton-impl/typechecker::qualify nil arg)))))
            (named-argument-nodes
              (loop :for (name . type) :in named-argument-schemes
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
       named-argument-schemes
       (typed-node-application
        (coalton-impl/typechecker::to-scheme
         (coalton-impl/typechecker::qualify
          nil
          (coalton-impl/typechecker::function-return-type type)))
        '@@SOURCE-UNKNOWN@@
        node
        named-argument-nodes)
       nil)))

  (:method ((node typed-node-abstraction) env)
    (unless (coalton-impl/typechecker::typed-node-application-p
             (typed-node-abstraction-subexpr node))
      (return-from pointfree node))

    (unless (function-type-p (typed-node-type (typed-node-abstraction-subexpr node)))
      (return-from pointfree node))


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
             :for arg :in arguments
             :collect (cons arg arg))
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
        (typed-node-application-rator subexpr)
        (append
         (typed-node-application-rands subexpr)
         argument-nodes))
       (append
        (typed-node-abstraction-name-map node)
        argument-name-map))))

  (:method (node env)
    (declare (ignore env))
    node))

