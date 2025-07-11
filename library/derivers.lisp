(defpackage #:coalton-library/derivers
  (:use #:cl)
  (:local-nicknames
   (#:classes #:coalton-library/classes)
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:derive #:coalton-impl/typechecker/derive)))

(in-package #:coalton-library/derivers)

(defmethod derive:derive-methods ((class (eql 'classes:eq)) def env)
  "Deriver implementation for class `Eq'."
  (let ((location (source:location def)))
    (list
     (parser:make-instance-method-definition
      :name (parser:make-node-variable
             :location location
             :name 'classes:==)
      :params (list
               (parser:make-pattern-var
                :location location
                :name 'a
                :orig-name 'a)
               (parser:make-pattern-var
                :location location
                :name 'b
                :orig-name 'b))
      :body (parser:make-node-body
             :nodes nil
             :last-node (parser:make-node-match
                         :location location
                         :expr (parser:make-node-application
                                :location location
                                :rator (parser:make-node-variable
                                        :location location
                                        :name 'classes:Tuple)
                                :rands (list
                                        (parser:make-node-variable
                                         :location location
                                         :name 'a)
                                        (parser:make-node-variable
                                         :location location
                                         :name 'b)))
                         :branches (append
                                    (mapcar
                                     (lambda (ctor)
                                       (let ((cfields-a
                                               (mapcar (lambda (_)
                                                         (declare (ignore _))
                                                         (gensym "ctor-field"))
                                                       (parser:type-definition-ctor-field-types ctor)))
                                             (cfields-b
                                               (mapcar (lambda (_)
                                                         (declare (ignore _))
                                                         (gensym "ctor-field"))
                                                       (parser:type-definition-ctor-field-types ctor))))
                                         (parser:make-node-match-branch
                                          :location location
                                          :pattern (parser:make-pattern-constructor
                                                    :location location
                                                    :name 'classes:Tuple
                                                    :patterns (list
                                                               (parser:make-pattern-constructor
                                                                :location location
                                                                :name (parser:identifier-src-name (parser:type-definition-ctor-name ctor))
                                                                :patterns (mapcar
                                                                           (lambda (cfield)
                                                                             (parser:make-pattern-var
                                                                              :location location
                                                                              :name cfield
                                                                              :orig-name cfield))
                                                                           cfields-a))
                                                               (parser:make-pattern-constructor
                                                                :location location
                                                                :name (parser:identifier-src-name (parser:type-definition-ctor-name ctor))
                                                                :patterns (mapcar
                                                                           (lambda (cfield)
                                                                             (parser:make-pattern-var
                                                                              :location location
                                                                              :name cfield
                                                                              :orig-name cfield))
                                                                           cfields-b))))
                                          :body (parser:make-node-body
                                                 :nodes nil
                                                 :last-node (parser:make-node-and
                                                             :location location
                                                             :nodes (append
                                                                     (mapcar
                                                                      (lambda (cfield-a cfield-b)
                                                                        (parser:make-node-application
                                                                         :location location
                                                                         :rator (parser:make-node-variable
                                                                                 :location location
                                                                                 :name 'classes:==)
                                                                         :rands (list
                                                                                 (parser:make-node-variable
                                                                                  :location location
                                                                                  :name cfield-a)
                                                                                 (parser:make-node-variable
                                                                                  :location location
                                                                                  :name cfield-b))))
                                                                      cfields-a
                                                                      cfields-b)
                                                                     (list
                                                                      (parser:make-node-variable
                                                                       :location location
                                                                       :name 'coalton:True))))))))
                                     (parser:type-definition-ctors def))
                                    (if (= 1 (length (parser:type-definition-ctors def)))
                                        nil
                                        (list
                                         (parser:make-node-match-branch
                                          :location location
                                          :pattern (parser:make-pattern-wildcard
                                                    :location location)
                                          :body (parser:make-node-body
                                                 :nodes nil
                                                 :last-node (parser:make-node-variable
                                                             :location location
                                                             :name 'coalton:False))))))))
      :location location
      :inline nil))))
