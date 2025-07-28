(defpackage #:coalton-library/derivers
  (:use #:cl)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:classes #:coalton-library/classes)
   (#:hash #:coalton-library/hash)))

(in-package #:coalton-library/derivers)

(defmethod tc:derive-methods ((class (eql 'classes:eq)) def env)
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

(defun hash-symbols (symbol &rest symbols)
  (flet ((hash-symbol (sym)
           (hash:lisp-combine-hashes
            (sxhash sym)
            (sxhash (package-name (symbol-package sym)))))) 
    (reduce #'hash:lisp-combine-hashes
            symbols
            :key #'hash-symbol
            :initial-value (hash-symbol symbol))))

(defmethod tc:derive-methods ((class (eql 'classes:hash)) def env)
  "Deriver implementation for class `Hash'.

The hashes generated are not guaranteed to be stable when the type is
redefined, since constructors are differentiated by their index in the
`define-type'. If you only append constructors, things will stay the
same, but if you insert, they will not.

The generated method will be shaped like this:

```
(define (hash x)
  (match x
    ((Ctor1)
     ,(hash-symbols 'TypeName 'Ctor1))
    ((Ctor2 field1 field2)
     (combine-hashes
      field2
      (combine-hashes
       field1
       ,(hash-symbols 'TypeName 'Ctor2))))))
'''"
  (let ((location (source:location def)))
    (list
     (parser:make-instance-method-definition
      :name (parser:make-node-variable
             :location location
             :name 'classes:hash)
      :params (list (parser:make-pattern-var
                     :location location
                     :name 'x
                     :orig-name 'x))
      :body (parser:make-node-body
             :nodes nil
             :last-node (parser:make-node-match
                         :location location
                         :expr (parser:make-node-variable
                                :location location
                                :name 'x)
                         :branches (loop :for index :from 0
                                         :for ctor :in (parser:type-definition-ctors def)
                                         :for cfields := (mapcar (lambda (_)
                                                                   (declare (ignore _))
                                                                   (gensym "ctor-field"))
                                                                 (parser:type-definition-ctor-field-types ctor))
                                         :for patterns := (mapcar (lambda (cfield)
                                                                    (parser:make-pattern-var
                                                                     :location location
                                                                     :name cfield
                                                                     :orig-name cfield))
                                                                  cfields)
                                         :collect (parser:make-node-match-branch
                                                   :location location
                                                   :pattern (parser:make-pattern-constructor
                                                             :location location
                                                             :name (parser:identifier-src-name (parser:type-definition-ctor-name ctor))
                                                             :patterns patterns)
                                                   :body (parser:make-node-body
                                                          :nodes nil
                                                          :last-node (reduce
                                                                      (lambda (acc cfield)
                                                                        (parser:make-node-application
                                                                         :location location
                                                                         :rator (parser:make-node-variable
                                                                                 :location location
                                                                                 :name 'coalton-library/hash:combine-hashes)
                                                                         :rands (list
                                                                                 (parser:make-node-application
                                                                                  :location location
                                                                                  :rator (parser:make-node-variable
                                                                                          :location location
                                                                                          :name 'classes:hash)
                                                                                  :rands (list
                                                                                          (parser:make-node-variable
                                                                                           :location location
                                                                                           :name cfield)))
                                                                                 acc)))
                                                                      cfields
                                                                      :initial-value (parser:make-node-lisp
                                                                                      :location location
                                                                                      :type (parser:make-tycon :location location :name 'classes:hash)
                                                                                      :vars '()
                                                                                      :var-names '()
                                                                                      :body (list
                                                                                             (hash-symbols
                                                                                              (parser:identifier-src-name 
                                                                                               (parser:type-definition-name def))
                                                                                              (parser:identifier-src-name
                                                                                               (parser:type-definition-ctor-name ctor)))))))))))
      :location location
      :inline nil))))

(defmethod tc:derive-methods ((class (eql 'classes:default)) def env)
  "Deriver implementation for class `Eq'."
  (unless (= 1 (length (parser:type-definition-ctors def)))
    (let ((type-name (parser:identifier-src-name (parser:type-definition-name def))))
      (tc:tc-error (format nil "Cannot derive class ~A for type ~A."
                           class
                           type-name)
                   (source:note (parser:type-definition-derive def)
                                "Class ~A can only be derived for types with a single constructor."
                                class)
                   (source:note def
                                "when deriving class ~A for type ~A."
                                class
                                type-name))))

  (let ((location (source:location (parser:type-definition-derive def)))
        (ctor (first (parser:type-definition-ctors def))))
    (list
     (parser:make-instance-method-definition
      :name (parser:make-node-variable
             :location location
             :name 'classes:default)
      :params (list
               (parser:make-pattern-var
                :location location
                :name 'coalton:_
                :orig-name 'coalton:_))
      :body (parser:make-node-body
             :nodes nil
             :last-node (parser:make-node-application
                         :location location
                         :rator (parser:make-node-variable
                                 :location location
                                 :name (parser:identifier-src-name (parser:type-definition-name def)))
                         :rands (mapcar
                                 (constantly 
                                  (parser:make-node-application
                                   :location location
                                   :rator (parser:make-node-variable
                                           :location location
                                           :name 'classes:default)
                                   :rands '()))
                                 (parser:type-definition-ctor-field-types ctor))))
      :location location
      :inline nil))))
