;;;
;;; Mirror of toplevel definitions in src/parser/toplevel.lisp with
;;; types attached. Structs with a name field have a `node-variable'
;;; in that slot. The `node-type' slot of that variable is the type of
;;; the entire binding.
;;;

(defpackage #:coalton-impl/typechecker/toplevel
  (:use
   #:cl
   #:coalton-impl/source
   #:coalton-impl/typechecker/pattern
   #:coalton-impl/typechecker/expression)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:toplevel-define                    ; STRUCT
   #:make-toplevel-define               ; CONSTRUCTOR
   #:toplevel-define-name               ; ACCESSOR
   #:toplevel-define-params             ; ACCESSOR
   #:toplevel-define-body               ; ACCESSOR
   #:toplevel-define-list               ; TYPE
   #:instance-method-definition         ; STRUCT
   #:make-instance-method-definition    ; CONSTRUCTOR
   #:instance-method-definition-name    ; ACCESSOR
   #:instance-method-definition-params  ; ACCESSOR
   #:instance-method-definition-body    ; ACCESSOR
   #:instance-method-definition-list    ; TYPE
   #:toplevel-define-instance           ; STRUCT
   #:make-toplevel-define-instance      ; CONSTRUCTOR
   #:toplevel-define-instance-context   ; ACCESSOR
   #:toplevel-define-instance-pred      ; ACCESSOR
   #:toplevel-define-instance-methods   ; ACCESSOR
   #:toplevel-define-instance-head-location  ; ACCESSOR
   #:toplevel-define-instance-list      ; TYPE
   ))

(in-package #:coalton-impl/typechecker/toplevel)

(defstruct (toplevel-definition
            (:constructor nil)
            (:copier nil))
  (location (util:required 'location) :type location :read-only t))

(defmethod location ((self toplevel-definition))
  (toplevel-definition-location self))

(defstruct (toplevel-define
            (:include toplevel-definition)
            (:copier nil))
  (name   (util:required 'name)   :type node-variable :read-only t)
  (params (util:required 'params) :type pattern-list  :read-only t)
  (body   (util:required 'body)   :type node-body     :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-define-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-define-p x))))

(deftype toplevel-define-list ()
  '(satisfies toplevel-define-list-p))

(defmethod tc:apply-substitution (subs (node toplevel-define))
  (declare (type tc:substitution-list subs)
           (values toplevel-define &optional))

  (make-toplevel-define
   :name (tc:apply-substitution subs (toplevel-define-name node))
   :params (tc:apply-substitution subs (toplevel-define-params node))
   :body (tc:apply-substitution subs (toplevel-define-body node))
   :location (location node)))

(defstruct (instance-method-definition
            (:include toplevel-definition)
            (:copier nil))
  (name   (util:required 'name)   :type node-variable :read-only t)
  (params (util:required 'params) :type pattern-list  :read-only t)
  (body   (util:required 'body)   :type node-body     :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun instance-method-definition-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'instance-method-definition-p x))))

(deftype instance-method-definition-list ()
  '(satisfies instance-method-definition-list-p))

(defmethod tc:apply-substitution (subs (method instance-method-definition))
  (declare (type tc:substitution-list subs)
           (values instance-method-definition))

  (make-instance-method-definition
   :name (tc:apply-substitution subs (instance-method-definition-name method))
   :params (tc:apply-substitution subs (instance-method-definition-params method))
   :body (tc:apply-substitution subs (instance-method-definition-body method))
   :location (location method)))

(defstruct (toplevel-define-instance
            (:include toplevel-definition)
            (:copier nil))
  (context       (util:required 'context)       :type tc:ty-predicate-list :read-only t)
  (pred          (util:required 'pred)          :type tc:ty-predicate      :read-only t)
  (methods       (util:required 'methods)       :type hash-table           :read-only t)
  (head-location (util:required 'head-location) :type location             :read-only t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun toplevel-define-instance-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'toplevel-define-instance-p x))))

(deftype toplevel-define-instance-list ()
  '(satisfies toplevel-define-instance-list-p))
