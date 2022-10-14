;;;;
;;;; Mirror of patterns in src/parser/pattern.lisp with types attached.
;;;;

(defpackage #:coalton-impl/typechecker/pattern
  (:use
   #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:pattern                            ; STRUCT
   #:pattern-type                       ; ACCESSOR
   #:pattern-source                     ; ACCESSOR
   #:pattern-list-p                     ; FUNCTION
   #:pattern-list                       ; TYPE
   #:pattern-var                        ; STRUCT
   #:make-pattern-var                   ; ACCESSOR
   #:pattern-var-name                   ; ACCESSOR
   #:pattern-var-orig-name              ; ACCESSOR
   #:pattern-var-p                      ; FUNCTION
   #:pattern-literal                    ; STRUCT
   #:make-pattern-literal               ; CONSTRUCTOR
   #:pattern-literal-value              ; ACCESSOR
   #:pattern-literal-p                  ; FUNCTION
   #:pattern-wildcard                   ; STRUCT
   #:make-pattern-wildcard              ; ACCESSOR
   #:pattern-wildcard-p                 ; FUNCTION
   #:pattern-constructor                ; STRUCT
   #:make-pattern-constructor           ; CONSTRUCTOR
   #:pattern-constructor-name           ; ACCESSOR
   #:pattern-constructor-patterns       ; ACCESSOR
   #:pattern-constructor-p              ; FUNCTION
   #:parse-pattern                      ; FUNCTION
   #:pattern-variables                  ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/pattern)

(defstruct (pattern
            (:constructor nil)
            (:copier nil))
  (type   (util:required 'type)   :type tc:qualified-ty :read-only t)
  (source (util:required 'source) :type cons            :read-only t))

(defun pattern-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'pattern-p x)))

(deftype pattern-list ()
  '(satisfies pattern-list-p))

(defstruct (pattern-var
            (:include pattern)
            (:copier nil))
  (name      (util:required 'name)      :type parser:identifier :read-only t)
  (orig-name (util:required 'orig-name) :type parser:identifier :read-only t))

(defstruct (pattern-literal
            (:include pattern)
            (:copier nil))
  (value (util:required 'value) :type util:literal-value :read-only t))

(defstruct (pattern-wildcard
            (:include pattern)
            (:copier nil)))

(defstruct (pattern-constructor
            (:include pattern)
            (:copier nil))
  (name     (util:required 'name)     :type parser:identifier :read-only t)
  (patterns (util:required 'patterns) :type pattern-list      :read-only t))

;;;
;;; Methods
;;;

(defmethod tc:apply-substitution (subs (node pattern-var))
  (declare (type tc:substitution-list subs)
           (values pattern-var &optional))
  (make-pattern-var
   :type (tc:apply-substitution subs (pattern-type node))
   :source (pattern-source node)
   :name (pattern-var-name node)
   :orig-name (pattern-var-orig-name node)))

(defmethod tc:apply-substitution (subs (node pattern-literal))
  (declare (type tc:substitution-list subs)
           (values pattern-literal &optional))
  (make-pattern-literal
   :type (tc:apply-substitution subs (pattern-type node))
   :source (pattern-source node)
   :value (pattern-literal-value node)))

(defmethod tc:apply-substitution (subs (node pattern-wildcard))
  (declare (type tc:substitution-list subs)
           (values pattern-wildcard &optional))
  (make-pattern-wildcard
   :type (tc:apply-substitution subs (pattern-type node))
   :source (pattern-source node)))

(defmethod tc:apply-substitution (subs (node pattern-constructor))
  (declare (type tc:substitution-list subs)
           (values pattern-constructor &optional))
  (make-pattern-constructor
   :type (tc:apply-substitution subs (pattern-type node))
   :source (pattern-source node)
   :name (pattern-constructor-name node)
   :patterns (tc:apply-substitution subs (pattern-constructor-patterns node))))
