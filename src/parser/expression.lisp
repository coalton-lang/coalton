(defpackage #:coalton-impl/parser/expression
  (:use
   #:cl
   #:coalton-impl/parser/base
   #:coalton-impl/parser/types
   #:coalton-impl/parser/pattern)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:util #:coalton-impl/util))
  (:export
   #:node                               ; STRUCT
   #:node-source                        ; ACCESSOR
   #:node-list                          ; TYPE
   #:node-variable                      ; STRUCT
   #:make-node-variable                 ; CONSTRUCTOR
   #:node-variable-name                 ; ACCESSOR
   #:node-variable-list                 ; TYPE
   #:node-literal                       ; STRUCT
   #:make-node-literal                  ; CONSTRUCTOR
   #:node-literal-value                 ; ACCESSOR
   #:node-integer-literal               ; STRUCT
   #:make-node-integerl-literal         ; CONSTRUCTOR
   #:node-integer-literal-value         ; ACCESSOR
   #:node-bind                          ; STRUCT
   #:make-node-bind                     ; CONSTRUCTOR
   #:node-bind-pattern                  ; ACCESSOR
   #:node-bind-expr                     ; ACCESSOR
   #:node-bind-source                   ; ACCESSOR
   #:node-body-element                  ; TYPE
   #:node-body-element-list             ; TYPE
   #:node-body                          ; STRUCT
   #:make-node-body                     ; CONSTRUCTOR
   #:node-body-nodes                    ; ACCESSOR
   #:node-body-last-node                ; ACCESSOR
   #:node-abstraction                   ; STRUCT
   #:make-node-abstraction              ; CONSTRUCTOR
   #:node-abstraction-vars              ; ACCESSOR
   #:node-abstraction-body              ; ACCESSOR
   #:node-let-binding                   ; STRUCT
   #:make-node-let-binding              ; CONSTRUCTOR
   #:node-let-binding-name              ; ACCESSOR
   #:node-let-binding-value             ; ACCESSOR
   #:node-let-binding-list              ; TYPE
   #:node-let                           ; STRUCT
   #:make-node-let                      ; CONSTRUCTOR
   #:node-let-bindings                  ; ACCESSOR
   #:node-let-body                      ; ACCESSOR
   #:node-lisp                          ; STRUCT
   #:make-node-lisp                     ; CONSTRUCTOR
   #:node-lisp-type                     ; ACCESSOR
   #:node-lisp-vars                     ; ACCESSOR
   #:node-lisp-body                     ; ACCESSOR
   #:node-match-branch                  ; STRUCT
   #:make-node-match-branch             ; CONSTRUCTOR
   #:node-match-branch-pattern          ; ACCESSOR
   #:node-match-branch-body             ; ACCESSOR
   #:node-match-branch-source           ; ACCESSOR
   #:node-match-branch-list             ; TYPE
   #:node-match                         ; STRUCT
   #:make-node-match                    ; CONSTRUCTOR
   #:node-match-expr                    ; ACCESSOR
   #:node-match-branches                ; ACCESSOR
   #:node-progn                         ; STRUCT
   #:make-node-progn                    ; CONSTRUCTOR
   #:node-progn-body                    ; ACCESSOR
   #:node-the                           ; STRUCT
   #:make-node-the                      ; CONSTRUCTOR
   #:node-the-type                      ; ACCESSOR
   #:node-the-expr                      ; ACCESSOR
   #:node-return                        ; STRUCT
   #:make-node-return                   ; CONSTRUCTOR
   #:node-return-expr                   ; ACCESSOR
   #:node-application                   ; STRUCT
   #:make-node-application              ; CONSTRUCTOR
   #:node-application-rator             ; ACCESSOR
   #:node-application-rands             ; ACCESSOR
   #:node-or                            ; STRUCT
   #:make-node-or                       ; CONSTRUCTOR
   #:node-or-nodes                      ; ACCESSOR
   #:node-and                           ; STRUCT
   #:make-node-and                      ; CONSTRUCTOR
   #:node-and-nodes                     ; ACCESSOR
   #:node-if                            ; STRUCT
   #:make-node-if                       ; CONSTRUCTOR
   #:node-if-expr                       ; ACCESSOR
   #:node-if-then                       ; ACCESSOR
   #:node-if-else                       ; ACCESSOR
   #:node-when                          ; STRUCT
   #:make-node-when                     ; CONSTRUCTOR
   #:node-when-expr                     ; ACCESSOR
   #:node-when-body                     ; ACCESSOR
   #:node-unless                        ; STRUCT
   #:make-node-unless                   ; CONSTRUCTOR
   #:node-unless-expr                   ; ACCESSOR
   #:node-unless-body                   ; ACCESSOR
   #:node-cond-clause                   ; STRUCT
   #:make-node-cond-clause              ; CONSTRUCTOR
   #:node-cond-clause-expr              ; ACCESSOR
   #:node-cond-clause-body              ; ACCESSOR
   #:node-cond-clause-source            ; ACCESSOR
   #:node-cond-clause-list              ; TYPE
   #:node-cond                          ; STRUCT
   #:make-node-cond                     ; CONSTRUCTOR
   #:node-cond-clauses                  ; ACCESSOR
   #:node-do-bind                       ; STRUCT
   #:make-node-do-bind                  ; CONSTRUCTOR
   #:node-do-bind-name                  ; ACCESSOR
   #:node-do-bind-expr                  ; ACCESSOR
   #:node-do-bind-source                ; ACCESSOR
   #:node-do-body-element               ; TYPE
   #:node-body-element-list             ; TYPE
   #:node-do                            ; STRUCT
   #:make-node-do                       ; CONSTRUCTOR
   #:node-do-nodes                      ; ACCESSOR
   #:node-do-last-node                  ; ACCESSOR
   #:parse-expression                   ; FUNCTION
   #:parse-body                         ; FUNCTION
   #:parse-variable                     ; FUNCTION
   ))

;; TODO: declare in let expressions

(in-package #:coalton-impl/parser/expression)

(defstruct (node
            (:constructor nil)
            (:copier nil))
  (source (util:required 'source) :type cons :read-only t))

(defun node-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-p x)))

(deftype node-list ()
  '(satisfies node-list-p))

(defstruct (node-variable
            (:include node)
            (:copier nil))
  (name (util:required 'name) :type identifier :read-only t))

(defun node-variable-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-variable-p x)))

(deftype node-variable-list ()
  '(satisfies node-variable-list-p))

(defstruct (node-literal
            (:include node)
            (:copier nil))
  (value (util:required 'value) :type (and util:literal-value (not integer)) :read-only t))

(defstruct (node-integer-literal
            (:include node)
            (:copier nil))
  (value (util:required 'value) :type integer :read-only t))

;;
;; Does not subclass node, can only appear in a node body
;;
(defstruct (node-bind
            (:copier nil))
  (pattern (util:required 'pattern) :type pattern :read-only t)
  (expr    (util:required 'expr)    :type node    :read-only t)
  (source  (util:required 'source)  :type cons    :read-only t))

(deftype node-body-element ()
  '(or node node-bind))

(defun node-body-element-p (x)
  (typep x 'node-body-element))

(defun node-body-element-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-body-element-p x)))

(deftype node-body-element-list ()
  '(satisfies node-body-element-list-p))

;;
;; Does not subclass node, can only appear directly within some nodes
;;
;; - must contain at least one node
;; - cannot be terminated by a `node-bind'
;; - does not have source information (but it's children do)
;;
(defstruct (node-body
            (:copier nil))
  (nodes     (util:required 'node)      :type node-body-element-list :read-only t)
  (last-node (util:required 'last-node) :type node                   :read-only t))

(defstruct (node-abstraction
            (:include node)
            (:copier nil))
  (vars (util:required 'vars) :type node-variable-list :read-only t)
  (body (util:required 'body) :type node-body          :read-only t))

(defstruct (node-let-binding
            (:copier nil))
  (name   (util:required 'name)   :type node-variable :read-only t)
  (value  (util:required 'value)  :type node          :read-only t)
  (source (util:required 'source) :type cons          :read-only t))

(defun node-let-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-let-binding-p x)))

(deftype node-let-binding-list ()
  '(satisfies node-let-binding-list-p))

(defstruct (node-let-declare
            (:copier nil))
  (name   (util:required 'name)   :type node-variable :read-only t)
  (type   (util:required 'type)   :type ty            :read-only t)
  (source (util:required 'source) :type cons          :read-only t))

(defun node-let-declare-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-let-declare-p x)))

(deftype node-let-declare-list ()
  '(satisfies node-let-declare-list-p))

(defstruct (node-let
            (:include node)
            (:copier nil))
  (bindings (util:required 'bindings) :type node-let-binding-list :read-only t)
  (declares (util:required 'declares) :type node-let-declare-list :read-only t)
  (body     (util:required 'body)     :type node-body             :read-only t))

(defstruct (node-lisp
            (:include node)
            (:copier nil))
  (type (util:required 'type) :type ty                 :read-only t)
  (vars (util:required 'vars) :type node-variable-list :read-only t)
  (body (util:required 'body) :type cst:cst            :read-only t))

(defstruct (node-match-branch
            (:copier nil))
  (pattern (util:required 'pattern) :type pattern   :read-only t)
  (body    (util:required 'body)    :type node-body :read-only t)
  (source  (util:required 'source)  :type cons      :read-only t))

(defun node-match-branch-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-match-branch-p x)))

(deftype node-match-branch-list ()
  '(satisfies node-match-branch-list-p))

(defstruct (node-match
            (:include node)
            (:copier nil))
  (expr     (util:required 'expr)     :type node                   :read-only t)
  (branches (util:required 'branches) :type node-match-branch-list :read-only t))

(defstruct (node-progn
            (:include node)
            (:copier nil))
  (body (util:required 'body) :type node-body :read-only t))

(defstruct (node-the
            (:include node)
            (:copier nil))
  (type (util:required 'type) :type ty   :read-only t)
  (expr (util:required 'expr) :type node :read-only t))

(defstruct (node-return
            (:include node)
            (:copier nil))
  ;; Either the returned expression or null in the case of "(return)"
  (expr (util:required 'expr) :type (or null node) :read-only t))

(defstruct (node-application
            (:include node)
            (:copier nil))
  (rator (util:required 'rator) :type node      :read-only t)
  (rands (util:required 'rands) :type node-list :read-only t))

(defstruct (node-or
            (:include node)
            (:copier nil))
  (nodes (util:required 'nodes) :type node-list :read-only t))

(defstruct (node-and
            (:include node)
            (:copier nil))
  (nodes (util:required 'nodes) :type node-list :read-only t))

(defstruct (node-if
            (:include node)
            (:copier nil))
  (expr (util:required 'expr) :type node :read-only t)
  (then (util:required 'expr) :type node :read-only t)
  (else (util:required 'else) :type node :read-only t))

(defstruct (node-when
            (:include node)
            (:copier nil))
  (expr (util:required 'expr) :type node      :read-only t)
  (body (util:required 'body) :type node-body :read-only t))

(defstruct (node-unless
            (:include node)
            (:copier nil))
  (expr (util:required 'expr) :type node      :read-only t)
  (body (util:required 'body) :type node-body :read-only t))

(defstruct (node-cond-clause
            (:copier nil))
  (expr   (util:required 'expr)   :type node      :read-only t)
  (body   (util:required 'body)   :type node-body :read-only t)
  (source (util:required 'source) :type cons      :read-only t))

(defun node-cond-clause-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-cond-clause-p x)))

(deftype node-cond-clause-list ()
  '(satisfies node-cond-clause-list-p))

(defstruct (node-cond
            (:include node)
            (:copier nil))
  (clauses (util:required 'clauses) :type node-cond-clause-list :read-only t))

(defstruct (node-do-bind
            (:copier nil))
  (name   (util:required 'name)   :type node-variable :read-only t)
  (expr   (util:required 'expr)   :type node          :read-only t)
  (source (util:required 'source) :type cons          :read-only t))

(deftype node-do-body-element ()
  '(or node node-bind node-do-bind))

(defun node-do-body-element-p (x)
  (typep x 'node-do-body-element))

(defun node-do-body-element-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-do-body-element-p x)))

(deftype node-do-body-element-list ()
  '(satisfies node-do-body-element-list-p))

(defstruct (node-do
            (:include node)
            (:copier nil))
  (nodes     (util:required 'nodes)     :type node-do-body-element-list :read-only t)
  (last-node (util:required 'last-node) :type node                      :read-only t))

(defun parse-expression (form file)
  (declare (type cst:cst form)
           (type sb-sys::fd-stream file)
           (values node &optional))

  ;;
  ;; Atoms
  ;;

  (when (cst:atom form)
    (return-from parse-expression
      (typecase (cst:raw form)
        (null
         (error 'parse-error
                :err (coalton-error
                      form file
                      :message "Malformed expression"
                      :primary-note "unexpected `nil` or `()`")))

        (symbol (parse-variable form file))

        (t
         (parse-literal form file)))))

  ;;
  ;; Dotted Lists
  ;;

  (unless (cst:proper-list-p form)
    (error 'parse-error
           :err (coalton-error
                 form file
                 :message "Malformed expression"
                 :primary-note "unexpected dotted list")))

  ;;
  ;; Keywords
  ;;

  (when (cst:atom (cst:first form))
    (case (cst:raw (cst:first form))
      (coalton:fn
          (let ((variables)
                (body))

            ;; (fn)
            (unless (cst:consp (cst:rest form))
              (error 'parse-error
                     :err (coalton-error
                           form file
                           :highlight :end
                           :message "Malformed function"
                           :primary-note "expected function arguments")))

            ;; (fn (...))
            (unless (cst:consp (cst:rest (cst:rest form)))
              (error 'parse-error
                     :err (coalton-error
                           form file
                           :highlight :end
                           :message "Malformed function"
                           :primary-note "expected function body")))

            ;; (fn x ...)
            ;;
            ;; NOTE: (fn () ...) is allowed
            (when (and (cst:atom (cst:second form))
                       (not (eq (cst:raw (cst:second form)) nil)))
              (error 'parse-error
                     :err (coalton-error
                           (cst:second form) file
                           :message "Malformed function"
                           :primary-note "malformed arugment list"
                           :help-notes
                           (list
                            (make-coalton-error-help
                             :span (cst:source (cst:second form))
                             :replacement
                             (lambda (existing)
                               (concatenate 'string "(" existing ")"))
                             :message "add parentheses")))))

            (setf variables
                  (loop :for vars := (cst:second form) :then (cst:rest vars)
                        :while (cst:consp vars)
                        :collect (parse-variable (cst:first vars) file)))

            (setf body (parse-body (cst:nthrest 2 form) form file))

            (return-from parse-expression
              (make-node-abstraction
               :vars variables
               :body body
               :source (cst:source form)))))

      (coalton:let
          (let ((bindings)
                (body)
                (declares))

            ;; (let)
            (unless (cst:consp (cst:rest form))
              (error 'parse-error
                     :err (coalton-error
                           form file
                           :highlight :end
                           :message "Malformed let"
                           :primary-note "expected let binding list")))

            ;; (let (...))
            (unless (cst:consp (cst:rest (cst:rest form)))
              (error 'parse-error
                     :err (coalton-error
                           form file
                           :highlight :end
                           :message "Malformed let"
                           :primary-note "expected let body")))

            ;; (let x ...)
            (unless (cst:consp (cst:second form))
              (error 'parse-error
                     :err (coalton-error
                           (cst:second form) file
                           :message "Malformed let"
                           :primary-note "expected binding list")))

            (setf bindings
                  (loop :for bindings := (cst:second form) :then (cst:rest bindings)
                        :while (cst:consp bindings)
                        :for binding := (cst:first bindings)
                        ;; if binding is in the form (declare x y+)
                        :if (and (cst:consp binding)
                                 (cst:consp (cst:rest form))
                                 (cst:consp (cst:rest (cst:rest form)))
                                 (cst:atom (cst:first binding))
                                 (eq (cst:raw (cst:first binding)) 'coalton:declare))
                          :do (push (parse-let-declare binding file) declares)
                        :else
                          :collect (parse-let-binding binding file)))

            (setf body (parse-body (cst:nthrest 2 form) form file))
            (setf declares (nreverse declares))

            (return-from parse-expression
              (make-node-let
               :bindings bindings
               :declares declares
               :body body
               :source (cst:source form)))))

      (coalton:lisp
          (let ((type)
                (vars)
                (body))

            ;; (lisp)
            (unless (cst:consp (cst:rest form))
              (error 'parse-error
                     :err (coalton-error
                           form file
                           :highlight :end
                           :message "Malformed lisp expression"
                           :primary-note "expected expression type")))

            ;; (lisp T)
            (unless (cst:consp (cst:rest (cst:rest form)))
              (error 'parse-error
                     :err (coalton-error
                           form file
                           :highlight :end
                           :message "Malformed lisp expression"
                           :primary-note "expected binding list")))

            ;; (lisp T (...))
            (unless (cst:consp (cst:rest (cst:rest (cst:rest form))))
              (error 'parse-error
                     :err (coalton-error
                           form file
                           :highlight :all
                           :message "Malformed lisp expression"
                           :primary-note "expected body")))

            (setf type (parse-type (cst:second form) file))

            (setf vars
                  (loop :for vars := (cst:third form) :then (cst:rest vars)
                        :while (cst:consp vars)
                        :collect (parse-variable (cst:first vars) file)))

            (setf body (cst:nthrest 3 form))

            (return-from parse-expression
              (make-node-lisp
               :type type
               :vars vars
               :body body
               :source (cst:source form)))))

      (coalton:match
          (let ((expr)
                (branches))

            ;; (match)
            (unless (cst:consp (cst:rest form))
              (error 'parse-error
                     :err (coalton-error
                           form file
                           :highlight :end
                           :message "Malformed match expression"
                           :primary-note "expected expression")))

            ;; (match x)
            (unless (cst:consp (cst:rest (cst:rest form)))
              (error 'parse-error
                     :err (coalton-error
                           form file
                           :highlight :end
                           :message "Malformed match expression"
                           :primary-note "expected match arms")))

            (setf expr (parse-expression (cst:second form) file))

            (setf branches
                  (loop :for branches := (cst:nthrest 2 form) :then (cst:rest branches)
                        :while (cst:consp branches)
                        :collect (parse-match-branch (cst:first branches) file)))

            (return-from parse-expression
              (make-node-match
               :expr expr
               :branches branches
               :source (cst:source form)))))

      (coalton:progn
        (return-from parse-expression
          (make-node-progn
           :body (parse-body (cst:rest form) form file)
           :source (cst:source form))))

      (coalton:the
       (let ((type)
             (expr))

         ;; (the)
         (unless (cst:consp (cst:rest form))
           (error 'parse-error
                  :err (coalton-error
                        form file
                        :highlight :end
                        :message "Malformed the expression"
                        :primary-note "expected type")))

         ;; (the T)
         (unless (cst:consp (cst:rest (cst:rest form)))
           (error 'parse-error
                  :err (coalton-error
                        form file
                        :highlight :end
                        :message "Malformed the expression"
                        :primary-note "expected value")))

         ;; (the a b c)
         (when (cst:consp (cst:rest (cst:rest (cst:rest form))))
           (error 'parse-error
                  :err (coalton-error
                        (cst:first (cst:rest (cst:rest (cst:rest form)))) file
                        :message "Malformed the expression"
                        :primary-note "unexpected trailing form")))

         (setf type (parse-type (cst:second form) file))
         (setf expr (parse-expression (cst:third form) file))

         (return-from parse-expression
           (make-node-the
            :type type
            :expr expr
            :source (cst:source form)))))

      (coalton:return
        (let ((expr))

          ;; (return ...)
          (when (cst:consp (cst:rest form))
            ;; (return a b ...)
            (when (cst:consp (cst:rest (cst:rest form)))
              (error 'parse-error
                     :err (coalton-error
                           (cst:first (cst:rest (cst:rest form))) file
                           :message "Malformed return expression"
                           :primary-note "unexpected trailing form")))

            (setf expr (parse-expression (cst:second form) file)))

          (return-from parse-expression
            (make-node-return
             :expr expr
             :source (cst:source form)))))

      (coalton:or
       (unless (cst:consp (cst:rest form))
         (error 'parse-error
                :err (coalton-error
                      form file
                      :highlight :end
                      :message "Malformed or expression"
                      :primary-note "expected one or more arguments")))

       (return-from parse-expression
         (make-node-or
          :nodes (loop :for args := (cst:rest form) :then (cst:rest args)
                       :while (cst:consp args)
                       :for arg := (cst:first args)
                       :collect (parse-expression arg file))
          :source (cst:source form))))

      (coalton:and
       (unless (cst:consp (cst:rest form))
         (error 'parse-error
                :err (coalton-error
                      form file
                      :highlight :end
                      :message "Malformed and expression"
                      :primary-note "expected one or more arguments")))

       (return-from parse-expression
         (make-node-and
          :nodes (loop :for args := (cst:rest form) :then (cst:rest args)
                       :while (cst:consp args)
                       :for arg := (cst:first args)
                       :collect (parse-expression arg file))
          :source (cst:source form))))

      (coalton:if
       (unless (cst:consp (cst:rest form))
         (error 'parse-error
                :err (coalton-error
                      form file
                      :highlight :end
                      :message "Malformed if expression"
                      :primary-note "expected a predicate")))

       (unless (cst:consp (cst:rest (cst:rest form)))
         (error 'parse-error
                :err (coalton-error
                      form file
                      :highlight :end
                      :message "Malformed if expression"
                      :primary-note "expected a form")))

       (unless (cst:consp (cst:rest (cst:rest (cst:rest form))))
         (error 'parse-error
                :err (coalton-error
                      form file
                      :highlight :end
                      :message "Malformed if expression"
                      :primary-note "expected a form")))

       (when (cst:consp (cst:rest (cst:rest (cst:rest (cst:rest form)))))
         (error 'parse-error
                :err (coalton-error
                      (cst:first (cst:rest (cst:rest (cst:rest (cst:rest form))))) file
                      :highlight :end
                      :message "Malformed if expression"
                      :primary-note "unexpected trailing form")))

       (return-from parse-expression
         (make-node-if
          :expr (parse-expression (cst:second form) file)
          :then (parse-expression (cst:third form) file)
          :else (parse-expression (cst:fourth form) file)
          :source (cst:source form))))

      (coalton:when
          (unless (cst:consp (cst:rest form))
            (error 'parse-error
                   :err (coalton-error
                         form file
                         :highlight :end
                         :message "Malformed when expression"
                         :primary-note "expected a predicate")))

        (return-from parse-expression
          (make-node-when
           :expr (parse-expression (cst:second form) file)
           :body (parse-body (cst:rest (cst:rest form)) form file)
           :source (cst:source form))))

      (coalton:unless
          (unless (cst:consp (cst:rest form))
            (error 'parse-error
                   :err (coalton-error
                         form file
                         :highlight :end
                         :message "Malformed unless expression"
                         :primary-note "expected a predicate")))

        (return-from parse-expression
          (make-node-unless
           :expr (parse-expression (cst:second form) file)
           :body (parse-body (cst:rest (cst:rest form)) form file)
           :source (cst:source form))))

      (coalton:cond
        (unless (cst:consp (cst:rest form))
          (error 'parse-error
                 :err (coalton-error
                       form file
                       :highlight :end
                       :message "Malformed cond expression"
                       :primary-note "expected one or more clauses")))

        (return-from parse-expression
          (make-node-cond
           :clauses (loop :for clauses := (cst:rest form) :then (cst:rest clauses)
                          :while (cst:consp clauses)
                          :for clause := (cst:first clauses)
                          :collect (parse-cond-clause clause file))
           :source (cst:source form))))

      (coalton:do
       (return-from parse-expression
         (parse-do form file)))))

  ;;
  ;; Function Application
  ;;

  (let ((rator)
        (rands))

    (setf rator (parse-expression (cst:first form) file))

    (when (cst:consp (cst:rest form))
      (setf rands
            (loop :for rands := (cst:rest form) :then (cst:rest rands)
                  :while (cst:consp rands)
                  :for rand := (cst:first rands)
                  :collect (parse-expression rand file))))

    (make-node-application
     :rator rator
     :rands rands
     :source (cst:source form))))

(defun parse-variable (form file)
  (declare (type cst:cst form)
           (type sb-sys::fd-stream file)
           (values node-variable &optional))

  (unless (and (cst:atom form)
               (identifier-p (cst:raw form)))
    (error 'parse-error
           :err (coalton-error
                 form file
                 :message "Invalid variable"
                 :primary-note "expected identifier")))

  (make-node-variable
   :name (cst:raw form)
   :source (cst:source form)))

(defun parse-literal (form file)
  (declare (type cst:cst form)
           (type sb-sys::fd-stream file)
           (values node &optional))

  (assert (cst:atom form))

  (typecase (cst:raw form)
    (integer
     (make-node-integer-literal
      :value (cst:raw form)
      :source (cst:source form)))

    (util:literal-value
     (make-node-literal
      :value (cst:raw form)
      :source (cst:source form)))

    (t
     (error 'parse-error
            :err (coalton-error
                  form file
                  :message "Invalid literal"
                  :primary-note "unknown literal type")))))

(defun parse-body (form enclosing-form file)
  (declare (type cst:cst form)
           (type sb-sys::fd-stream file)
           (values node-body &optional))

  (when (cst:atom form)
    (error 'parse-error
           :err (coalton-error
                 enclosing-form file
                 :highlight :end
                 :message "Malformed function"
                 :primary-note "expected body")))

  (assert (cst:proper-list-p form))

  (let ((nodes)
        (last-node))

    (setf nodes
          (loop :for nodes := form :then (cst:rest nodes)
                :while (cst:consp nodes)

                ;; Not the last node
                :if (cst:consp (cst:rest nodes))
                  :collect (parse-body-element (cst:first nodes) file)

                ;; The last node
                :else
                  :do (setf last-node (parse-body-last-node (cst:first nodes) file))))

    (make-node-body
     :nodes nodes
     :last-node last-node)))

(defun shorthand-let-p (form)
  "Returns t if FORM is in the form of (let x = y+)"
  (declare (type cst:cst form)
           (values boolean))

  (when (cst:atom form)
    (return-from shorthand-let-p nil))

  ;; (let)
  (unless (cst:consp (cst:rest form))
    (return-from shorthand-let-p nil))

  ;; (let x)
  (unless (cst:consp (cst:rest (cst:rest form)))
    (return-from shorthand-let-p nil))

  ;; (let x =)
  (unless (cst:consp (cst:rest (cst:rest (cst:rest form))))
    (return-from shorthand-let-p nil))

  (and (cst:atom (cst:first form))
             (eq (cst:raw (cst:first form)) 'coalton:let)
             (cst:atom (cst:third form))
             (eq (cst:raw (cst:third form)) 'coalton:=)))

;; Forms passed to parse-node-bind must be previously verified by `shorthand-let-p'
(defun parse-node-bind (form file)
  (declare (type cst:cst form)
           (type sb-sys::fd-stream file)
           (values node-bind))

  (when (cst:consp (cst:rest (cst:rest (cst:rest (cst:rest form)))))
    (error 'parse-error
           :err (coalton-error
                 (cst:first (cst:rest (cst:rest (cst:rest (cst:rest form))))) file
                 :message "Malformed shorthand let"
                 :primary-note "unexpected trailing form")))

  (let ((pattern)
        (expr))

    (setf pattern (parse-pattern (cst:second form) file))
    (setf expr (parse-expression (cst:fourth form) file))

    (make-node-bind
     :pattern pattern
     :expr expr
     :source (cst:source form))))

(defun parse-body-element (form file)
  (declare (type cst:cst form)
           (type sb-sys::fd-stream file)
           (values node-body-element &optional))

  (when (cst:atom form)
    (return-from parse-body-element
      (parse-expression form file)))

  (unless (cst:proper-list-p form)
    (error 'parse-error
           :err (coalton-error
                 form file
                 :message "Malformed body expression"
                 :primary-note "unexpected dotted list")))


  (if (shorthand-let-p form)
      (parse-node-bind form file)
      (parse-expression form file)))

(defun parse-body-last-node (form file)
  (declare (type cst:cst form)
           (type sb-sys::fd-stream file)
           (values node &optional))

  (when (shorthand-let-p form)
    (error 'parse-error
           :err (coalton-error
                 form file
                 :message "Malformed body expression"
                 :primary-note "body forms cannot be terminated by a shorthand let")))

  (parse-expression form file))

(defun parse-let-binding (form file)
  (declare (type cst:cst form)
           (type sb-sys::fd-stream file)
           (values node-let-binding &optional))

  (when (cst:atom form)
    (error 'parse-error
           :err (coalton-error
                 form file
                 :message "Malformed let binding"
                 :primary-note "expected list")))

  (unless (cst:proper-list-p form)
    (error 'parse-error
           :err (coalton-error
                 form file
                 :message "Malformed let binding"
                 :primary-note "unexpected dotted list")))

  ;; (x)
  (unless (cst:consp (cst:rest form))
    (error 'parse-error
           :err (coalton-error
                 form file
                 :highlight :end
                 :message "Malformed let binding"
                 :primary-note "let bindings must have a value")))

  ;; (a b c ...)
  (when (cst:consp (cst:rest (cst:rest form)))
    (error 'parse-error
           :err (coalton-error
                 (cst:first (cst:rest (cst:rest form))) file
                 :message "Malformed let binding"
                 :primary-note "unexpected trailing form")))

  (make-node-let-binding
   :name (parse-variable (cst:first form) file)
   :value (parse-expression (cst:second form) file)
   :source (cst:source form)))

(defun parse-match-branch (form file)
  (declare (type cst:cst form)
           (type sb-sys::fd-stream file)
           (values node-match-branch &optional))

  (when (cst:atom form)
    (error 'parse-error
           :err (coalton-error
                 form file
                 :message "Malformed match branch"
                 :primary-note "expected list")))

  (unless (cst:proper-list-p form)
    (error 'parse-error
           :err (coalton-error
                 form file
                 :message "Malformed match branch"
                 :primary-note "unexpected dotted list")) )

  ;; (P)
  (unless (cst:consp (cst:rest form))
    (error 'parse-error
           :err (coalton-error
                 form file
                 :highlight :end
                 :message "Malformed match branch"
                 :primary-note "expected body")))

  (let ((pattern)
        (body))

    (setf pattern (parse-pattern (cst:first form) file))
    (setf body (parse-body (cst:rest form) form file))

    (make-node-match-branch
     :pattern pattern
     :body body
     :source (cst:source form))))

(defun parse-cond-clause (form file)
  (declare (type cst:cst form)
           (type sb-sys::fd-stream file)
           (values node-cond-clause))

  (when (cst:atom form)
    (error 'parse-error
           :err (coalton-error
                 form file
                 :message "Malformed cond clause"
                 :primary-note "expected list")))

  (unless (cst:proper-list-p form)
    (error 'parse-error
           :err (coalton-error
                 form file
                 :message "Malformed cond clause"
                 :primary-note "unexpected dotted list")))

  (make-node-cond-clause
   :expr (parse-expression (cst:first form) file)
   :body (parse-body (cst:rest form) form file)
   :source (cst:source form)))

(defun parse-do (form file)
  (declare (type cst:cst form)
           (type sb-sys::fd-stream))

  (assert (cst:consp form))

  (unless (cst:consp (cst:rest form))
    (error 'parse-error
           :err (coalton-error
                 form file
                 :highlight :end
                 :message "Malformed do expression"
                 :primary-note "expected one or more forms")))

  (let ((nodes)
        (last-node))

    (setf nodes
          (loop :for nodes := (cst:rest form) :then (cst:rest nodes)
                :while (cst:consp nodes)
                :for node := (cst:first nodes)

                ;; Not the last node
                :if (cst:consp (cst:rest nodes))
                  :collect (parse-do-body-element node file)

                :else
                  :do (setf last-node (parse-do-body-last-node node (cst:first form) file))))

    (make-node-do
     :nodes nodes
     :last-node last-node
     :source (cst:source form))))

(defun do-bind-p (form)
  "Returns t if FORM is in the form of (x <- y+)"
  (declare (type cst:cst form)
           (values boolean))

  (unless (cst:consp form)
    (return-from do-bind-p nil))

  ;; (x)
  (unless (cst:consp (cst:rest form))
    (return-from do-bind-p nil))

  ;; (x y)
  (unless (cst:consp (cst:rest (cst:rest form)))
    (return-from do-bind-p nil))

  ;; (x (y) ...)
  (unless (cst:atom (cst:second form))
    (return-from do-bind-p nil))

  (eq (cst:raw (cst:second form)) 'coalton:<-))

;; Forms passed to this function must first be validated with `do-bind-p'
(defun parse-node-do-bind (form file)
  (declare (type cst:cst form)
           (values node-do-bind))

  (when (cst:consp (cst:rest (cst:rest (cst:rest form))))
    (error 'parse-error
           :err (coalton-error
                 (cst:first (cst:rest (cst:rest (cst:rest form)))) file
                 :message "Malformed bind form"
                 :primary-note "unexpected trailing form")))

  (make-node-do-bind
   :name (parse-variable (cst:first form) file)
   :expr (parse-expression (cst:third form) file)
   :source (cst:source form)))

(defun parse-do-body-element (form file)
  (declare (type cst:cst form)
           (type sb-sys::fd-stream file)
           (values node-do-body-element &optional))

  (when (shorthand-let-p form)
    (return-from parse-do-body-element
      (parse-node-bind form file)))

  (when (do-bind-p form)
    (return-from parse-do-body-element
      (parse-node-do-bind form file)))

  (parse-expression form file))

(defun parse-do-body-last-node (form parent-form file)
  (declare (type cst:cst form)
           (type cst:cst parent-form)
           (type sb-sys::fd-stream file)
           (values node &optional))

  (when (shorthand-let-p form)
    (error 'parse-error
           :err (coalton-error
                 form file
                 :message "Malformed do expression"
                 :primary-note "do expressions cannot be terminated by a shorthand let"
                 :notes
                 (list
                  (make-coalton-error-note
                   :type :secondary
                   :span (cst:source parent-form)
                   :message "when parsing do expression")))))

  (when (do-bind-p form)
    (error 'parse-error
           :err (coalton-error
                 form file
                 :message "Malformed do expression"
                 :primary-note "do expression cannot be terminated by a bind"
                 :notes
                 (list
                  (make-coalton-error-note
                   :type :secondary
                   :span (cst:source parent-form)
                   :message "when parsing do expression")))))

  (parse-expression form file))

(defun parse-let-declare (form file)
  (declare (type cst:cst form)
           (type sb-sys::fd-stream file)
           (values node-let-declare))

  (assert (cst:consp form))
  (assert (cst:consp (cst:rest form)))
  (assert (cst:consp (cst:rest (cst:rest form))))

  (assert (cst:atom (cst:first form)))
  (assert (eq (cst:raw (cst:first form)) 'coalton:declare))

  (when (cst:consp (cst:rest (cst:rest (cst:rest form))))
    (error 'parse-error
           :err (coalton-error
                 (cst:fourth form) file
                 :message "Malformed declare"
                 :primary-note "unexpected form")))


  (make-node-let-declare
   :name (parse-variable (cst:second form) file)
   :type (parse-type (cst:third form) file)
   :source (cst:source form)))
