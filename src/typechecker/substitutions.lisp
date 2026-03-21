(defpackage #:coalton-impl/typechecker/substitutions
  (:use
   #:cl
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/types)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:substitution                       ; STRUCT
   #:make-substitution                  ; CONSTRUCTOR
   #:substitution-from                  ; ACCESSOR
   #:substitution-to                    ; ACCESSOR
   #:substitution-p                     ; FUNCTION
   #:substitution-list                  ; TYPE
   #:merge-substitution-lists           ; FUNCTION
   #:compose-substitution-lists         ; FUNCTION
   #:apply-substitution                 ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/substitutions)

;;;
;;; Type substitutions
;;;

(defstruct substitution
  (from (util:required 'from) :type tyvar :read-only t)
  (to   (util:required 'to)   :type ty    :read-only t))

(defun substitution-list-p (thing)
  (and (alexandria:proper-list-p thing)
       (every (lambda (x) (typep x 'substitution)) thing)))

(deftype substitution-list ()
  '(satisfies substitution-list-p))

(define-condition substitution-list-merge-error (coalton-internal-type-error)
  ())

(defun merge-substitution-lists (s1 s2)
  "Merge substitution lists S1 and S2 together, erroring on disagreeing entries."
  (declare (type substitution-list s1)
           (type substitution-list s2)
           (values substitution-list))
  (let ((overlap (intersection s1 s2 :key #'substitution-from :test #'ty=)))
    (if (every (lambda (x)
                 (ty= (apply-substitution s1 x) (apply-substitution s2 x)))
               (mapcar #'substitution-from overlap))
        (concatenate 'list s1 s2)
        (error 'substitution-list-merge-error))))

(defun compose-substitution-lists (s1 s2)
  "Compose substitution lists S1 and S2 together, applying S1 to S2."
  (declare (type substitution-list s1)
           (type substitution-list s2)
           (values substitution-list))
  (append
   (mapcar
    (lambda (s)
      (make-substitution
       :from (substitution-from s)
       :to (apply-substitution s1 (substitution-to s))))
    s2)
   s1))

(defgeneric apply-substitution (subst-list type)
  (:documentation "Apply the substitutions defined in SUBST-LIST on TYPE.")
  ;; For a type variable, substitute if it is in SUBST-LIST, otherwise return the original type
  (:method (subst-list (type tyvar))
    (let ((subst (find type subst-list :key #'substitution-from :test #'ty=)))
      (if subst
          (substitution-to subst)
          type)))
  ;; For a type application, recurse down into all the types
  (:method (subst-list (type tapp))
    (let* ((alias (mapcar (lambda (alias) (apply-substitution subst-list alias))
                          (ty-alias type)))
           (from (apply-substitution subst-list (tapp-from type)))
           (to (apply-substitution subst-list (tapp-to type)))
           (applied-type (make-tapp :alias alias :from from :to to)))
      (if (and (tapp-p applied-type)
               (function-type-p applied-type))
          (make-function-ty
           :alias alias
           :positional-input-types (list (tapp-to (tapp-from applied-type)))
           :keyword-input-types nil
           :keyword-open-p nil
           :output-types (list (tapp-to applied-type)))
          applied-type)))
  (:method (subst-list (entry keyword-ty-entry))
    (make-keyword-ty-entry
     :keyword (keyword-ty-entry-keyword entry)
     :type (apply-substitution subst-list (keyword-ty-entry-type entry))))
  (:method (subst-list (type function-ty))
    (make-function-ty
     :alias (mapcar (lambda (alias) (apply-substitution subst-list alias)) (ty-alias type))
     :positional-input-types (apply-substitution subst-list
                                                (function-ty-positional-input-types type))
     :keyword-input-types (apply-substitution subst-list
                                             (function-ty-keyword-input-types type))
     :keyword-open-p (function-ty-keyword-open-p type)
     :output-types (normalize-function-output-types
                    (apply-substitution subst-list
                                        (function-ty-output-types type)))))
  (:method (subst-list (type result-ty))
    (make-result-ty
     :alias (mapcar (lambda (alias) (apply-substitution subst-list alias)) (ty-alias type))
     :output-types (apply-substitution subst-list
                                       (result-ty-output-types type))))
  ;; Otherwise, do nothing
  (:method (subst-list (type ty))
    type)
  ;; Allow for calling on lists
  (:method (subst-list (type-list list))
    (mapcar (lambda (x) (apply-substitution subst-list x)) type-list)))



(defun pprint-substitution (stream sub &optional colon-p at-sign-p)
  (declare (ignore colon-p)
           (ignore at-sign-p))
  (write-string "#T" stream)
  (write (tyvar-id (substitution-from sub)) :stream stream)
  (write-string " +-> " stream)
  (write (substitution-to sub) :stream stream)
  nil)

(set-pprint-dispatch 'substitution 'pprint-substitution)
