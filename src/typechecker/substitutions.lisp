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
   #:rebuild-if-changed                 ; MACRO
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

(defun apply-substitution-to-list (subst-list list)
  "Apply SUBST-LIST to each element of LIST, returning LIST itself when no
element changes."
  (loop :for tail :on list
        :for element := (car tail)
        :for new := (apply-substitution subst-list element)
        :unless (eq new element)
          :do (return
                (nconc (ldiff list tail)
                       (cons new
                             (mapcar (lambda (x) (apply-substitution subst-list x))
                                     (cdr tail)))))
        :finally (return list)))

(defun substitute-part (subst-list part)
  "Apply SUBST-LIST to PART, a type or a list of types, returning PART itself
when nothing in it changes."
  (if (listp part)
      (apply-substitution-to-list subst-list part)
      (apply-substitution subst-list part)))

(defmacro rebuild-if-changed ((subst-list original) (&rest parts) &body rebuild)
  "Apply SUBST-LIST to the PARTS of ORIGINAL, and rebuild ORIGINAL only if that
changes one of them.

Each element of PARTS is (VAR FORM [NORMALIZE]), where FORM reads a type, or a
list of types, out of ORIGINAL, and VAR is bound to the result of applying
SUBST-LIST to it, passed through the function NORMALIZE if one is named.
Substitution returns its argument when nothing in it changes."
  (alexandria:with-gensyms (subst)
    (let ((originals (mapcar (lambda (part) (gensym (symbol-name (first part)))) parts)))
      `(let* ((,subst ,subst-list)
              ,@(loop :for (var form normalize) :in parts
                      :for part :in originals
                      :collect `(,part ,form)
                      :collect `(,var ,(if normalize
                                           `(,normalize (substitute-part ,subst ,part))
                                           `(substitute-part ,subst ,part)))))
         (if (and ,@(loop :for (var) :in parts
                          :for part :in originals
                          :collect `(eq ,var ,part)))
             ,original
             (progn ,@rebuild))))))

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
      (rebuild-if-changed (s1 s)
          ((to (substitution-to s)))
        (make-substitution :from (substitution-from s) :to to)))
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
    (let ((applied-type (rebuild-if-changed (subst-list type)
                            ((alias (ty-alias type))
                             (from (tapp-from type))
                             (to (tapp-to type)))
                          (make-tapp :alias alias :from from :to to))))
      (if (function-type-p applied-type)
          (make-function-ty
           :alias (ty-alias applied-type)
           :positional-input-types (list (tapp-to (tapp-from applied-type)))
           :keyword-input-types nil
           :keyword-open-p nil
           :output-types (list (tapp-to applied-type)))
          applied-type)))
  (:method (subst-list (entry keyword-ty-entry))
    (rebuild-if-changed (subst-list entry)
        ((type (keyword-ty-entry-type entry)))
      (make-keyword-ty-entry
       :keyword (keyword-ty-entry-keyword entry)
       :type type)))
  (:method (subst-list (type function-ty))
    (rebuild-if-changed (subst-list type)
        ((alias (ty-alias type))
         (positional (function-ty-positional-input-types type))
         (keyword (function-ty-keyword-input-types type))
         (output (function-ty-output-types type) normalize-function-output-types))
      (make-function-ty
       :alias alias
       :positional-input-types positional
       :keyword-input-types keyword
       :keyword-open-p (function-ty-keyword-open-p type)
       :output-types output)))
  (:method (subst-list (type result-ty))
    (rebuild-if-changed (subst-list type)
        ((alias (ty-alias type))
         (output (result-ty-output-types type)))
      (make-result-ty :alias alias :output-types output)))
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
