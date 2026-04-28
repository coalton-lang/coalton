(defpackage #:coalton-impl/typechecker/scheme
  (:use
   #:cl
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/kinds
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/substitutions
   #:coalton-impl/typechecker/predicate)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:settings #:coalton-impl/settings))
  (:export
   #:ty-scheme                          ; STRUCT
   #:make-ty-scheme                     ; CONSTRUCTOR
   #:ty-scheme-explicit-p              ; ACCESSOR
   #:ty-scheme-kinds                    ; ACCESSOR
   #:ty-scheme-type                     ; ACCESSOR
   #:ty-scheme=                         ; FUNCTION
   #:ty-scheme-p                        ; FUNCTION
   #:scheme-list                        ; TYPE
   #:scheme-binding-list                ; TYPE
   #:quantify                           ; FUNCTION
   #:to-scheme                          ; FUNCTION
   #:fresh-inst                         ; FUNCTION
   #:ty-scheme-instantiation-types      ; FUNCTION
   #:scheme-predicates                  ; FUNCTION
   #:predicates-to-scheme               ; FUNCTION
   #:fresh-pred                         ; FUNCTION
   #:fresh-preds                        ; FUNCTION
   #:quantify-using-tvar-order          ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/scheme)

;;;
;;; Type schemes
;;;

(defstruct ty-scheme 
  ;; True when the quantified binders came from an explicit user-written
  ;; FORALL rather than implicit quantification.
  (explicit-p nil                    :type boolean      :read-only t)
  (kinds      (util:required 'kinds) :type list         :read-only t)
  (type       (util:required 'type)  :type qualified-ty :read-only t))

(defun ty-scheme= (ty-scheme1 ty-scheme2)
  "Check alpha-equivalence of two type schemes.

Uses bidirectional tgen-id mapping to verify that quantified variables can be
consistently renamed to make both schemes identical. Predicates are compared
as unordered bags (O(n²) when the tgen map is complete after comparing the
main type, O(n!) otherwise — but predicate lists are small in practice)."
  (labels ((kind-bag= (kinds1 kinds2)
             (labels ((bag= (remaining candidates)
                        (if (endp remaining)
                            (endp candidates)
                            (let ((reduced (remove (first remaining) candidates
                                                   :test #'equalp :count 1)))
                              (and (< (length reduced) (length candidates))
                                   (bag= (rest remaining) reduced))))))
               (bag= kinds1 kinds2)))

           (bind-tgen (id1 id2 kinds1 kinds2 map12 map21)
             (let ((entry12 (assoc id1 map12))
                   (entry21 (assoc id2 map21)))
               (cond
                 (entry12
                  (values (= (cdr entry12) id2) map12 map21))
                 (entry21
                  (values nil map12 map21))
                 ((or (>= id1 (length kinds1))
                      (>= id2 (length kinds2))
                      (not (equalp (nth id1 kinds1) (nth id2 kinds2))))
                  (values nil map12 map21))
                 (t
                  (values t (acons id1 id2 map12) (acons id2 id1 map21))))))

           (alpha-children= (children1 children2 kinds1 kinds2 map12 map21)
             "Compare two lists of types element-wise under the current tgen mapping."
             (if (/= (length children1) (length children2))
                 (values nil map12 map21)
                 (loop :for c1 :in children1
                       :for c2 :in children2
                       :do (multiple-value-bind (eq? m12 m21)
                               (alpha-ty= c1 c2 kinds1 kinds2 map12 map21)
                             (unless eq? (return (values nil map12 map21)))
                             (setf map12 m12 map21 m21))
                       :finally (return (values t map12 map21)))))

           (alpha-ty= (type1 type2 kinds1 kinds2 map12 map21)
             (cond
               ;; Leaf types: tyvars and tycons compare structurally
               ((or (typep type1 'tyvar) (typep type1 'tycon))
                (values (ty= type1 type2) map12 map21))
               ;; Quantified variables: check/extend the bijection
               ((typep type1 'tgen)
                (if (typep type2 'tgen)
                    (bind-tgen (tgen-id type1) (tgen-id type2)
                               kinds1 kinds2 map12 map21)
                    (values nil map12 map21)))
               ;; Type application
               ((typep type1 'tapp)
                (if (typep type2 'tapp)
                    (alpha-children= (list (tapp-from type1) (tapp-to type1))
                                     (list (tapp-from type2) (tapp-to type2))
                                     kinds1 kinds2 map12 map21)
                    (values nil map12 map21)))
               ;; Keyword type entry
               ((typep type1 'keyword-ty-entry)
                (if (and (typep type2 'keyword-ty-entry)
                         (eq (keyword-ty-entry-keyword type1)
                             (keyword-ty-entry-keyword type2)))
                    (alpha-ty= (keyword-ty-entry-type type1)
                               (keyword-ty-entry-type type2)
                               kinds1 kinds2 map12 map21)
                    (values nil map12 map21)))
               ;; Function types
               ((typep type1 'function-ty)
                (if (and (typep type2 'function-ty)
                         (= (length (function-ty-keyword-input-types type1))
                            (length (function-ty-keyword-input-types type2)))
                         (eq (function-ty-keyword-open-p type1)
                             (function-ty-keyword-open-p type2)))
                    (alpha-children= (append (function-ty-positional-input-types type1)
                                             (function-ty-keyword-input-types type1)
                                             (function-ty-output-types type1))
                                     (append (function-ty-positional-input-types type2)
                                             (function-ty-keyword-input-types type2)
                                             (function-ty-output-types type2))
                                     kinds1 kinds2 map12 map21)
                    (values nil map12 map21)))
               ;; Result types
               ((typep type1 'result-ty)
                (if (typep type2 'result-ty)
                    (alpha-children= (result-ty-output-types type1)
                                     (result-ty-output-types type2)
                                     kinds1 kinds2 map12 map21)
                    (values nil map12 map21)))
               (t
                (values nil map12 map21))))

           ;; Predicate bag comparison.  When the tgen map is already
           ;; complete (the common case after comparing the main type),
           ;; matches are deterministic so we commit to the first hit
           ;; and avoid backtracking — O(n²) instead of O(n!).  The
           ;; backtracking path is kept for the rare incomplete-map case.
           (alpha-pred-bag= (preds1 preds2 kinds1 kinds2 map12 map21)
             (cond
               ((endp preds1) (values (endp preds2) map12 map21))
               ((endp preds2) (values nil map12 map21))
               (t
                (let ((complete-map-p (= (length map12) (length kinds1))))
                  (dolist (p2 preds2 (values nil map12 map21))
                    (when (and (eq (ty-predicate-class (first preds1))
                                   (ty-predicate-class p2))
                               (= (length (ty-predicate-types (first preds1)))
                                  (length (ty-predicate-types p2))))
                      (multiple-value-bind (eq? m12 m21)
                          (alpha-children= (ty-predicate-types (first preds1))
                                           (ty-predicate-types p2)
                                           kinds1 kinds2 map12 map21)
                        (when eq?
                          (multiple-value-bind (rest-eq? rm12 rm21)
                              (alpha-pred-bag= (rest preds1)
                                               (remove p2 preds2 :count 1 :test #'eq)
                                               kinds1 kinds2 m12 m21)
                            (when (or rest-eq? complete-map-p)
                              (return (values rest-eq? rm12 rm21)))))))))))))

    (let ((kinds1 (ty-scheme-kinds ty-scheme1))
          (kinds2 (ty-scheme-kinds ty-scheme2)))
      (and (= (length kinds1) (length kinds2))
           (kind-bag= kinds1 kinds2)
           (multiple-value-bind (types-eq? map12 map21)
               (alpha-ty= (qualified-ty-type (ty-scheme-type ty-scheme1))
                          (qualified-ty-type (ty-scheme-type ty-scheme2))
                          kinds1 kinds2 nil nil)
             (and types-eq?
                  (alpha-pred-bag= (qualified-ty-predicates (ty-scheme-type ty-scheme1))
                                   (qualified-ty-predicates (ty-scheme-type ty-scheme2))
                                   kinds1 kinds2 map12 map21)))))))

(defmethod make-load-form ((self ty-scheme) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun scheme-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-scheme-p x)))

(deftype scheme-list ()
  '(satisfies scheme-list-p))

(defun scheme-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol ty-scheme))) x)))

(deftype scheme-binding-list ()
  `(satisfies scheme-binding-list-p))

;;;
;;; Operations on Schemes
;;;

(defun quantify (tyvars type)
  "Quantify the TYVARS that occur in TYPE, preserving binder metadata.

The resulting scheme keeps the source-name carried by each quantified
variable so later instantiation and pretty printing can recover the
programmer-written binders."
  (declare (type tyvar-list tyvars)
           (type qualified-ty type)
           (values ty-scheme))
  (let* ((vars (remove-if
                (lambda (x) (not (find x tyvars :test #'ty=)))
                (type-variables type)))
         (kinds (mapcar #'kind-of vars))
         (subst (loop :for var :in vars
                      :for id :from 0
                      :collect (make-substitution
                                :from var
                                :to (make-tgen :id id
                                               :allow-result-p (tyvar-allow-result-p var)
                                               :source-name (tyvar-source-name var))))))
    (make-ty-scheme
     :explicit-p nil
     :kinds kinds
     :type (apply-substitution subst type))))

(defun ty-scheme-instantiation-types (ty-scheme)
  "Return fresh instantiation variables for TY-SCHEME's quantified binders.

The returned variables preserve each binder's source-name so fresh-inst
and pretty printing can recover the programmer-written binders."
  (declare (type ty-scheme ty-scheme)
           (values tyvar-list &optional))
  (let* ((source-names (make-array (length (ty-scheme-kinds ty-scheme))
                                   :initial-element nil))
         (allow-result-flags (make-array (length (ty-scheme-kinds ty-scheme))
                                         :initial-element nil))
         (scheme-type (ty-scheme-type ty-scheme)))
    (labels ((collect-instantiation-metadata (object)
               (typecase object
                 (tgen
                  (when (< (tgen-id object) (length source-names))
                    (setf (aref source-names (tgen-id object))
                          (or (aref source-names (tgen-id object))
                              (tgen-source-name object)))
                    (setf (aref allow-result-flags (tgen-id object))
                          (or (aref allow-result-flags (tgen-id object))
                              (tgen-allow-result-p object)))))
                 (tapp
                  (collect-instantiation-metadata (tapp-from object))
                  (collect-instantiation-metadata (tapp-to object)))
                 (keyword-ty-entry
                  (collect-instantiation-metadata (keyword-ty-entry-type object)))
                 (function-ty
                  (collect-instantiation-metadata (function-ty-positional-input-types object))
                  (collect-instantiation-metadata (function-ty-keyword-input-types object))
                  (collect-instantiation-metadata (function-ty-output-types object)))
                 (result-ty
                  (collect-instantiation-metadata (result-ty-output-types object)))
                 (qualified-ty
                  (collect-instantiation-metadata (qualified-ty-predicates object))
                  (collect-instantiation-metadata (qualified-ty-type object)))
                 (ty-predicate
                  (collect-instantiation-metadata (ty-predicate-types object)))
                 (list
                  (map nil #'collect-instantiation-metadata object)))))
      (collect-instantiation-metadata scheme-type)
      (loop :for kind :in (ty-scheme-kinds ty-scheme)
            :for i :from 0
            :collect (make-variable :kind kind
                                    :source-name (aref source-names i)
                                    :allow-result-p (aref allow-result-flags i))))))

(defgeneric to-scheme (ty)
  (:method ((ty qualified-ty))
    (make-ty-scheme
     :explicit-p nil
     :kinds nil
     :type ty))

  (:method ((ty ty))
    (to-scheme (qualify nil ty))))

(defun fresh-inst (ty-scheme)
  "Instantiate TY-SCHEME with fresh type variables that preserve binder names."
  (declare (type ty-scheme ty-scheme)
           (values qualified-ty &optional))
  (let ((types (ty-scheme-instantiation-types ty-scheme)))
    (instantiate types (ty-scheme-type ty-scheme))))

(defun scheme-predicates (ty-scheme)
  "Get freshly instantiated predicates of scheme TY-SCHEME"
  (qualified-ty-predicates (fresh-inst ty-scheme)))

(defun predicates-to-scheme (preds sentinel)
  "Return a scheme over PREDS using SENTINEL as the unquantified result type.

SENTINEL exists only to make a predicate list fit the qualified type carried
by type schemes. Pass the same sentinel to multiple calls when comparing the
resulting schemes for alpha-equivalence.
"
  (declare (type ty-predicate-list preds)
           (type ty sentinel)
           (values ty-scheme &optional))
  (quantify (type-variables preds)
            (qualify preds sentinel)))

(defun fresh-pred (pred)
  "Returns PRED with fresh type variables"
  (declare (type ty-predicate pred)
           (values ty-predicate))
  (let ((scheme (predicates-to-scheme (list pred) (make-variable))))
    (car (qualified-ty-predicates (fresh-inst scheme)))))

(defun fresh-preds (preds)
  "Returns PRED with fresh type variables"
  (declare (type ty-predicate-list preds)
           (values ty-predicate-list))
  (let ((scheme (predicates-to-scheme preds (make-variable))))
    (qualified-ty-predicates (fresh-inst scheme))))

(defun quantify-using-tvar-order (tyvars type &optional (explicit-p nil))
  "Quantify TYVARS in TYPE, preserving the order supplied by TYVARS.

Only variables that actually occur in TYPE are quantified. When
EXPLICIT-P is true, the resulting scheme records that it came from an
explicit FORALL, which is later used to decide whether the binders
become lexically scoped."
  (let* ((vars (remove-if
                (lambda (x) (not (find x (type-variables type) :test #'ty=)))
                tyvars))
         (kinds (mapcar #'kind-of vars))
         (subst (loop :for var :in vars
                      :for id :from 0
                      :collect (make-substitution
                                :from var
                                :to (make-tgen :id id
                                               :allow-result-p (tyvar-allow-result-p var)
                                               :source-name (tyvar-source-name var))))))
    (make-ty-scheme
     :explicit-p explicit-p
     :kinds kinds
     :type (apply-substitution subst type))))

;;;
;;; Methods
;;;

(defmethod apply-substitution (subst-list (type ty-scheme))
  (make-ty-scheme
   :explicit-p (ty-scheme-explicit-p type)
   :kinds (ty-scheme-kinds type)
   :type (apply-substitution subst-list (ty-scheme-type type))))

(defmethod type-variables ((type ty-scheme))
  (type-variables (ty-scheme-type type)))

(defmethod kind-of ((type ty-scheme))
  (kind-of (fresh-inst type)))

(defmethod function-type-p ((type ty-scheme))
  (function-type-p (fresh-inst type)))

(defmethod function-return-type ((type ty-scheme))
  (to-scheme (function-return-type (fresh-inst type))))

(defmethod function-type-arguments ((type ty-scheme))
  (function-type-arguments (fresh-inst type)))

(defmethod remove-source-info ((scheme ty-scheme))
  (make-ty-scheme
   :explicit-p (ty-scheme-explicit-p scheme)
   :kinds (ty-scheme-kinds scheme)
   :type (remove-source-info (ty-scheme-type scheme))))
