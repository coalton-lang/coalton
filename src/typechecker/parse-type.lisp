;;;; parse-type.lisp

(in-package #:coalton-impl/typechecker)

;;;
;;; Type parsing
;;;

;;
;; A valid type signature is defined as follows:
;;
;; type-signature := <type-expr>                ; Type with no constraints
;;                 | (<pred> => <type-expr>)    ; Type with single constraint
;;                 | ((<pred>)+ => <type-expr>) ; Type with multiple constraints
;;
;; NOTE: either => or ⇒ can be used in type constraint expressions.
;;
;; pred := <class> <type-expr>+
;;
;; type-expr := <tyvar>                         ; Type variable
;;            | <0-arity tycon>                 ; Zero arity type constructor
;;            | (<tycon> <type-expr>+)          ; Applied type constructor
;;            | (<type-expr> [-> <type-expr>]+) ; Function type
;;
;; NOTE: either -> or → can be used in function type expressions.
;;
;; tyvar := symbol in the keyword package (e.g. :a, :b, etc.)
;;

(alexandria:define-constant keyword-package (find-package "KEYWORD") :test #'equalp)

(defun parse-and-resolve-type (env top-expr &optional type-vars disallowed-type-vars additional-predicates)
  "Parse the type expression EXPR in environment ENV returning a TY-SCHEME

Optional TYPE-VARS is an ALIST specifying type variables within the current parsing context
Optional DISALLOWED-TYPE-VARS is a list specifying the type variables which should be excluded from type quantification
Optional ADDITIONAL-PREDICATES specifys additional predicates to qualify the resulting type over"
  (declare (type environment env)
           (values ty-scheme &optional))

  ;; Parse out the expression and quantify over non-disallowed type variables
  (multiple-value-bind (parsed type-vars subs)
      (parse-qualified-type-expr env top-expr type-vars nil :additional-predicates additional-predicates)
    (apply-substitution subs
                        (quantify (remove-if
                                   (lambda (x) (member x disallowed-type-vars :test #'equalp))
                                   (type-variables (mapcar #'cadr type-vars)))
                                  parsed))))

(defun parse-type-expr (env expr type-vars subs &optional output-kind)
  "Parse type expression EXPR with the type variable context TYPE-VARS"
  (declare (type environment env)
           (type list type-vars)
           (values ty list substitution-list))
  (with-parsing-context ("type expression ~A" expr)
    (labels ((find-tyvar-entry (name)
               (find name type-vars :key #'car))
             (find-tyvar-entry-from-tvar (tvar)
               (find tvar type-vars :key #'cadr :test #'equalp))
             (resolve-type-variables (type kind)
               "Resolve kinds of all type variables in TYPE such that TYPE is of kind KIND"
               (etypecase type
                 (tvar
                  (let ((entry (find-tyvar-entry-from-tvar type)))
                    ;; If we don't know this tyvar then don't attempt
                    ;; to resolve, just check that it is the correct
                    ;; kind.
                    (if (null entry)
                        (progn
                          (unless (equalp kind (kind-of type))
                            (error 'kind-mismatch-error :type type :kind kind))
                          type)
                        (if (null (cddr entry))
                            ;; If the kind of the variable has not been
                            ;; resolved then set its kind and emit a
                            ;; substitution
                            (let* ((new-var (make-variable kind))
                                   (type-vars-with-entry-removed (remove-if
                                                                  (lambda (e)
                                                                    (equalp type (cadr e)))
                                                                  type-vars))
                                   (entry (cons (car entry) (cons new-var (kind-arity kind)))))
                              (setf type-vars (append type-vars-with-entry-removed (list entry)))
                              (push (%make-substitution (tvar-tyvar type) new-var) subs)
                              new-var)

                            ;; Otherwise, assert that we are resolved to the correct kind
                            (progn
                              (unless (and (= (cddr entry) (kind-arity kind))
                                           (equalp kind (kind-of (cadr entry))))
                                (error 'kind-mismatch-error :type (cadr entry) :kind kind))
                              type)))))
                 (tcon
                  ;; For type constructors, simply assert
                  ;; that we have the correct kind.
                  (unless (equalp kind (kind-of type))
                    (error 'kind-mismatch-error :type type :kind kind))
                  type)
                 (tapp
                  ;; For type applications, we will
                  ;; assume simple kind and require the
                  ;; applied type to be of kind *
                  (%make-tapp (resolve-type-variables (tapp-from type) (kFun kStar kind))
                              (resolve-type-variables (tapp-to type) kStar))))))
      (let ((type
              (etypecase expr
                ;; Symbols are either type constructors or type variables
                (symbol
                 (cond
                   ;; Type variable
                   ((equalp (symbol-package expr)
                            keyword-package)
                    (or (car (cdr (find-tyvar-entry expr)))
                        (let* ((var (make-variable))
                               ;; Create a type variable with unknown
                               ;; arity to be determined on usage
                               (entry (cons expr (cons var nil))))
                          (setf type-vars (append type-vars (list entry)))
                          var)))
                   ;; Type constructor in current scope
                   (t
                    (lookup-type env expr))))
                (list
                 (cond
                   ((some #'coalton-arrow-p expr)
                    (unless (oddp (length expr))
                      (error-parsing-type expr "Malformed function type"))

                    (let ((arg-types nil)
                          (function-type nil))
                      ;; Create argument types in the _correct_ order
                      (loop :for (type arrow) :on expr :by #'cddr
                            :for num :from 0
                            :do
                               ;; Unless we are at the last type, there must be arrows
                               (unless (or (= num (/ (1- (length expr)) 2))
                                           (coalton-arrow-p arrow))
                                 (error-parsing-type expr "Malformed function type"))

                               ;; Parse the type, adding any new type variables to the context
                               (multiple-value-bind (arg-type new-type-vars new-subs)
                                   (parse-type-expr env type type-vars subs)
                                 (setf type-vars new-type-vars
                                       subs new-subs)
                                 ;; Ensure we have a type of kind kStar
                                 (let ((resolved-arg-type (resolve-type-variables arg-type kStar)))
                                   (push resolved-arg-type arg-types))))
                      (dolist (arg-type arg-types)
                        (setf function-type
                              (if function-type
                                  (make-function-type arg-type function-type)
                                  arg-type)))
                      function-type))
                   (t
                    ;; NOTE: We might be able to remove this restriction if we allow constructing non-* types
                    (unless (symbolp (first expr))
                      (error-parsing-type expr "Invalid type constructor ~S" (first expr)))

                    (let* ((ty-con-arity (if output-kind
                                             (+ (length (rest expr)) (kind-arity output-kind))
                                             (length (rest expr))))
                           (ty-con-kind (make-kind-of-arity ty-con-arity))
                           (ty-con (multiple-value-bind (tcon new-type-vars new-subs)
                                       (parse-type-expr env (first expr) type-vars subs ty-con-kind)
                                     (setf type-vars new-type-vars
                                           subs new-subs)
                                     (resolve-type-variables tcon ty-con-kind)))
                           (arg-tys
                             (loop :for e :in (rest expr)
                                   :collect (multiple-value-bind (arg-type new-type-vars new-subs)
                                                (parse-type-expr env e type-vars subs)
                                              (setf type-vars new-type-vars
                                                    subs new-subs)
                                              arg-type))))
                      (apply-type-argument-list ty-con arg-tys))))))))
        (let ((output-type (apply-substitution subs type)))
          (values output-type type-vars subs))))))

(defun parse-qualified-type-expr (env expr type-vars subs &key
                                                            additional-predicates
                                                            additional-class-predicates
                                                            allow-unknown-classes)
  "Parse qualified type expression EXPR in type environment ENV

Optional ADDITIONAL-PREDICATES provides additional predicates to qualify over
Optional ADDITIONAL-CLASS-PREDICATES provides additional class predicates to use before looking in ENV
Optional ALLOW-UNKNOWN-CLASSES allows classes to appear in the type expression that are not defined in ENV or ADDITIONAL-CLASS-PREDICATES"
  (declare (type environment env)
           (type list type-vars)
           (values qualified-ty list substitution-list))
  (with-parsing-context ("qualified type expression ~A" expr)
    (let ((type
            (cond
              ;; If the expression is a list and contains => then it has constraints
              ((and (listp expr)
                    (some #'coalton-double-arrow-p expr))
               ;; Split the expression into parts before and after the arrow
               (let ((subseqs (split-sequence:split-sequence-if #'coalton-double-arrow-p expr)))
                 (unless (and (= 2 (length subseqs))
                              (= 1 (length (second subseqs))))
                   (error-parsing-type expr "Malformed constrained type"))
                 ;; If the first member of the predicates is a list then we can assume there are multiple to parse.
                 (let ((preds (if (listp (first (first subseqs)))
                                  (loop :for pred-expr :in (first subseqs)
                                        :collect (multiple-value-bind (pred new-type-vars new-subs)
                                                     (parse-type-predicate env pred-expr type-vars subs
                                                                           :allow-unknown-classes allow-unknown-classes
                                                                           :additional-class-predicates additional-class-predicates)
                                                   (setf type-vars new-type-vars
                                                         subs new-subs)
                                                   pred))
                                  (multiple-value-bind (pred new-type-vars new-subs)
                                      (parse-type-predicate env (first subseqs) type-vars subs
                                                            :allow-unknown-classes allow-unknown-classes
                                                            :additional-class-predicates additional-class-predicates)
                                    (setf type-vars new-type-vars
                                          subs new-subs)
                                    (list pred))))
                       (type (multiple-value-bind (type new-type-vars new-subs)
                                 (parse-type-expr env (first (second subseqs)) type-vars subs)
                               (setf type-vars new-type-vars
                                     subs new-subs)
                               type)))

                   ;; Ensure predicates can only constrain type variables in the type
                   (let ((tyvars (type-variables type)))
                     (dolist (pred preds)
                       (dolist (pred-tyvar (type-variables (apply-substitution subs pred)))
                         (unless (member pred-tyvar tyvars :test #'equalp)
                           (error-parsing-type expr "Type variable ~S appears in predicates but not in type"
                                               (car (find pred-tyvar
                                                          type-vars
                                                          :key (lambda (x)
                                                                 (tvar-tyvar (cadr x)))
                                                          :test #'equalp)))))))

                   (let* ((preds (append additional-predicates preds))
                          (reduced-preds (reduce-context env preds)))
                     (unless (null (set-exclusive-or preds reduced-preds))
                       (warn "Reduced context for type ~A~%   from ~{~A~^, ~} to ~{~A~^, ~}"
                             expr
                             preds
                             (or reduced-preds '("nothing"))))
                     (qualify reduced-preds type)))))
              ;; Otherwise parse as a type
              (t
               (multiple-value-bind (type new-type-vars new-subs)
                   (parse-type-expr env expr type-vars subs)
                 (setf type-vars new-type-vars
                       subs new-subs)
                 (qualify (reduce-context env additional-predicates) type))))))
      (values (apply-substitution subs type) type-vars subs))))

(defun parse-type-predicate (env expr type-vars subs &key
                                                       additional-class-predicates
                                                       allow-unknown-classes)
  "Parse type predicate EXPR in type environment ENV

Optional ADDITIONAL-CLASS-PREDICATES provides additional class predicates to use before looking in ENV
Optional ALLOW-UNKNOWN-CLASSES allows classes to appear in the type expression that are not defined in ENV or ADDITIONAL-CLASS-PREDICATES"
  (declare (type environment env)
           (type list type-vars)
           (values ty-predicate list substitution-list))
  (with-parsing-context ("type predicate ~A" expr)
    (unless (and (listp expr)
                 (>= (length expr) 2)
                 (symbolp (first expr)))
      (error-parsing-type expr "Malformed type predicate"))
    (let* ((pred-class (first expr))
           ;; Look up the class in the environment to find the kind of any type variables
           (class-pred (or (find pred-class additional-class-predicates :key #'ty-predicate-class)
                           (let ((class-entry (lookup-class env pred-class :no-error allow-unknown-classes)))
                             (and class-entry
                                  (ty-class-predicate class-entry)))))
           (class-pred-kinds (and class-pred
                                  (mapcar #'kind-of (ty-predicate-types class-pred))))

           (pred-types
             ;; We need two loops so that we can do with or without the kinds
             (if class-pred-kinds
                 (loop :for pred-expr :in (cdr expr)
                       :for pred-kind :in class-pred-kinds
                       :collect (multiple-value-bind (pred-type new-type-vars new-subs)
                                    (parse-type-expr env pred-expr type-vars subs pred-kind)
                                  (setf subs new-subs)
                                  (setf type-vars (append type-vars new-type-vars))
                                  pred-type))
                 (loop :for pred-expr :in (cdr expr)
                       :collect (multiple-value-bind (pred-type new-type-vars new-subs)
                                    (parse-type-expr env pred-expr type-vars subs nil)
                                  (setf subs new-subs)
                                  (setf type-vars (append type-vars new-type-vars))
                                  pred-type)))))

      (values (apply-substitution subs (ty-predicate pred-class pred-types))
              type-vars
              subs))))


;;;
;;; Arrows
;;;

(alexandria:define-constant +coalton-arrows+ '(coalton:-> coalton:→) :test #'equalp
  :documentation "Allowed arrows in coalton function type expressions.")

(alexandria:define-constant +coalton-double-arrows+ '(coalton:=> coalton:⇒) :test #'equalp
  :documentation "Allowed double arrows in coalton type constraint expressions.")

(defun arrow-p (symbol valid-symbols)
  (or (and (member symbol valid-symbols) t)
      (and (symbolp symbol)
           ;; Throw an error if a non-coalton arrow is used.
           ;; NOTE: This disallows users to declare types with the same name as coalton arrows.
           (dolist (sym valid-symbols)
             (when (string= (symbol-name sym) (symbol-name symbol))
               (error-parsing-type sym "Invalid coalton arrow ~S. Did you mean ~S" sym symbol)))
           nil)))

(defun coalton-arrow-p (symbol)
  (arrow-p symbol +coalton-arrows+))

(defun coalton-double-arrow-p (symbol)
  (arrow-p symbol +coalton-double-arrows+))
