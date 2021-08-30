(in-package #:coalton-impl/codegen)

(defun compile-typeclass-dicts (preds context env)
  "Emit the dictionary arguments to pass in to application with PREDS"
  (declare (type ty-predicate-list preds)
           (type list context)
           (type environment env)
           (values list))
  
  (loop :for pred :in preds
	:collect
        (let ((context-pred (find pred context
                                  :key #'car
                                  :test #'equalp ; Test that we have the same type variables
                                  )))
          (cond
	    ((not (null context-pred))
             (cdr context-pred))
            #+ignore
            ((static-predicate-p pred)
	     (static-lookup-dict pred env))
            (t
             (lookup-dict pred context env))))))


(defun make-dict-plist (preds context env)
  "Emit a plist of (PRED . SYMBOL) for each predicate in PREDS"
  (declare (type ty-predicate-list preds)
           (type list context)
           (values list))
  
  (loop :for pred :in preds
        :collect
        (let ((context-pred (find pred context
                                  :key #'car
                                  :test #'equalp ; Test that we have the same type variables
                                  )))
          (cond
            ((not (null context-pred))
             context-pred)
            ((static-predicate-p pred)
             (coalton-impl::coalton-bug "Static predicates are not allowed to exist in types"))
            (t
             (or (lookup-dict pred context env)
                 (coalton-impl::coalton-bug "Invalid state. Unable to find dict for pred: ~A context: ~A~%"
					    pred
					    context)))))))

(defun static-predicate-p (pred)
  "Is PRED a static predicate (no type variables)"
  (= 0 (length (coalton-impl/typechecker::type-variables (ty-predicate-types pred)))))

(defun lookup-dict (pred context env)
  "Lookup dictionary for predicate PRED"
  (or (lookup-in-context pred context env)
      (multiple-value-bind (instance subs)
          (lookup-class-instance env pred)
        (let* ((instance (coalton-impl/typechecker::apply-substitution subs instance))
               (instance-context (ty-class-instance-constraints instance)))
          (if (null instance-context)
              ;; Without constraints we can just return the symbol of the dict
              (ty-class-instance-codegen-sym instance)
              ;; Otherwise we need to recurse on all context dicts and build the new dict
              `(,(ty-class-instance-codegen-sym instance)
                ,@(mapcar (lambda (pred)
                            (lookup-dict pred context env))
                          instance-context)))))))

(defun lookup-in-context (pred context env)
  "Lookup the (PREDICATE . SYMBOL) value of PRED in context or in superclasses of predicates in CONTEXT"
  (let ((context-pred (find pred context
                            :key #'car
                            :test #'equalp)))
    ;; Either PRED is a member of context
    (when context-pred
      (return-from lookup-in-context (cdr context-pred)))

    (labels ((lookup-pred (pred ctx-pred method-accessor)
               "Try to match PRED to CTX-PRED or one of its superclasses. METHOD-ACCESSOR is the symbol to retrieve the "
               (let* ((ctx-class (lookup-class env (ty-predicate-class ctx-pred)))
                      ;; Find the substitutions to match our looked up class to the given CTX-PRED
                      (subs (coalton-impl/typechecker::predicate-match (ty-class-predicate ctx-class) ctx-pred))
                      (ctx-class (coalton-impl/typechecker::apply-substitution subs ctx-class)))
                 ;; If the predicate matches then we can use it
                 (cond
                   ((equalp pred ctx-pred)
                    (list method-accessor))
                   (t
                    (let ((superclass-ret
                            (some (lambda (super-dict-entry)
                                    (let* ((superclass-pred (car super-dict-entry))
                                           (superclass-accessor (cdr super-dict-entry))
                                           (superclass-accessor-symbol
                                             (alexandria:format-symbol (symbol-package superclass-accessor) "CLASS/~A-~A" (ty-predicate-class ctx-pred) superclass-accessor)))
                                      (lookup-pred pred superclass-pred superclass-accessor-symbol)))
                                  (ty-class-superclass-dict ctx-class))))
                      (when superclass-ret
                        (cons method-accessor superclass-ret))))))))
      (loop :for (ctx-pred . ctx-sym) :in context :do
        (let ((super (lookup-pred pred ctx-pred ctx-sym)))
          (when super
            (labels ((build-call (args)
                       (if (null (cdr args))
                           (car args)
                           (list (car args)
                                 (build-call (cdr args))))))
              (return-from lookup-in-context (build-call (reverse super))))))))
    
    ;; If we don't find anything then return nil
    nil))
