(in-package #:coalton-user)

;; Define IF as a macro to prevent branches from eagerly being evaluated
(cl:defmacro coalton:if (expr then else)
  `(match ,expr
     ((True) ,then)
     ((False) ,else)))

(cl:defmacro coalton:when (expr then)
  `(if ,expr
       (seq
	,then
	Unit)
       Unit))

(cl:defmacro coalton:unless (expr then)
  `(if ,expr
       Unit
       (seq
	,then
	Unit)))

(cl:defmacro coalton:cond (cl:&rest exprs)
  (cl:labels ((build-calls (exprs)
                (cl:if (cl:null (cl:cdr exprs))
                       `(coalton:if ,(cl:caar exprs)
                                    ,(cl:cadar exprs)
                                    (lisp :a ()  (cl:error "Non-exhaustive COND")))
                       `(coalton:if ,(cl:caar exprs)
                                    ,(cl:cadar exprs)
                                    ,(build-calls (cl:cdr exprs))))))
    (build-calls exprs)))

(cl:defmacro nest (cl:&rest items)
  "A syntactic convenience for function application. Transform

    (NEST f g h x)

to

    (f (g (h x)))."
  (cl:assert (cl:<= 2 (cl:list-length items)))
  (cl:let ((last (cl:last items))
           (butlast (cl:butlast items)))
    (cl:reduce (cl:lambda (x acc)
                 (cl:list x acc))
               butlast :from-end cl:t :initial-value (cl:first last))))

(cl:defmacro pipe (cl:&rest items)
  "A syntactic convenience for function application, sometimes called a \"threading macro\". Transform

    (PIPE x h g f)

to

    (f (g (h x)))."
  (cl:assert (cl:<= 2 (cl:list-length items)))
  `(nest ,@(cl:reverse items)))

(cl:defmacro make-list (cl:&rest forms)
  (cl:labels
      ((list-helper (forms)
	 (cl:if (cl:endp forms)
		`coalton-user:Nil
		`(coalton-user:Cons ,(cl:car forms) ,(list-helper (cl:cdr forms))))))
    (list-helper forms)))

(cl:defmacro to-boolean (expr)
  "Convert generalized boolean lisp expression EXPR to coalton boolean"
  `(cl:if (cl:the cl:boolean ,expr)
          True
          False))

(cl:defmacro coalton:do (cl:&rest forms)
  (cl:labels ((process (forms)
                (cl:let ((form (cl:car forms)))
                  (cl:cond ((cl:not (cl:listp form))
                            ;; If it is not a list then simply emit the form
                            form)
                           (;; If we are on the last one then just emit the form
                            (cl:null (cl:cdr forms))

                            (cl:when (cl:member 'coalton:<- form)
                              (cl:error "Last element of DO block cannot be a binding"))

                            form)
			   (;; If the form is a let binding
			    (cl:and
			     (cl:= 4 (cl:length form))
			     (cl:eql 'coalton:let (cl:first form))
			     (cl:symbolp (cl:second form))
			     (cl:eql 'coalton:= (cl:third form)))
			    `(let ((,(cl:second form) ,(cl:fourth form)))
			       ,(process (cl:cdr forms))))

			   ((cl:eql 'coalton:let (cl:first form))
			    (cl:error "Invalid let form in do expression"))

                           (;; Otherwise if we are a binding we can use >>=
                            (cl:and
                             (cl:= 3 (cl:length form))
                             (cl:eql 'coalton:<- (cl:second form)))
                            
                            (cl:let ((binding-name (cl:first form))
                                     (binding-value (cl:third form)))
                              
                              `(>>= ,binding-value
                                    (fn (,binding-name)
                                      ,(process (cl:cdr forms))))))
                           (;; Or just perform the action.
                            cl:t

                            (cl:when (cl:member 'coalton:<- form)
                              (cl:error "Malformed DO notation form ~A" form))

                            `(>> ,form
                                 ,(process (cl:cdr forms))))))))
    (process forms)))

(cl:defmacro coalton:progn (cl:&rest forms)
  (cl:assert (cl:< 0 (cl:length forms)) () "Malformed progn block.")
  (cl:labels ((process (forms)
		(cl:if (cl:= 1 (cl:length forms))
		       (cl:car forms)
		       (cl:let ((before-let cl:nil))
			 (cl:loop :for form :in forms :do
			   (cl:progn

			     (cl:cond
			       ((cl:and
				 (cl:listp form)
				 (cl:eql 'coalton:let (cl:first form))
				 (cl:eql 'coalton:= (cl:third form))
				 (cl:symbolp (cl:second form)))
				(cl:progn
				  (cl:assert
				   (cl:< (cl:+ 1 (cl:length before-let)) (cl:length forms)) ()  "Progn cannot be terminated by let")
				  (cl:return-from process
				    `(coalton:seq
				      ,@(cl:reverse before-let)
				      (coalton:let ((,(cl:second form) ,(cl:fourth form)))
					,(process (cl:nthcdr (cl:+ 1 (cl:length before-let)) forms)))))))

			       ((cl:and
				 (cl:listp form)
				 (cl:eql 'coalton:let (cl:first form)))
				(cl:error "Invalid let form in progn expression"))

			       (cl:t (cl:push form before-let)))
			     ))

			 ;; There was never a let generate a simple seq
			 `(coalton:seq
			   ,@forms)
			 ))))
    (process forms)))
