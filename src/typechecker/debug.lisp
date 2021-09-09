(in-package #:coalton-impl/typechecker)

(defun print-value-db (env &optional package)
  (check-type env environment)
  (let ((sorted-by-package (make-hash-table)))
    ;; Sort the entires by package
    (fset:do-map (sym entry (shadow-realm-data (environment-value-environment env)))
      (push (cons sym entry) (gethash (symbol-package sym) sorted-by-package)))

    ;; Print out the entries for each package
    (labels ((print-package (package entries)
               (format t "[package ~A]~%~%" (package-name package))
               ;; Remove qualifications from package symbols
               (let ((*package* package))
                 (loop :for (name . type) :in entries :do
                   (format t "  ~A :: ~A~%"
		           name
		           type)))
               (format t "~%")))
      (if package
          (let ((p (find-package package)))
            (unless p
              (error "Invalid package ~A" package))
            (print-package p (gethash p sorted-by-package)))
	  (maphash #'print-package sorted-by-package)))))

(defun print-type-db (env &optional package)
  (check-type env environment)
  (let ((sorted-by-package (make-hash-table)))
    ;; Sort the entires by package
    (fset:do-map (sym entry (shadow-realm-data (environment-type-environment env)))
      (push (cons sym entry) (gethash (symbol-package sym) sorted-by-package)))

    ;; Print out the entries for each package

    (labels ((print-package (package entries)
               (format t "[package ~A]~%~%" (package-name package))
               (loop :for (name . entry) :in entries :do
                 (format t "  ~A :: ~A~%"
		         name
		         (kind-of entry)))
               (format t "~%")))
      (if package
          (let ((p (find-package package)))
            (unless p
              (error "Invalid package ~A" package))
            (print-package p (gethash p sorted-by-package)))
	  (maphash #'print-package sorted-by-package)))))

(defun print-class-db (env &optional package)
  (check-type env environment)
  (let ((sorted-by-package (make-hash-table)))
    ;; Sort the entires by package
    (fset:do-map (sym entry (shadow-realm-data (environment-class-environment env)))
      (push (cons sym entry) (gethash (symbol-package sym) sorted-by-package)))

    ;; Print out the entries for each package

    (labels ((print-package (package entries)
               (format t "[package ~A]~%~%" (package-name package))
               (let ((*package* package))
                 (loop :for (name . entry) :in entries :do
                   (with-pprint-variable-context ()
                     (let ((class-pred (ty-class-predicate entry)))
                       (format t "  [~S (~A :: ~A)]~%"
			       (ty-predicate-class class-pred)
			       (ty-predicate-types class-pred)
			       (mapcar #'kind-of (ty-predicate-types class-pred))))
                     (loop :for (method-name . method-type) :in (ty-class-unqualified-methods entry) :do
                       (format t "    ~S :: ~A~%" method-name method-type)))
                   (format t "~%")))
               (format t "~%")))
      (if package
          (let ((p (find-package package)))
            (unless p
              (error "Invalid package ~A" package))
            (print-package p (gethash p sorted-by-package)))
	  (maphash #'print-package sorted-by-package)))))

(defun print-instance-db (env &optional package)
  (check-type env environment)
  (let ((sorted-by-package (make-hash-table)))
    ;; Sort the entires by package
    (fset:do-map (sym entry (shadow-realm-data (environment-class-environment env)))
      (push (cons entry (lookup-class-instances env sym :no-error t))
            (gethash (symbol-package sym) sorted-by-package)))

    ;; Print out the entries for each package

    (labels ((print-package (package entries)
               (format t "[package ~A]~%~%" (package-name package))
               (let ((*package* package))
                 (loop
		   :for (entry . instances) :in entries
		   :when (not (null instances))
                     ;; Generate substitutions for class
                     :do (with-pprint-variable-context ()
			   (let* ((class-pred (ty-class-predicate entry)))
			     (format t "  [~S (~A :: ~A)]~%"
				     (ty-predicate-class class-pred)
		                     (ty-predicate-types class-pred)
                		     (mapcar #'kind-of (ty-predicate-types class-pred)))))

			 (fset:do-seq (instance instances)
			   (format t "    ")
			   ;; Generate type variable substitutions from instance constraints
			   (with-pprint-variable-context ()
			     (let* ((instance-constraints (ty-class-instance-constraints instance))
				    (instance-predicate (ty-class-instance-predicate instance)))
                               (cond
				 ((= 0 (length instance-constraints))
				  (format t "~A~%" instance-predicate))
				 ((= 1 (length instance-constraints))
				  (format t "~A ~A ~A~%"
					  (first instance-constraints)
					  (if *coalton-print-unicode* "⇒" "=>")
					  instance-predicate))
				 (t
				  (format t "~A ~A ~A~%"
					  instance-constraints
					  (if *coalton-print-unicode* "⇒" "=>")
					  instance-predicate))))))

			 (format t "~%")))
               (format t "~%")))
      (if package
          (let ((p (find-package package)))
            (unless p
              (error "Invalid package ~A" package))
            (print-package p (gethash p sorted-by-package)))
	  (maphash #'print-package sorted-by-package)))))
