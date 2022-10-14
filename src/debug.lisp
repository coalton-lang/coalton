(defpackage #:coalton-impl/debug
  (:use #:cl)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:algo #:coalton-impl/algorithm)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton-impl/debug)

(defun coalton:print-value-db (&optional package)
  "Print the global value environment"
  (let ((env entry:*global-environment*)
        (sorted-by-package (make-hash-table)))
    ;; Sort the entires by package
    (fset:do-map (sym entry (algo:immutable-map-data (tc:environment-value-environment env)))
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

(defun coalton:print-type-db (&optional package)
  "Print the global type environment"
  (let ((env entry:*global-environment*)
        (sorted-by-package (make-hash-table)))
    ;; Sort the entires by package
    (fset:do-map (sym entry (algo:immutable-map-data (tc:environment-type-environment env)))
      (push (cons sym entry) (gethash (symbol-package sym) sorted-by-package)))

    ;; Print out the entries for each package

    (labels ((print-package (package entries)
               (format t "[package ~A]~%~%" (package-name package))
               (loop :for (name . entry) :in entries :do
                 (format t "  ~A :: ~A~%"
                         name
                         (tc:kind-of entry)))
               (format t "~%")))
      (if package
          (let ((p (find-package package)))
            (unless p
              (error "Invalid package ~A" package))
            (print-package p (gethash p sorted-by-package)))
          (maphash #'print-package sorted-by-package)))))

(defun coalton:print-class-db (&optional package)
  "Print the global class environment"
  (let ((env entry:*global-environment*)
        (sorted-by-package (make-hash-table)))
    ;; Sort the entires by package
    (fset:do-map (sym entry (algo:immutable-map-data (tc:environment-class-environment env)))
      (push (cons sym entry) (gethash (symbol-package sym) sorted-by-package)))

    ;; Print out the entries for each package

    (labels ((print-package (package entries)
               (format t "[package ~A]~%~%" (package-name package))
               (let ((*package* package))
                 (loop :for (name . entry) :in entries :do
                   (tc:with-pprint-variable-context ()
                     (let ((class-pred (tc:ty-class-predicate entry)))
                       (format t "  [~S (~A :: ~A)]~%"
                               (tc:ty-predicate-class class-pred)
                               (tc:ty-predicate-types class-pred)
                               (mapcar #'tc:kind-of (tc:ty-predicate-types class-pred))))
                     (loop :for (method-name . method-type) :in (tc:ty-class-unqualified-methods entry) :do
                       (format t "    ~S :: ~A~%" method-name method-type)))
                   (format t "~%")))
               (format t "~%")))
      (if package
          (let ((p (find-package package)))
            (unless p
              (error "Invalid package ~A" package))
            (print-package p (gethash p sorted-by-package)))
          (maphash #'print-package sorted-by-package)))))

(defun coalton:print-instance-db (&optional package)
  "Print the global instance environment"
  (let ((env entry:*global-environment*)
        (sorted-by-package (make-hash-table)))
    ;; Sort the entires by package
    (fset:do-map (sym entry (algo:immutable-map-data (tc:environment-class-environment env)))
      (push (cons entry (tc:lookup-class-instances env sym :no-error t))
            (gethash (symbol-package sym) sorted-by-package)))

    ;; Print out the entries for each package

    (labels ((print-package (package entries)
               (format t "[package ~A]~%~%" (package-name package))
               (let ((*package* package))
                 (loop
                   :for (entry . instances) :in entries
                   :when (not (null instances))
                     ;; Generate substitutions for class
                     :do (tc:with-pprint-variable-context ()
                           (let* ((class-pred (tc:ty-class-predicate entry)))
                             (format t "  [~S (~A :: ~A)]~%"
                                     (tc:ty-predicate-class class-pred)
                                     (tc:ty-predicate-types class-pred)
                                     (mapcar #'tc:kind-of (tc:ty-predicate-types class-pred)))))

                         (fset:do-seq (instance instances)
                           (format t "    ")
                           ;; Generate type variable substitutions from instance constraints
                           (tc:with-pprint-variable-context ()
                             (let* ((instance-constraints (tc:ty-class-instance-constraints instance))
                                    (instance-predicate (tc:ty-class-instance-predicate instance)))
                               (cond
                                 ((= 0 (length instance-constraints))
                                  (format t "~A~%" instance-predicate))
                                 ((= 1 (length instance-constraints))
                                  (format t "~A ~A ~A~%"
                                          (first instance-constraints)
                                          (if settings:*coalton-print-unicode* "⇒" "=>")
                                          instance-predicate))
                                 (t
                                  (format t "~A ~A ~A~%"
                                          instance-constraints
                                          (if settings:*coalton-print-unicode* "⇒" "=>")
                                          instance-predicate))))))

                         (format t "~%")))
               (format t "~%")))
      (if package
          (let ((p (find-package package)))
            (unless p
              (error "Invalid package ~A" package))
            (print-package p (gethash p sorted-by-package)))
          (maphash #'print-package sorted-by-package)))))

(defun coalton:print-specializations (&optional package)
  "Print all specializations"
  (let ((env entry:*global-environment*)
        (sorted-by-package (make-hash-table)))
    (fset:do-map (sym entry (algo:immutable-listmap-data (tc:environment-specialization-environment env)))
      (push (cons sym entry) (gethash (symbol-package sym) sorted-by-package)))

    (labels ((print-package (package entries)
               (format t "[package ~A]~%~%" (package-name package))
               (loop :for (name . specs) :in entries
                     :do (progn
                           (format t "  ~A :: ~A~%" name (tc:lookup-value-type env name))
                           (fset:do-seq (spec specs)
                             (format t "    ~A :: ~A~%"
                                     (tc:specialization-entry-to spec)
                                     (tc:specialization-entry-to-ty spec)))
                           (format t "~%")))
               (format t "~%")))
      (if package
          (let ((p (find-package package)))
            (unless p
              (error "Invalid package ~A" package))
            (print-package p (gethash p sorted-by-package)))
          (maphash #'print-package sorted-by-package)))))

(defun coalton:type-of (symbol)
  "Lookup the type of value SYMBOL in the global environment"
  (tc:lookup-value-type entry:*global-environment* symbol))

(defun coalton:kind-of (symbol)
  "Lookup the kind of type SYMBOL in the global environment"
  (tc:kind-of (coalton-impl/typechecker::type-entry-type (tc:lookup-type entry:*global-environment* symbol))))

(defun coalton:lookup-code (name)
  "Lookup the compiled code of a given definition"
  (declare (type symbol name))
  (tc:lookup-code entry:*global-environment* name))

(defun coalton:lookup-class (name)
  "Lookup a given class"
  (declare (type symbol name))
  (tc:lookup-class entry:*global-environment* name))

(defun coalton:lookup-fundeps (name)
  "Lookup the fundep structure for a given class"
  (declare (type symbol name))
  (tc:lookup-fundep-environment entry:*global-environment* name))
