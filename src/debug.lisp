(defpackage #:coalton-impl/debug
  (:use #:cl)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:map #:coalton-impl/algorithm/hamt)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton-impl/debug)

(defun group-symbols-by-package (m)
  (map:reduce (lambda (m sym entry)
                (map:assoc m
                           (symbol-package sym)
                           (cons (cons sym entry)
                                 (map:get m (symbol-package sym)))))
              m))

(defun print-symbols (m f package)
  (let* ((grouped-symbols (group-symbols-by-package m))
         (packages (if package
                       (list (or (find-package package)
                                 (error "Invalid package ~A" package)))
                       (sort (map:keys grouped-symbols)
                             #'string< :key #'package-name))))
    (dolist (package packages)
      (funcall f package (map:get grouped-symbols package)))))

(defun coalton:print-value-db (&optional (env entry:*global-environment*) package)
  "Print the global value environment"
  (print-symbols (tc:environment-value-environment env)
                 (lambda (package entries)
                   (format t "[package ~A]~%~%" (package-name package))
                   ;; Remove qualifications from package symbols
                   (let ((*package* package))
                     (loop :for (name . type) :in entries :do
                       (format t "  ~A :: ~A~%" name type)))
                   (format t "~%"))
                 package))

(defun coalton:print-type-db (&optional (env entry:*global-environment*) package)
  "Print the global type environment"
  (print-symbols (tc:environment-type-environment env)
                 (lambda (package entries)
                   (format t "[package ~A]~%~%" (package-name package))
                   (loop :for (name . entry) :in entries :do
                     (format t "  ~A :: ~A~%" name (tc:kind-of entry)))
                   (format t "~%"))
                 package))

(defun coalton:print-class-db (&optional (env entry:*global-environment*) package)
  "Print the global class environment"
  (print-symbols (tc:environment-class-environment env)
                 (lambda (package entries)
                   (format t "[package ~A]~%~%" (package-name package))
                   (let ((*package* package))
                     (loop :for (name . entry) :in entries :do
                       (tc:with-pprint-variable-context ()
                         (let ((class-pred (tc:ty-class-predicate entry)))
                           (format t "  [~S (~A :: ~A)]~%"
                                   (tc:ty-predicate-class class-pred)
                                   (tc:ty-predicate-types class-pred)
                                   (mapcar #'tc:kind-of (tc:ty-predicate-types class-pred))))
                         (loop :for method :in (tc:ty-class-unqualified-methods entry) :do
                           (format t "    ~S :: ~A~%"
                                   (tc:ty-class-method-name method)
                                   (tc:ty-class-method-type method))))
                       (format t "~%")))
                   (format t "~%"))
                 package))

(defun coalton:print-instance-db (&optional (env entry:*global-environment*) package)
  "Print the global instance environment"
  (print-symbols (tc:environment-class-environment env)
                 (lambda (package entries)
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
                             (dolist (instance instances)
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
                   (format t "~%"))
                 package))

(defun coalton:print-specializations (&optional (env entry:*global-environment*) package)
  "Print all specializations"
  (print-symbols (tc:environment-specialization-environment env)
                 (lambda (package entries)
                   (format t "[package ~A]~%~%" (package-name package))
                   (loop :for (name . specs) :in entries
                         :do (progn
                               (format t "  ~A :: ~A~%" name (tc:lookup-value-type env name))
                               (dolist (spec specs)
                                 (format t "    ~A :: ~A~%"
                                         (tc:specialization-entry-to spec)
                                         (tc:specialization-entry-to-ty spec)))
                               (format t "~%")))
                   (format t "~%"))
                 package))

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
