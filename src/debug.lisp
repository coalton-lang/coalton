(defpackage #:coalton-impl/debug
  (:use #:cl)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:algo #:coalton-impl/algorithm)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton-impl/debug)

(deftype package-designator ()
  '(or character package string symbol))

(defun coalton:print-value-db (&optional package)
  "Print the global value environment"
  (check-type package (or null package-designator) "package designator")
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
  (check-type package (or null package-designator) "package designator")
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
  (check-type package (or null package-designator) "package designator")
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
                     (loop :for method :in (tc:ty-class-unqualified-methods entry) :do
                       (format t "    ~S :: ~A~%"
                               (tc:ty-class-method-name method)
                               (tc:ty-class-method-type method))))
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
  (check-type package (or null package-designator) "package designator")
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
  "Print all specializations."
  (check-type package (or null package-designator) "package designator")
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
  "Lookup the type of value SYMBOL in the global environment. Return either

- a TY-SCHEME (which will pretty print as a Coalton type) representing the type of the symbol SYMBOL,

- the symbol :MACRO if SYMBOL names a macro, or

- NIL if the symbol isn't known to Coalton."
  (check-type symbol symbol)
  (if (macro-function symbol)
      ':macro
      (tc:lookup-value-type entry:*global-environment* symbol :no-error t)))

(defun coalton:describe-type-of (symbol)
  "Lookup the type of value SYMBOL in the global environment.

- If the symbol names a Coalton value, print the type and type aliases.

- If the symbol is a macro, print \"macro\".

- If the symbol is not known to Coalton, print \"unknown\". "
  (check-type symbol symbol)
  (when (macro-function symbol)
    (write-line "macro")
    (return-from coalton:describe-type-of (values)))
  (let ((tc:*coalton-type-printing-mode* ':types-and-aliases)
        (type (tc:lookup-value-type entry:*global-environment* symbol :no-error t)))
    (cond
      ((null type)
       (write-line "unknown"))
      (t
       (format t "~S~%" type)))
    (values)))

(defun coalton:describe-type-alias (symbol)
  "Lookup the type aliased by SYMBOL in the global environment and print information about it. If SYMBOL does not name a type, then print \"unknown\"."
  (check-type symbol symbol)
  (let ((tc::*coalton-type-printing-mode* :types-and-aliases)
        (alias (tc:lookup-type-alias entry:*global-environment* symbol :no-error t)))
    (cond
      ((null alias)
       (write-line "unknown"))
      (t
       (tc:with-pprint-variable-context ()
         (format t "~S~%" (tc:type-alias-entry-type alias)))))
    (values)))

(defun coalton:set-type-printing-mode (mode)
  "Set the type printing mode for the display of types.

MODE must be one of

:TYPES             only display the types of symbols
:ALIASES           only display the aliases of the types of symbols
:TYPES-AND-ALIASES display types and the aliases that refer to them."
  (unless (member mode '(:types :aliases :types-and-aliases))
    (error "Invalid type printing mode ~A, must be :TYPES, :ALIASES, or :TYPES-AND-ALIASES." mode))
  (setf tc:*coalton-type-printing-mode* mode))

(defun coalton:kind-of (symbol)
  "Lookup the kind of type SYMBOL in the global environment. If it's not
known, return NIL."
  (check-type symbol symbol)
  (let ((type (tc:lookup-type entry:*global-environment* symbol :no-error t)))
    (cond
      ((null type)
       nil)
      (t
       (tc:kind-of (tc:type-entry-type type))))))

(defun coalton:lookup-code (name)
  "Lookup the compiled code of a given definition. Return NIL if the
name is not known."
  (declare (type symbol name))
  (tc:lookup-code entry:*global-environment* name :no-error t))

(defun coalton:lookup-class (name)
  "Lookup a given class. Return NIL if the name is not known."
  (declare (type symbol name))
  (tc:lookup-class entry:*global-environment* name :no-error t))

(defun coalton:lookup-type (name)
  "Lookup a given type. Return NIL if the name is not known."
  (declare (type symbol name))
  (tc:lookup-type entry:*global-environment* name :no-error t))

(defun coalton:lookup-runtime-type (name)
  "Lookup the runtime type of the given type. Return NIL if the name is not known."
  (declare (type symbol name))
  (let ((type (coalton:lookup-type name)))
    (typecase type
      (tc:type-entry (tc:type-entry-runtime-type type))
      (t             nil))))

(defun coalton:lookup-fundeps (name)
  "Lookup the fundep structure for a given class. Return NIL if the name
is not known."
  (declare (type symbol name))
  (tc:lookup-fundep-environment entry:*global-environment* name :no-error t))
