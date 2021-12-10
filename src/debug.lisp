(in-package #:coalton-impl)

(defun print-value-db (&optional package)
  "Print the global value environment"
  (coalton-impl/typechecker::print-value-db *global-environment* package))

(defun print-type-db (&optional package)
  "Print the global type environment"
  (coalton-impl/typechecker::print-type-db *global-environment* package))

(defun print-class-db (&optional package)
  "Print the global class environment"
  (coalton-impl/typechecker::print-class-db *global-environment* package))

(defun print-instance-db (&optional package)
  "Print the global instance environment"
  (coalton-impl/typechecker::print-instance-db *global-environment* package))

(defun coalton:type-of (symbol)
  "Lookup the type of value SYMBOL in the global environment"
  (coalton-impl::lookup-value-type *global-environment* symbol))

(defun coalton:kind-of (symbol)
  "Lookup the kind of type SYMBOL in the global environment"
  (coalton-impl/typechecker::kind-of (coalton-impl/typechecker::type-entry-type (coalton-impl::lookup-type *global-environment* symbol))))

;;
;; Read table shenanigans below. You have been warned!
;;

;; Depth counter for read-coalton-toplevel-form
(defvar *coalton-read-depth*)

(defun read-coalton-toplevel-form (stream open-paren)
  "Read the form starting with #\( until the corresponding #\). If
this is a top level form then this will greedily absorb other forms
and wrap in a COALTON-TOPLEVEL block."
  (declare (ignore open-paren))

  ;; If we are not yet in a coalton form then *coalton-read-depth*
  ;; should be unbound. In this case, bind it to 0 and parse
  ;; greedily. Otherwise, increment the depth and just parse this
  ;; object.
  (let ((*coalton-read-depth*
          (if (boundp '*coalton-read-depth*)
              (1+ *coalton-read-depth*)
              0)))

    ;; Helper function to read the form until the corresponding close paren
    (labels ((get-next-object ()
               (loop :for val
                       :=
                       (let ((next-char (peek-char t stream nil :eof t)))
                         (cond
                           ((eql next-char :eof)
                            nil)
                           ;; If the next character is a close paren then
                           ;; just throw it away.
                           ((char= next-char #\))
                            (read-char stream t nil t)
                            nil)
                           ;; Otherwise, read the form normally
                           (t
                            (values (read stream t nil t) nil))))
                     :while val
                     :collect val))
             (collect-forms (forms)
               (when forms
                 (let* ((in-coalton-form t)
                        (coalton-forms
                          ;; Collect all adjacent valid toplevel coalton forms
                          (loop :for form := (car forms)
                                :while (and forms
                                        ;form
                                            in-coalton-form)
                                :for valid-coalton
                                  := (and (listp form)
                                          (member (first form) **special-operators**))
                                :if valid-coalton
                                  :do (pop forms)
                                :if valid-coalton
                                  :collect form
                                :if (not valid-coalton)
                                  :do (setf in-coalton-form nil))))
                   (cond
                     ;; If we have not collected any then try invalid
                     ((null coalton-forms)
                      ;; Collect all adjacent invalid toplevel coalton forms
                      (setf in-coalton-form nil)
                      (let ((non-coalton-forms
                              (loop :for form := (car forms)
                                    :while (and forms
                                        ;form
                                                (not in-coalton-form))
                                    :for valid-coalton
                                      := (and (listp form)
                                              (member (first form) **special-operators**))
                                    :if (not valid-coalton)
                                      :do (pop forms)
                                    :if (not valid-coalton)
                                      :collect form
                                    :else
                                      :do (setf in-coalton-form t))))
                        ;; Emit the invalid forms in a PROGN and continue
                        (append
                         (list (append
                                (list 'progn)
                                non-coalton-forms))
                         (collect-forms forms))))
                     (t
                      ;; Emit the valid forms in a COALTON-TOPLEVEL and continue
                      (append
                       (list (append
                              (list 'coalton:coalton-toplevel)
                              coalton-forms))
                       (collect-forms forms))))))))
      (cond
        ;; If we are the toplevel form then we want to absorb all other forms
        ((zerop *coalton-read-depth*)
         (let ((out
                 (append
                  ;; First, read the rest of this form until the first zero-depth #\)
                  (list (get-next-object))
                  ;; Then, slurp up all the rest of the forms in the file
                  (loop :for val := (read stream nil nil t)
                        :while val
                        :collect val))))

           (append '(progn)
                   (collect-forms out))))
        ;; Otherwise, read normally
        (t
         (get-next-object))))))

;; Define a coalton readtable identified by symbol COALTON:SYNTAX
(named-readtables:defreadtable coalton:syntax
  (:merge :standard)
  (:macro-char #\( #'read-coalton-toplevel-form))

(defun coalton:enable-coalton-toplevel-file ()
  "Enables the coalton readtable to read the file as a coalton toplevel form."
  (named-readtables:in-readtable coalton:syntax))
