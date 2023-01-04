(defpackage #:coalton-impl/parser/macro
  (:use
   #:cl
   #:coalton-impl/parser/base
   #:coalton-impl/parser/types
   #:coalton-impl/parser/pattern)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree))
  (:export
   #:expand-macro))

(in-package #:coalton-impl/parser/macro)

(defun expand-macro (form)
  "Expand the macro in FORM using MACROEXPAND-1, trying our best to preserve source information."
  (declare (type cst:cst form)
           (values cst:cst &optional))
  (let (;; Fallback to the macro source if unable to find something more specific.
        (fallback-source (cst:source form))
        ;; Table mapping forms within FORM to their sources. We will
        ;; check pointer equality of forms in the output of the macro
        ;; to these to retrieve source information.
        (source-table (make-hash-table :test #'eq)))
    (fill-source-table form source-table (make-hash-table :test #'eq))
    
    (rebuild-cst
     (macroexpand-1 (cst:raw form))
     source-table
     fallback-source
     (make-hash-table :test #'eq))))

(defun fill-source-table (cst source-table seen-forms)
  "Fill SOURCE-TABLE with source information in CST and its children."
  (declare (type cst:cst cst)
           (type hash-table source-table))
  (cond
    ;; If we have already seen this form then skip it.
    ((gethash cst seen-forms)
     nil)
    (t
     ;; When forms appear multiple times the later ones don't have any
     ;; source information. We can safely ignore these.
     (unless (or (cst:null cst)
                 (nth-value 1 (gethash (cst:raw cst) source-table)))
       (setf (gethash (cst:raw cst) source-table) (cst:source cst)))
     (when (cst:consp cst)
       (loop :for tail := cst :then (cst:rest tail)
             :while (cst:consp tail)
             :do (fill-source-table (cst:first tail) source-table seen-forms)
             :finally
                ;; Only walk the last form if it is not null. This is
                ;; only the case in dotted lists.
                (unless (cst:null tail)
                  (fill-source-table tail source-table seen-forms)))))))

(defun rebuild-cst (form source-table fallback-source seen-forms)
  "Rebuild a CST from the FORM.

SOURCE-TABLE contains a pointer mapping from macro input forms to source information.
FALLBACK-SOURCE is the source information of the macro to use as a fallback.
SEEN-FORMS is a hash table of known forms to prevent hang on cyclical list forms."
  (declare (type (or atom list) form)
           (type hash-table source-table seen-forms)
           (type cons fallback-source)
           (values cst:cst &optional))
  (let ((source (gethash form source-table fallback-source)))
    (cond ((nth-value 1 (gethash form seen-forms))
           (gethash form seen-forms))
          ((atom form)
           (make-instance
            'cst:atom-cst
            :raw form
            :source source))
          (t
           (let ((result (make-instance
                          'cst:cons-cst
                          :raw form
                          :source source)))
             (setf (gethash form seen-forms) result)
             (reinitialize-instance
              result
              :first (rebuild-cst (car form) source-table fallback-source seen-forms)
              :rest (rebuild-cst (cdr form) source-table fallback-source seen-forms)))))))
