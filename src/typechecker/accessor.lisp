(defpackage #:coalton-impl/typechecker/accessor
  (:use
   #:cl
   #:coalton-impl/typechecker/base)
  (:local-nicknames
   (#:se #:source-error)
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:accessor                           ; STRUCT
   #:make-accessor                      ; CONSTRUCTOR
   #:accessor-from                      ; ACCESSOR
   #:accessor-to                        ; ACCESSOR
   #:accessor-field                     ; ACCESSOR
   #:accessor-source                    ; ACCESSOR
   #:accessor-list                      ; TYPE
   #:base-type                          ; FUNCTION
   #:solve-accessors                    ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/accessor)

(defstruct (accessor
            (:copier nil))
  (from   (util:required 'from)   :type tc:ty  :read-only t)
  (to     (util:required 'to)     :type tc:ty  :read-only t)
  (field  (util:required 'field)  :type string :read-only t)
  (source (util:required 'source) :type cons   :read-only t))

(defun accessor-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'accessor-p x)))

(deftype accessor-list ()
  '(satisfies accessor-list-p))

(defun base-type (ty)
  (declare (type tc:ty ty)
           (values tc:ty &optional))
  (cond
    ((tc:tycon-p ty)
     ty)

    ((tc:tyvar-p ty)
     ty)

    ((tc:tapp-p ty)
     (base-type (tc:tapp-from ty)))

    (t
     (util:unreachable))))

(defun solve-accessors (accessors file env)
  (declare (type accessor-list accessors)
           (type se:file file)
           (type tc:environment env))

  (let ((subs nil)
        (solved-accessors nil))

    (loop :with continue := t
          :while continue

          :for i :from 0

          :do (loop :for accessor :in accessors
                    :do (multiple-value-bind (matchp subs_)
                            (solve-accessor accessor file env)
                          (when matchp
                            (push accessor solved-accessors))
                          (setf subs (tc:compose-substitution-lists subs subs_))))

          :if solved-accessors
            :do (setf accessors
                      (tc:apply-substitution
                       subs
                       (set-difference accessors solved-accessors :test #'eq)))
          :else
            :do (setf continue nil)

          :do (setf solved-accessors nil))

    (values accessors subs)))

(defun solve-accessor (accessor file env)
  (declare (type accessor accessor)
           (type se:file file)
           (type tc:environment env)
           (values boolean tc:substitution-list))

  (let ((ty (base-type (accessor-from accessor))))

    (unless (tc:tycon-p ty)
      (return-from solve-accessor (values nil nil)))

    (let* ((ty-name (tc:tycon-name ty))

           (type-entry (tc:lookup-type env ty-name))

           (struct-ty (tc:apply-type-argument-list
                       (tc:type-entry-type type-entry)
                       (tc:type-entry-tyvars type-entry)))

           (struct-entry (tc:lookup-struct env ty-name :no-error t)))

      (unless struct-entry
        (error 'tc:tc-error
               :err (se:source-error
                     :span (accessor-source accessor)
                     :file file
                     :message "Invalid accessor"
                     :primary-note
                     (format nil "type '~S' is not a struct" ty-name))))

      (let ((subs (tc:match struct-ty (accessor-from accessor)))
            (field (tc:get-field struct-entry (accessor-field accessor) :no-error t)))

        (unless field
          (error 'tc:tc-error
                 :err (se:source-error
                       :span (accessor-source accessor)
                       :file file
                       :message "Invalid accessor"
                       :primary-note
                       (format nil "struct '~S' does not have the field '~A'"
                               ty-name
                               (accessor-field accessor)))))

        ;; the order of unification matters here
        (setf subs (tc:unify subs (accessor-to accessor)
                             (tc:struct-field-type field)))

        (values t subs)))))

(defmethod tc:apply-substitution (subs (accessor accessor))
  (declare (type tc:substitution-list subs)
           (values accessor))

  (make-accessor
   :from (tc:apply-substitution subs (accessor-from accessor))
   :to (tc:apply-substitution subs (accessor-to accessor))
   :field (accessor-field accessor)
   :source (accessor-source accessor)))

(defmethod tc:type-variables ((accessor accessor))
  (append (tc:type-variables (accessor-from accessor))
          (tc:type-variables (accessor-to accessor))))
