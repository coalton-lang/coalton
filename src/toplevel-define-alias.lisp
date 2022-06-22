(in-package #:coalton-impl)

#|
The structure of an alias definition looks like this:

(define-type Card
  "A playing card, see https://en.wikipedia.org/wiki/Playing_card"
  (Card Kind Suite))

(define-alias Deck
  "A deck, AKA a list of cards" ; this is optional
  (List Card))

|#

(defstruct (partial-alias-entry (:constructor partial-alias-entry (alias docstring aliased)))
  (alias     (required 'alias)     :type symbol)
  (docstring (required 'docstring) :type (or string null))
  (aliased   (required 'aliased)   :type t)
  ;; AKA the tri-color abstraction: https://en.wikipedia.org/wiki/Tracing_garbage_collection#Tri-color_marking
  (expansion 'not-started          :type (member not-started in-progress finished)))

(defun complete-alias-entry (partial)
  "Turns a partial alias entry into a complete alias entry"
  (with-slots (alias docstring aliased expansion) partial
    (assert (eq expansion 'finished)
        () "Cannot complete un-expanded partial alias entry ~A" partial)
    (alias-entry alias docstring aliased)))

(defun process-alias-definitions (alias-defines env)
  "Processes a list of `define-alias` forms, expands them into concrete types, and adds them to the alias environment"
  (declare (type list alias-defines)
           (type environment env))
  (let ((new-aliases (make-immutable-map)))
    (dolist (alias-define alias-defines)
      (assert (and (alexandria:proper-list-p alias-defines)
                   (<= 3 (length alias-define) 4)
                   (eq (car alias-define) 'coalton:define-alias))
          () "Malformed DEFINE-ALIAS form ~A" alias-define)
      (let* ((alias     (second alias-define))
             (docstring (and (= (length alias-define) 4) (third alias-define)))
             (aliased   (if docstring (fourth alias-define) (third alias-define))))
        (assert (and (symbolp alias) (not (keywordp alias)))
            () "Malformed alias name ~A" alias)
        (assert (or (null docstring) (stringp docstring))
            () "Malformed alias docstring ~A" docstring)
        (setf new-aliases (immutable-map-set new-aliases alias (partial-alias-entry alias docstring aliased)))))
    (labels
        ((expand-type (type)
           "Expands TYPE to remove all aliases in-place. Returns the type after expansion"
           (etypecase type
             ;; An existing alias entry
             (alias-entry (alias-entry-aliased type))
             ;; A new, possibly unexpanded alias entry
             (partial-alias-entry
              (with-slots (alias aliased expansion) type
                (case expansion
                  ((finished)    aliased)
                  ((in-progress) (error "Invalid recursive definition of alias ~A" alias))
                  ((not-started)
                   (setf expansion 'in-progress
                         aliased   (expand-type aliased)
                         expansion 'finished)
                   aliased))))
             ;; Type variable - already fully expanded
             (keyword type)
             ;; Either a concrete type or an alias
             (symbol
              (let ((alias-entry (or (immutable-map-lookup new-aliases type)
                                     (lookup-alias env type))))
                (if alias-entry
                    ;; This is a type alias
                    (expand-type alias-entry)
                    ;; This is a concrete type
                    type)))
             ;; Null - this shouldn't happen™, so error out
             (null (error "Cannot expand null type"))
             ;; A type constructor - apply to each of the arguments
             (list (cons (car type) (mapcar #'expand-type (cdr type)))))))
      (dolist (alias (immutable-map-keys new-aliases))
        (expand-type (immutable-map-lookup new-aliases alias)))
      (dolist (alias (immutable-map-keys new-aliases))
        (setf env (set-alias env alias (complete-alias-entry (immutable-map-lookup new-aliases alias)))))
      env)))
      
