;;;; Establishes some sane values for where to read code from and
;;;; where to write compiled code to. Enables separate, concurrent
;;;; compilation of different variants for the same Lisp environment.
;;;;
;;;; Essentially, tries to emulate javac's three arguments (search for
;;;; (x/3) in the code below):
;;;;
;;;; (1/3) --sourcepath foo:bar       Where is the code that we're compiling?
;;;;
;;;; (2/3) --classpath fu:bor   Where are the libraries/systems we depend on?
;;;;
;;;; (3/3) -d directory   What's the destination directory for compiled code?
;;;;
;;;; Along with (1-3/3), it attempts to collect/initialise all
;;;; required environment variables for the instance of the system
;;;; that we're building. It starts with these first, since it uses
;;;; their values for (3/3).

(require "asdf")

(in-package #:cl-user)

(defun get-var-or-default (var-name &key (default) (get-it #'uiop:getenvp))
  (declare (string var-name))
  (let ((val-from-env (funcall get-it var-name)))
    (if val-from-env
        val-from-env
        (if (not default)
            (error (format nil "~&The variable ~A has not been defined/exported"
                           var-name))
            (progn
              (format *error-output* "~&***WARNING***: variable ~A has not been defined/exported - using default value \"~A\"~%" var-name default)
              default)))))
(defun get-env-var-or-default (var-name &optional default)
  (get-var-or-default var-name :default default))
(defun get-file-var-or-default (var-name &optional default)
  (get-var-or-default var-name :default default :get-it #'uiop:safe-read-file-form))

;;; Get defining characteristics for this version of the current
;;; system:
(defparameter *system-name* "coalton")
(defparameter *system-version*
  (get-file-var-or-default "VERSION.txt"))
(defparameter *system-home*
  (identity ; uiop/pathname:ensure-absolute-pathname
   (get-env-var-or-default "COALTON_HOME")))
(defparameter *build-dir-prefix-name*
  (get-env-var-or-default "BLDDIR" "~/.cache/common-lisp"))
(defparameter *quicklisp-home*
  (get-env-var-or-default "QUICKLISP_HOME" "~/quicklisp"))
(defparameter *current-lisp* (uiop:implementation-identifier))
(defparameter *safety*
  (parse-integer (format nil "~A" (get-env-var-or-default "SAFETY" "0"))))
(defparameter *coalton-env*
  (get-env-var-or-default "COALTON_ENV" "development"))
(defparameter *coalton-disable-specializations*
  (get-env-var-or-default "COALTON_DISABLE_SPECIALIZATION" "0"))
(defparameter *coalton-heuristic-inlining*
  (get-env-var-or-default "COALTON_HEURISTIC_INLINING" "1"))
(defparameter *system-full-name*
  (format nil "~{~a~^-~}"
          ;; format string idea from
          ;; https://groups.google.com/g/comp.lang.lisp/c/Ac_FqxHKdnA?pli=1
          (list *system-name*
                *system-version*
                *coalton-env*
                *safety*
                *coalton-disable-specializations*
                *coalton-heuristic-inlining*
                *current-lisp*)))
(defparameter *build-dir-name* (concatenate 'string *build-dir-prefix-name*
                                            "/"
                                            *system-full-name*))
(defparameter *build-dir* (uiop:truename* *build-dir-name*))

;;; Testing - print out defining characteristics:
#+nil
(format t
        (concatenate 'string
                     "~&"
                     "*system-name*: ~A~%"
                     "*system-version*: ~A~%"
                     "*coalton-env*: ~A~%"
                     "*safety*: ~A~%"
                     "*coalton-disable-specializations*: ~A~%"
                     "*coalton-heuristic-inlining*: ~A~%"
                     "*current-lisp*: ~A~%"
                     "*system-full-name*: ~A~%"
                     "~%*system-home*: ~A~%"
                     "*build-dir-prefix-name*: ~A~%"
                     "*build-dir-name*: ~A~%"
                     "*build-dir*: ~A~%")
        *system-name*
        *system-version*
        *coalton-env*
        *safety*
        *coalton-disable-specializations*
        *coalton-heuristic-inlining*
        *current-lisp*
        *system-full-name*
        *system-home*
        *build-dir-prefix-name*
        *build-dir-name*
        *build-dir*)

(format *error-output* (concatenate 'string
                                    "~&This is system ~A~%"
                                    "  Starting with asdf:*source-registry-parameter* ~A~%"
                                    "  Starting with asdf:*central-registry* ~A~%"
                                    "  Starting with build config: ~A~%")
        *system-full-name*
        asdf:*source-registry-parameter*
        asdf:*central-registry*
        (asdf/output-translations:output-translations))

;; ;;; Clear all configuration information.
;; (asdf:clear-configuration)

;;; Define path to libraries/systems used:
;;;
(asdf:initialize-source-registry
 `(:source-registry
   ;;; (1/3) Equivalent to javac's --sourcepath
   ;; ALWAYS first - Define where the current system's source code is:
   (:tree
    ,(uiop:truename* *system-home*))
   ;;; (2/3) Equivalent to javac's --classpath (the two following trees)
   ;; Second, check ~/quicklisp/local-projects/ - we can lock
   ;; libraries here, hooray!
   (:tree
    ,(uiop:truename* (concatenate 'string *quicklisp-home* "/local-projects/")))
   ;; Last, check ~/quicklisp/ - allow quicklisp to bring in new
   ;; versions for these (so it can bite us in the arse when we least
   ;; suspect it...).
   (:tree
    ,(uiop:truename* (concatenate 'string *quicklisp-home* "/")))
   ;; Maybe the initial configuration had some system paths/trees
   ;; defined as well - inherit them.
   :inherit-configuration))

;;; Testing - ensure the path to source & libraries is correct:
#+nil
(format t "~&asdf:*central-registry*: ~A~%" asdf:*central-registry*)

;;; Define where the output (compiled) files should be saved:
;; Testing - show the current output translations:
#+nil
(format t "~&output-translations was ~A~%"
        (asdf/output-translations:output-translations))
;;; *Change* the output translations
(let ((full-path-spec
       (uiop:parse-unix-namestring
        (uiop/pathname:ensure-absolute-pathname
         #+nil(uiop:wilden *build-dir-name*) ; doesn't work - bizarre
         (concatenate 'string *build-dir-name*
                      "/**/*.*")))))
  ;; Inspired by the following but more spartan https://stackoverflow.com/questions/59698446/change-path-of-compiled-files-in-the-asdf
  ;;
  ;; Remove the defaults altogether
  ;;; (3/3) Equivalent to javac's -d
  (setf (asdf/output-translations:output-translations)
        `( (,full-path-spec t)
           (t ,full-path-spec) )))

;; Tell user what you're going to use.
(format *error-output*
        (concatenate 'string
                     "~&This is system ~A~%"
                     "  Now using asdf:*source-registry-parameter* ~A~%"
                     "  Now using asdf:*central-registry* ~A~%"
                     "  Now building in: ~A~%")
        *system-full-name*
        asdf:*source-registry-parameter*
        asdf:*central-registry*
        (asdf/output-translations:output-translations))

;;; Use the safety level, if possible.
#+sbcl(sb-ext:restrict-compiler-policy 'safety *safety*)
#+abcl(setq system:*safety* *safety*)

(defun load-n-test ()
  ; (load "~/quicklisp/setup") (ql:quickload :coalton/tests)
;;; We no longer need quicklisp - nothing to download!
  (asdf:load-system :coalton/tests)

  (asdf:test-system :coalton/tests)

  (asdf:load-system :small-coalton-programs))

;;; Let's see what's happening
#+nil(trace load-n-test)

(load-n-test)

(format t "~&~A: FINISHED FINE!~%" *system-full-name*)
