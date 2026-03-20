;;;; An attempt to discover packages that abcl/clasp/ecl have problems
;;;; with.
;;;;
;;;; mkdir -p zz-temp ; rm -rf ~/.cache/co* ; for LISP in sbcl ccl abcl clasp ecl ; do (${LISP} --load ~/quicklisp/setup.lisp --load compat/test-packages.lisp > zz-temp/test-package-${LISP}.txt 2>&1 &); done

(defvar *cannot-load* nil)
(defvar *testing-fails* nil)

#+sbcl (push :concrete-syntax-tree *testing-fails*)
;; #+ccl (push :spinneret *testing-fails*) ;; needed extra packages.

#+abcl (push :concrete-syntax-tree *testing-fails*) ; + eclector yason concrete-syntax-tree

#+clasp (push :spinneret *cannot-load*) ;; reason :serapeum
#+clasp (dolit (pkg '(:trivial-garbage :float-features :fset :bordeaux-threads))
               (push pkg *testing-fails*))

#+ecl (push :spinneret *cannot-load*)
#+ecl (push :mgl-pax *testing-fails*)

(defvar *coalton-depends-on* '(;; From coalton.asd
                               :trivial-garbage :alexandria
                               :computable-reals
                               #+sbcl :sb-mpfr
                               #+sbcl :sb-gmp
                               :fiasco :trivial-benchmark :yason
                               :html-entities
                               #-(or clasp ecl) :spinneret
                               :uiop

                               ;; From coalton-compiler.asd
                               #-(or sbcl abcl) :concrete-syntax-tree
                               :eclector
                               :eclector-concrete-syntax-tree
                               :float-features :fset :named-readtables
                               :source-error :trivial-gray-streams))


(defvar *packages-used* '(;; Loaded from their repos.
                          ;;
                          ;; grep -v coalton ~/quicklisp/local-projects/system-index.txt | while read f ; do echo :`basename $f .asd` ; done | grep -v :named-readtables-test
                          :fset
                          :misc-extensions
                          :named-readtables

                          ;; Loaded from QuickLisp.
                          ;;
                          ;; for f in ~/quicklisp/dists/quicklisp/software/*/*.asd ; do echo :`basename $f .asd` ; done | (tr '\n' ' ' ; echo '')
                          :acclimation :alexandria
                          #-clasp :bordeaux-threads :cl-ppcre-unicode
                          :cl-ppcre :closer-mop :computable-reals
                          :concrete-syntax-tree :documentation-utils
                          :multilang-documentation-utils
                          :eclector-concrete-syntax-tree :eclector
                          :eclector.syntax-extensions :fiasco
                          :float-features-tests :float-features
                          :global-vars-test :global-vars :html-entities
                          :in-nomine :introspect-environment-test
                          :introspect-environment :iterate :lisp-namespace
                          :lisp-namespace.test ;; :mgl-pax-bootstrap
                          ;; :mgl-pax-test
                          #-ecl :mgl-pax ;; :mgl-pax.asdf
                          :mt19937
                          :parse-declarations-1.0 :parse-number
                          :serapeum :spinneret :split-sequence
                          :string-case :trivia :trivia.balland2006
                          :trivia.benchmark :trivia.cffi :trivia.fset
                          :trivia.level0 :trivia.level1 :trivia.level2
                          :trivia.ppcre :trivia.quasiquote :trivia.test
                          :trivia.trivial :trivial-arguments
                          :trivial-cltl2 :trivial-features-tests
                          :trivial-features :trivial-file-size
                          :trivial-garbage :trivial-gray-streams-test
                          :trivial-gray-streams :trivial-indent
                          :trivial-macroexpand-all :type-i :type-i.test
                          :yason-tests :yason))

(dolist (pkg *coalton-depends-on*)
  (progn
    (cl:handler-case
        (when (not (member pkg *cannot-load*))
          (format *error-output* "~%XXX-loading ~A~%" pkg)
          (if (not (ql:quickload pkg))
              (push pkg *cannot-load*)
              (cl:handler-case
                  (when (not (member pkg *testing-fails*))
                    (format *error-output* "~%YYY-testing ~A~%" pkg)
                    (when (not (asdf:test-system pkg))
                      ;; Note: Some return T on failure (e.g., serapeum) :-(
                      (push pkg *testing-fails*)))
                (cl:error (c)
                  (declare (ignore c))
                  (push pkg *testing-fails*)))))
      (cl:error (c)
        (declare (ignore c))
        (push pkg *cannot-load*)))
    (values *cannot-load* *testing-fails*)))

(format *error-output*
        "~%These packages cannot be loaded: ~%~A~%"
        *cannot-load*)

(format *error-output*
        "~%These packages *really* fail their self-tests: ~%~A~%"
        *testing-fails*)

(format *error-output*
        "~%NOTE:~% Some packages return T even when they fail their self-tests (e.g., eclector).~%")

(uiop:quit 0)
