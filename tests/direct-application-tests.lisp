(in-package #:coalton-tests)

(defun check-direct-application (initial toplevel-functions expected)
  (is (equalp
       (direct-application
        initial
        toplevel-functions
        (make-default-environment))
       expected)))

(alexandria:define-constant unit-type (coalton-impl/typechecker::to-scheme (coalton-impl/typechecker::qualify nil coalton-impl/typechecker::tUnit)) :test #'equalp)

(deftest test-direct-application ()
  ;;
  ;; f should be rewritten as a direct call
  ;;
  (check-direct-application
   ;; f 5
   (typed-node-application
    unit-type
    nil
    (typed-node-variable unit-type nil 'f)
    (list (typed-node-literal unit-type nil 5)))

   '((f . 1))

   (typed-node-direct-application
    unit-type
    nil
    unit-type
    'f
    (list (typed-node-literal unit-type nil 5))))

  ;;
  ;; g is only applied to one argument and should not be rewritten
  ;;
  (check-direct-application
   ;; g 5
   (typed-node-application
    unit-type
    nil
    (typed-node-variable unit-type nil 'g)
    (list (typed-node-literal unit-type nil 5)))

   '((g . 2))

   (typed-node-application
    unit-type
    nil
    (typed-node-variable unit-type nil 'g)
    (list (typed-node-literal unit-type nil 5))))

  ;;
  ;; f is shadowed and should not be rewritten
  ;;
  (check-direct-application
   ;; \f -> f 5
   (typed-node-abstraction
    unit-type
    nil
    (list (cons 'f unit-type))
    (typed-node-application
     unit-type
     nil
     (typed-node-variable unit-type nil 'f)
     (list (typed-node-literal unit-type nil 5)))
    nil)

   '((f . 1))

   (typed-node-abstraction
    unit-type
    nil
    (list (cons 'f unit-type))
    (typed-node-application
     unit-type
     nil
     (typed-node-variable unit-type nil 'f)
     (list (typed-node-literal unit-type nil 5)))
    nil))

  ;;
  ;; f is introduced as a function in a let binding
  ;; f should be rewritten throughout the let binding
  ;;
  (check-direct-application
   ;;
   ;; let
   ;;    x = f 5
   ;;    f x = 6
   ;;  in
   ;;      f 7
   (typed-node-let
    unit-type
    nil
    (list
     (cons 'x
           (typed-node-application
            unit-type
            nil
            (typed-node-variable unit-type nil 'f)
            (list (typed-node-literal unit-type nil 5))))
     (cons 'f
           (typed-node-abstraction
            unit-type
            nil
            (list (cons 'x unit-type))
            (typed-node-literal unit-type nil 6)
            nil)))
    (typed-node-application
     unit-type
     nil
     (typed-node-variable unit-type nil 'f)
     (list (typed-node-literal unit-type nil 7)))
    nil
    nil
    nil)

   nil

   (typed-node-let
    unit-type
    nil
    (list
     (cons 'x
           (typed-node-direct-application
            unit-type
            nil
            unit-type
            'f
            (list (typed-node-literal unit-type nil 5))))
     (cons 'f
           (typed-node-abstraction
            unit-type
            nil
            (list (cons 'x unit-type))
            (typed-node-literal unit-type nil 6)
            nil)))
    (typed-node-direct-application
     unit-type
     nil
     unit-type
     'f
     (list (typed-node-literal unit-type nil 7)))
    nil
    nil
    nil))

  ;;
  ;; f should not be rewritten because it is shadowed by the match binding
  ;;
  (check-direct-application
   ;;
   ;; match x of
   ;;   f -> f 5
   ;;
   (typed-node-match
    unit-type
    nil
    (typed-node-variable unit-type nil 'x)
    (list
     (typed-match-branch
      nil
      (pattern-var 'f)
      (typed-node-application
       unit-type
       nil
       (typed-node-variable unit-type nil 'f)
       (list (typed-node-literal unit-type nil 5)))
      nil
      nil)))

   '((f . 1))

   (typed-node-match
    unit-type nil
    (typed-node-variable unit-type nil 'x)
    (list
     (typed-match-branch
      nil
      (pattern-var 'f)
      (typed-node-application
       unit-type
       nil
       (typed-node-variable unit-type nil 'f)
       (list (typed-node-literal unit-type nil 5)))
      nil
      nil)))))
