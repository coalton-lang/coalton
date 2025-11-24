;;;; constants.lisp
;;;;
;;;; This file contains constant values used throughout compilation.

(defpackage #:coalton-impl/constants
  (:use #:cl)
  (:export
   #:+default-loop-label+               ; CONSTANT
   #:lisp-type-of-unit                  ; TYPE
   #:+value-of-unit+                    ; CONSTANT
   ))

(in-package #:coalton-impl/constants)

(defconstant +default-loop-label+ ':coalton-loop
  "Supplied as a loop label in while, while-let, for, loop, break and
continue when a label is not supplied by the user.")


;;; These are nothing special. Unit is just used a lot throughout
;;; compilation and is needed quite early. This seems like a
;;; reasonable enough place to put this. The actual definition of Unit
;;; as far as Coalton is concerned comes later.
(defconstant +value-of-unit+ 'coalton::Unit/Unit
  "The Lisp value of coalton:Unit. Needed early in compilation.")

(deftype lisp-type-of-unit ()
  '(member coalton::Unit/Unit))
