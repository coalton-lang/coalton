(uiop:define-package #:coalton-impl/algorithm
  (:documentation "Implementation of generic algorithms used by COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl)
  (:mix-reexport
   #:coalton-impl/algorithm/tarjan-scc
   #:coalton-impl/algorithm/immutable-map
   #:coalton-impl/algorithm/immutable-listmap))
