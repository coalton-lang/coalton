;; See https://github.com/Clozure/ccl/issues/441
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:set-fpu-mode :overflow nil :underflow nil :division-by-zero nil :invalid nil :inexact nil))
