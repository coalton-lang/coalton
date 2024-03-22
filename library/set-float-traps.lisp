(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  #+ccl (ccl:set-fpu-mode :overflow nil :underflow nil :division-by-zero nil :invalid nil :inexact nil)
  #+sbcl (sb-int:set-floating-point-modes :traps nil))
