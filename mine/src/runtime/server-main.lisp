;;;; server-main.lisp -- Runtime server entry point.
;;;;
;;;; When mine is invoked with --runtime-server, this module starts
;;;; the server loop that handles evaluation requests from the TUI.

(in-package #:mine/runtime/server-main)

;;; Soft reset -- clean editor state from inherited image

(defun %soft-reset ()
  "Prepare a clean subprocess for the runtime server.
NOTE: We must NOT delete editor packages here.  Their symbols are keys
in *global-environment*'s persistent maps (AVL trees ordered by
package-name then symbol-name).  Calling DELETE-PACKAGE uninterns those
symbols, mutating their SYMBOL-PACKAGE to NIL and silently breaking
the BST ordering invariant.  Subsequent lookups for unrelated types
then traverse wrong branches and fail with 'Unknown type' errors.
The packages are inert in the subprocess — leaving them costs nothing."
  ;; TODO: To reclaim the packages, first remove all Mine-defined entries
  ;; from *global-environment* (types, values, constructors, etc. whose
  ;; names are homed in MINE/ packages), then delete the packages.
  ;;
  ;; Clear ASDF registration for the editor system so it doesn't
  ;; interfere with user projects.
  (ignore-errors (asdf:clear-system "mine")))

;;; Entry point

(defun main (&optional port)
  "Start the runtime server on PORT (default: 0 = auto-assign).
PORT can be an integer or a string that will be parsed as an integer.
Blocks until the server finishes (via :quit message or connection loss)."
  (%soft-reset)
  (let ((port-number (etypecase port
                       (null 0)
                       (integer port)
                       (string (or (parse-integer port :junk-allowed t) 0)))))
    (format *error-output* ";; mine runtime server starting (port=~D)~%"
            port-number)
    (force-output *error-output*)
    (handler-case
        (mine/protocol/server:start-server port-number)
      (error (c)
        (format *error-output* ";; mine runtime server fatal error: ~A~%" c)
        (force-output *error-output*)
        (sb-ext:exit :code 1)))))
