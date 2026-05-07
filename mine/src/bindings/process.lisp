;;;; process.lisp -- SBCL-specific subprocess bindings.
;;;;
;;;; Thin wrappers around sb-ext:run-program for use from Coalton
;;;; via the `lisp` escape hatch.

(in-package #:mine/bindings/process)

;;; Launcher resolution

(defun %argv0-has-directory-component-p (argv0)
  (or (position #\/ argv0)
      #+win32 (position #\\ argv0)
      #+win32 (position #\: argv0)))

(defun %resolve-reexec-program (argv0)
  "Return the executable name/path to use when re-executing mine."
  (if (%argv0-has-directory-component-p argv0)
      (handler-case
          (namestring (truename (pathname argv0)))
        (error () argv0))
      argv0))

(defun reexec-program ()
  "Return the launcher program to use when spawning another mine process.

Bare command names are left untouched so `run-program' can search PATH.
Only names that already contain a directory component are canonicalized."
  (%resolve-reexec-program
   (or (sb-ext:posix-getenv "MINE_LAUNCHER")
       (first sb-ext:*posix-argv*)
       "mine")))

;;; Spawning

(defun spawn-subprocess (program args)
  "Spawn PROGRAM with ARGS as a child process.

PROGRAM is a pathname string; ARGS is a list of argument strings.
The child runs asynchronously (:wait nil).
  - stdin  is /dev/null (:input nil)
  - stdout is a readable stream (:output :stream)
  - stderr is a readable stream (:error :stream)

Returns the sb-ext:process object."
  (declare (type string program)
           (type list args))
  (sb-ext:run-program program args
                      :wait nil
                      :input nil
                      :output :stream
                      :error :stream
                      :search t))

;;; Queries

(defun process-alive-p (process)
  "Return T if PROCESS is still running, NIL otherwise."
  (sb-ext:process-alive-p process))

(defun process-exit-code (process)
  "Return the exit code of PROCESS, or NIL if it has not yet exited."
  (if (sb-ext:process-alive-p process)
      nil
      (sb-ext:process-exit-code process)))

;;; Control -- process-kill is in process-unix.lisp / process-win32.lisp

(defun process-wait (process)
  "Non-blocking wait: check whether PROCESS has exited.
Returns T if the process has exited, NIL if still running.
Does not block -- uses a zero timeout."
  (handler-case
      (sb-ext:process-wait process nil)
    ;; process-wait can signal if the process is already dead
    (error () t))
  (not (sb-ext:process-alive-p process)))
