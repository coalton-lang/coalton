;;;; platform-win32.lisp -- Windows terminal I/O via kernel32.dll console API.

(in-package #:mine/bindings/platform)

;;; Shared constant
(defconstant +read-buf-size+ 4096)

;;; ---- Constants ----

;; Standard device handles
(defconstant +std-input-handle+  #.(- (ash 1 32) 10))  ; (DWORD)-10
(defconstant +std-output-handle+ #.(- (ash 1 32) 11))  ; (DWORD)-11

;; Console input mode flags
(defconstant +enable-processed-input+        #x0001)
(defconstant +enable-line-input+             #x0002)
(defconstant +enable-echo-input+             #x0004)
(defconstant +enable-window-input+           #x0008)
(defconstant +enable-virtual-terminal-input+ #x0200)

;; Console output mode flags
(defconstant +enable-processed-output+            #x0001)
(defconstant +enable-virtual-terminal-processing+ #x0004)

;; WaitForSingleObject return values
(defconstant +wait-object-0+ 0)
(defconstant +wait-timeout+  #x102)

;;; ---- Foreign function bindings (kernel32.dll) ----

;; HANDLE GetStdHandle(DWORD nStdHandle)
(sb-alien:define-alien-routine ("GetStdHandle" %get-std-handle)
    (* t)
  (n-std-handle sb-alien:unsigned-int))

;; BOOL GetConsoleMode(HANDLE hConsole, LPDWORD lpMode)
(sb-alien:define-alien-routine ("GetConsoleMode" %get-console-mode)
    sb-alien:int
  (h-console (* t))
  (lp-mode (* sb-alien:unsigned-int)))

;; BOOL SetConsoleMode(HANDLE hConsole, DWORD dwMode)
(sb-alien:define-alien-routine ("SetConsoleMode" %set-console-mode)
    sb-alien:int
  (h-console (* t))
  (dw-mode sb-alien:unsigned-int))

;; BOOL GetConsoleScreenBufferInfo(HANDLE, PCONSOLE_SCREEN_BUFFER_INFO)
(sb-alien:define-alien-routine
    ("GetConsoleScreenBufferInfo" %get-console-screen-buffer-info)
    sb-alien:int
  (h-console (* t))
  (lp-info (* t)))

;; DWORD WaitForSingleObject(HANDLE hHandle, DWORD dwMilliseconds)
(sb-alien:define-alien-routine ("WaitForSingleObject" %wait-for-single-object)
    sb-alien:unsigned-int
  (h-handle (* t))
  (dw-milliseconds sb-alien:unsigned-int))

;; BOOL ReadFile(HANDLE, LPVOID, DWORD, LPDWORD, LPOVERLAPPED)
(sb-alien:define-alien-routine ("ReadFile" %read-file)
    sb-alien:int
  (h-file (* t))
  (lp-buffer (* t))
  (n-bytes-to-read sb-alien:unsigned-int)
  (lp-bytes-read (* sb-alien:unsigned-int))
  (lp-overlapped (* t)))

;; BOOL PeekNamedPipe(HANDLE, LPVOID, DWORD, LPDWORD, LPDWORD, LPDWORD)
(sb-alien:define-alien-routine ("PeekNamedPipe" %peek-named-pipe)
    sb-alien:int
  (h-pipe (* t))
  (lp-buffer (* t))
  (n-buf-size sb-alien:unsigned-int)
  (lp-bytes-read (* sb-alien:unsigned-int))
  (lp-total-bytes-avail (* sb-alien:unsigned-int))
  (lp-bytes-left-this-message (* sb-alien:unsigned-int)))

;; DWORD GetFileType(HANDLE hFile)
(sb-alien:define-alien-routine ("GetFileType" %get-file-type)
    sb-alien:unsigned-int
  (h-file (* t)))

;; BOOL SetConsoleCtrlHandler(PHANDLER_ROUTINE, BOOL)
(sb-alien:define-alien-routine ("SetConsoleCtrlHandler" %set-console-ctrl-handler)
    sb-alien:int
  (handler-routine (* t))
  (add sb-alien:int))

;;; ---- State ----

(defconstant +file-type-char+ 2)
(defconstant +file-type-pipe+ 3)

(defvar *stdin-handle* nil   "Console input handle.")
(defvar *stdout-handle* nil  "Console output handle.")
(defvar *stdin-is-pipe* nil  "T when stdin is a pipe (running under mine-app).")
(defvar *saved-input-mode* 0 "Original console input mode.")
(defvar *saved-output-mode* 0 "Original console output mode.")
(defvar *read-buf* nil       "Persistent read buffer (alien array).")
(defvar *bytes-read-buf* nil "Persistent DWORD buffer for ReadFile.")
(defvar *pipe-rows* nil      "Pipe-mode terminal rows (set by resize escape sequence).")
(defvar *pipe-cols* nil      "Pipe-mode terminal cols (set by resize escape sequence).")

;;; ---- Raw mode ----

(defun platform-enable-raw-mode ()
  "Put the Windows console into raw VT mode."
  ;; Get handles
  (setf *stdin-handle*  (%get-std-handle +std-input-handle+)
        *stdout-handle* (%get-std-handle +std-output-handle+))
  ;; Detect whether stdin is a pipe (mine-app) or a console (direct)
  (setf *stdin-is-pipe* (= (%get-file-type *stdin-handle*) +file-type-pipe+))
  ;; Save current modes (SetConsoleMode is a no-op on pipes)
  (let ((mode-buf (sb-alien:make-alien sb-alien:unsigned-int)))
    (unwind-protect
         (progn
           (%get-console-mode *stdin-handle* mode-buf)
           (setf *saved-input-mode* (sb-alien:deref mode-buf))
           (%get-console-mode *stdout-handle* mode-buf)
           (setf *saved-output-mode* (sb-alien:deref mode-buf)))
      (sb-alien:free-alien mode-buf)))
  ;; Set raw input mode: VT input, window events, no echo/line/processed
  (%set-console-mode *stdin-handle*
                     (logior +enable-virtual-terminal-input+
                             +enable-window-input+))
  ;; Set VT output mode
  (%set-console-mode *stdout-handle*
                     (logior +enable-processed-output+
                             +enable-virtual-terminal-processing+))
  (values))

(defun platform-disable-raw-mode ()
  "Restore the Windows console to its original mode."
  (when *stdin-handle*
    (ignore-errors
      (%set-console-mode *stdin-handle* *saved-input-mode*)
      (%set-console-mode *stdout-handle* *saved-output-mode*))
    (setf *stdin-handle* nil
          *stdout-handle* nil))
  (values))

;;; ---- Terminal size ----

(defun platform-set-pipe-size (rows cols)
  "Set pipe-mode terminal size (called from input parser on CSI 8;rows;cols t)."
  (setf *pipe-rows* rows
        *pipe-cols* cols)
  (values))

(defun platform-get-size ()
  "Return (VALUES rows cols).
Priority: pipe globals > COLUMNS/LINES env vars > GetConsoleScreenBufferInfo > 24x80."
  ;; 1. Pipe globals (set by resize escape sequence from mine-app)
  (when (and *pipe-rows* *pipe-cols*)
    (return-from platform-get-size (values *pipe-rows* *pipe-cols*)))
  ;; 2. COLUMNS/LINES env vars (initial size from mine-app)
  (let ((env-cols (parse-integer (or (sb-ext:posix-getenv "COLUMNS") "") :junk-allowed t))
        (env-lines (parse-integer (or (sb-ext:posix-getenv "LINES") "") :junk-allowed t)))
    (when (and env-cols env-lines (plusp env-cols) (plusp env-lines))
      (return-from platform-get-size (values env-lines env-cols))))
  ;; 3. GetConsoleScreenBufferInfo
  ;; CONSOLE_SCREEN_BUFFER_INFO is 22 bytes:
  ;;   offset  0: COORD  dwSize              (4 bytes: SHORT X, SHORT Y)
  ;;   offset  4: COORD  dwCursorPosition    (4 bytes)
  ;;   offset  8: WORD   wAttributes         (2 bytes)
  ;;   offset 10: SMALL_RECT srWindow        (8 bytes: SHORT Left,Top,Right,Bottom)
  ;;   offset 18: COORD  dwMaximumWindowSize (4 bytes)
  (let ((info (sb-alien:make-alien (sb-alien:array (sb-alien:unsigned 8) 22))))
    (unwind-protect
         (if (plusp (%get-console-screen-buffer-info
                     (or *stdout-handle*
                         (%get-std-handle +std-output-handle+))
                     (sb-alien:sap-alien (sb-alien:alien-sap info)
                                         (* t))))
             (let ((sap (sb-alien:alien-sap info)))
               ;; srWindow: Left=offset 10, Top=12, Right=14, Bottom=16
               (let ((left   (sb-sys:sap-ref-16 sap 10))
                     (top    (sb-sys:sap-ref-16 sap 12))
                     (right  (sb-sys:sap-ref-16 sap 14))
                     (bottom (sb-sys:sap-ref-16 sap 16)))
                 (values (max 1 (1+ (- bottom top)))
                         (max 1 (1+ (- right left))))))
             ;; 4. Fallback
             (values 24 80))
      (sb-alien:free-alien info))))

;;; ---- Input ----

(defun ensure-read-buf ()
  (unless *read-buf*
    (setf *read-buf*
          (sb-alien:make-alien
           (sb-alien:array (sb-alien:unsigned 8) #.+read-buf-size+))))
  (unless *bytes-read-buf*
    (setf *bytes-read-buf*
          (sb-alien:make-alien sb-alien:unsigned-int)))
  *read-buf*)

(defun %pipe-data-available-p ()
  "Return T if the stdin pipe has data available (non-blocking)."
  (let ((avail-buf (sb-alien:make-alien sb-alien:unsigned-int)))
    (unwind-protect
         (progn
           (setf (sb-alien:deref avail-buf) 0)
           (let ((ok (%peek-named-pipe
                      *stdin-handle*
                      (sb-alien:sap-alien (sb-sys:int-sap 0) (* t))
                      0
                      (sb-alien:sap-alien (sb-sys:int-sap 0)
                                          (* sb-alien:unsigned-int))
                      avail-buf
                      (sb-alien:sap-alien (sb-sys:int-sap 0)
                                          (* sb-alien:unsigned-int)))))
             (and (plusp ok) (plusp (sb-alien:deref avail-buf)))))
      (sb-alien:free-alien avail-buf))))

(defun %read-available-bytes ()
  "Read available bytes from stdin.  Assumes data is ready.
Returns (VALUES byte-vector count) or (VALUES NIL 0)."
  (setf (sb-alien:deref *bytes-read-buf*) 0)
  (let ((ok (%read-file
             *stdin-handle*
             (sb-alien:sap-alien
              (sb-alien:alien-sap *read-buf*) (* t))
             +read-buf-size+
             *bytes-read-buf*
             (sb-alien:sap-alien (sb-sys:int-sap 0) (* t)))))
    (cond
      ((plusp ok)
       (let ((n (sb-alien:deref *bytes-read-buf*)))
         (cond
           ((plusp n)
            (let ((result (make-array n :element-type '(unsigned-byte 8)))
                  (sap (sb-alien:alien-sap *read-buf*)))
              (dotimes (i n)
                (setf (aref result i) (sb-sys:sap-ref-8 sap i)))
              (values result n)))
           (t (values nil 0)))))
      (t (values nil 0)))))

(defun platform-read-bytes (timeout-ms)
  "Read available bytes from stdin with TIMEOUT-MS wait.
Returns (VALUES byte-vector count) on success, or (VALUES NIL 0)
on timeout or error."
  (ensure-read-buf)
  (cond
    ;; Pipe mode: poll with PeekNamedPipe (WaitForSingleObject does not
    ;; respect timeouts on synchronous pipe handles).
    (*stdin-is-pipe*
     (let ((deadline (+ (get-internal-real-time)
                        (* timeout-ms
                           (/ internal-time-units-per-second 1000)))))
       (loop
         (when (%pipe-data-available-p)
           (return (%read-available-bytes)))
         (when (>= (get-internal-real-time) deadline)
           (return (values nil 0)))
         (sleep 0.005))))
    ;; Console mode: use WaitForSingleObject
    (t
     (let ((wait-result (%wait-for-single-object
                         *stdin-handle*
                         (if (plusp timeout-ms) timeout-ms 0))))
       (cond
         ((= wait-result +wait-object-0+)
          (%read-available-bytes))
         (t
          (values nil 0)))))))

;;; ---- Output ----

(defun platform-write-string (s)
  "Write string S to the console (stdout)."
  (write-string s *standard-output*)
  (values))

(defun platform-flush ()
  "Flush console output."
  (force-output *standard-output*)
  (values))

;;; ---- Image save ----

(defun platform-reset-for-image-save ()
  "Reset state that cannot survive save-lisp-and-die."
  (setf *saved-input-mode* 0
        *saved-output-mode* 0
        *stdin-handle* nil
        *stdout-handle* nil
        *stdin-is-pipe* nil
        *read-buf* nil
        *bytes-read-buf* nil
        *pipe-rows* nil
        *pipe-cols* nil)
  (values))
