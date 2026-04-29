;;;; platform-unix.lisp -- Unix (macOS, Linux) terminal I/O via termios + ioctl.

(in-package #:mine/bindings/platform)

;;; Shared constant
(defconstant +read-buf-size+ 4096)

;;; ---- State ----

(defvar *saved-termios* nil
  "Original termios state, saved before enabling raw mode.")

(defvar *read-buf* nil
  "Persistent C-heap read buffer (alien array). Allocated lazily
so it survives save-lisp-and-die.")

;;; ---- Constants ----

(defconstant +tiocgwinsz+
  #+(or darwin openbsd) #x40087468
  #+linux   #x5413
  #-(or darwin linux openbsd) (error "TIOCGWINSZ: unsupported Unix variant"))

;;; ---- Raw mode ----

(defun platform-enable-raw-mode ()
  "Put the terminal into raw mode.
Disables echo, canonical processing, signals, flow control, and
output post-processing.  Sets VMIN=0, VTIME=0 for non-blocking reads."
  ;; Save current state for later restoration
  (setf *saved-termios* (sb-posix:tcgetattr 0))
  ;; Build a new termios with raw settings
  (let ((raw (sb-posix:tcgetattr 0)))
    ;; Input flags: clear BRKINT ICRNL INPCK ISTRIP IXON
    (setf (sb-posix:termios-iflag raw)
          (logandc2 (sb-posix:termios-iflag raw)
                    (logior sb-posix:brkint sb-posix:icrnl
                            sb-posix:inpck sb-posix:istrip
                            sb-posix:ixon)))
    ;; Output flags: clear OPOST (no output processing)
    (setf (sb-posix:termios-oflag raw)
          (logandc2 (sb-posix:termios-oflag raw) sb-posix:opost))
    ;; Control flags: ensure CS8 (8-bit characters)
    (setf (sb-posix:termios-cflag raw)
          (logior (sb-posix:termios-cflag raw) sb-posix:cs8))
    ;; Local flags: clear ECHO ICANON IEXTEN ISIG
    (setf (sb-posix:termios-lflag raw)
          (logandc2 (sb-posix:termios-lflag raw)
                    (logior sb-posix:echo sb-posix:icanon
                            sb-posix:iexten sb-posix:isig)))
    ;; Control characters: VMIN=0 VTIME=0 → read returns immediately
    (let ((cc (sb-posix:termios-cc raw)))
      (setf (aref cc sb-posix:vmin) 0)
      (setf (aref cc sb-posix:vtime) 0)
      (setf (sb-posix:termios-cc raw) cc))
    ;; Apply with TCSAFLUSH (drain output, discard pending input)
    (sb-posix:tcsetattr 0 sb-posix:tcsaflush raw))
  (values))

(defun platform-disable-raw-mode ()
  "Restore the terminal to its original state."
  (when *saved-termios*
    (ignore-errors
      (sb-posix:tcsetattr 0 sb-posix:tcsaflush *saved-termios*))
    (setf *saved-termios* nil))
  (values))

;;; ---- Terminal size ----

(defun platform-set-pipe-size (rows cols)
  "No-op on Unix (pipe size is only used on Windows under mine-app)."
  (declare (ignore rows cols))
  (values))

(defun platform-get-size ()
  "Return (VALUES rows cols) via ioctl TIOCGWINSZ.
Falls back to 24x80 if the ioctl fails."
  ;; struct winsize: uint16 ws_row, ws_col, ws_xpixel, ws_ypixel (8 bytes)
  ;; Allocate as a single uint64 for simplicity.
  (let ((buf (sb-alien:make-alien (sb-alien:unsigned 64))))
    (setf (sb-alien:deref buf) 0)
    (unwind-protect
         (let ((ret (sb-alien:alien-funcall
                     (sb-alien:extern-alien "ioctl"
                       (sb-alien:function sb-alien:int
                         sb-alien:int
                         sb-alien:unsigned-long
                         (* t)))
                     0 +tiocgwinsz+
                     (sb-alien:sap-alien (sb-alien:alien-sap buf)
                                         (* t)))))
           (if (>= ret 0)
               (let ((sap (sb-alien:alien-sap buf)))
                 (values (max 1 (sb-sys:sap-ref-16 sap 0))    ; ws_row
                         (max 1 (sb-sys:sap-ref-16 sap 2))))  ; ws_col
               (values 24 80)))
      (sb-alien:free-alien buf))))

;;; ---- Input ----

(defun ensure-read-buf ()
  "Return the persistent C-heap read buffer, allocating if needed."
  (or *read-buf*
      (setf *read-buf*
            (sb-alien:make-alien
             (sb-alien:array (sb-alien:unsigned 8) #.+read-buf-size+)))))

(defun platform-read-bytes (timeout-ms)
  "Read available bytes from stdin with TIMEOUT-MS millisecond wait.
Returns (VALUES byte-vector count) on success, or (VALUES NIL 0) on
timeout or error."
  (let ((timeout-secs (if (plusp timeout-ms)
                          (/ timeout-ms 1000.0d0)
                          0.0d0)))
    (cond
      ((sb-sys:wait-until-fd-usable 0 :input timeout-secs)
       ;; Data available — read as many bytes as we can
       (let* ((sap (sb-alien:alien-sap (ensure-read-buf)))
              (n (sb-unix:unix-read 0 sap +read-buf-size+)))
         (cond
           ((and n (plusp n))
            (let ((result (make-array n :element-type '(unsigned-byte 8))))
              (dotimes (i n)
                (setf (aref result i) (sb-sys:sap-ref-8 sap i)))
              (values result n)))
           (t
            (values nil 0)))))
      ;; Timeout
      (t
       (values nil 0)))))

;;; ---- Output ----

(defun platform-write-string (s)
  "Write string S to the terminal (stdout)."
  (write-string s *standard-output*)
  (values))

(defun platform-flush ()
  "Flush terminal output."
  (force-output *standard-output*)
  (values))

;;; ---- Image save ----

(defun platform-reset-for-image-save ()
  "Reset state that cannot survive save-lisp-and-die."
  (setf *saved-termios* nil
        *read-buf* nil)
  (values))
