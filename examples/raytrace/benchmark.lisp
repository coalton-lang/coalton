(defpackage #:coalton-raytrace/benchmark
  (:use #:cl)
  (:export #:run-benchmark #:render-frame #:write-ppm #:image-checksum))

(in-package #:coalton-raytrace/benchmark)

(defun check-settings (width height samples depth seed)
  (dolist (value (list width height samples depth))
    (check-type value (integer 1 #.most-positive-fixnum)))
  (check-type seed (integer 1 #.(1- (expt 2 32))))
  (unless (< (* width height 3) array-dimension-limit most-positive-fixnum)
    (error "Image dimensions exceed the framebuffer limit.")))

(defun make-framebuffer (width height)
  (make-array (* width height 3) :element-type 'double-float :initial-element 0d0))

(defun make-camera (width height)
  (coalton:call-coalton-function coalton-raytrace:make-camera
                                 (float (/ width height) 1d0)))

(defun render-into (scene camera width height samples depth seed pixels)
  (coalton:call-coalton-function coalton-raytrace:render!
                                 scene camera width height samples depth seed pixels))

(defun render-frame (&key (width 320) (height 180) (samples 16) (depth 8) (seed 12345))
  "Render one image without measuring it; return a flat array of linear RGB."
  (check-settings width height samples depth seed)
  (let ((pixels (make-framebuffer width height)))
    (render-into (coalton:call-coalton-function coalton-raytrace:make-scene)
                 (make-camera width height) width height samples depth seed pixels)
    pixels))

(defun image-checksum (pixels)
  "FNV-1a over linear RGB quantized to unsigned 16-bit, low byte first.
This is a convenient comparison fingerprint, not a floating-point tolerance test."
  (let ((hash #x811c9dc5))
    (map nil (lambda (channel)
               (let ((word (round (* 65535 (max 0d0 (min 1d0 channel))))))
                 (dolist (byte (list (ldb (byte 8 0) word) (ldb (byte 8 8) word)))
                   (setf hash (ldb (byte 32 0) (* #x1000193 (logxor hash byte)))))))
         pixels)
    hash))

(defun write-ppm (path pixels width height)
  "Write an ASCII PPM with square-root display gamma, outside the measured work."
  (unless (= (length pixels) (* width height 3))
    (error "Framebuffer size does not match image dimensions."))
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out "P3~%~D ~D~%255~%" width height)
    (loop for offset from 0 below (length pixels) by 3 do
      (loop for channel from offset below (+ offset 3) do
        (format out "~D " (floor (* 256 (min 0.999d0 (sqrt (max 0d0 (aref pixels channel))))))))
      (terpri out)))
  path)

(defun allocation-counter ()
  #+sbcl (sb-ext:get-bytes-consed)
  #-sbcl nil)

(defun gc-clock ()
  #+sbcl sb-ext:*gc-run-time*
  #-sbcl nil)

(defun collect-garbage ()
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc))

(defun median (values)
  (let* ((sorted (sort (copy-list values) #'<))
         (n (length sorted))
         (middle (floor n 2)))
    (if (oddp n) (nth middle sorted)
        (/ (+ (nth (1- middle) sorted) (nth middle sorted)) 2))))

(defun run-benchmark (&key (width 320) (height 180) (samples 16) (depth 8)
                          (seed 12345) (trials 3) output)
  "Warm up, then measure repeated renders of the same scene and random stream.
Return a report plist and the framebuffer. OUTPUT, if supplied, names a PPM file.
Scene/camera/buffer construction, full GC before each trial, checksums, comparison,
reporting, and file output are excluded. GC during rendering remains enabled."
  (check-settings width height samples depth seed)
  (check-type trials (integer 1 #.most-positive-fixnum))
  (let* ((scene (coalton:call-coalton-function coalton-raytrace:make-scene))
         (camera (make-camera width height))
         (pixels (make-framebuffer width height))
         (measurements nil))
    (format t "~&~A ~A; Coalton ~A~%~Dx~D, ~D samples/pixel, depth ~D, seed ~D~%"
            (lisp-implementation-type) (lisp-implementation-version)
            (getf coalton-raytrace::*build-settings* :mode)
            width height samples depth seed)
    (format t "Renderer build settings: ~S~%" coalton-raytrace::*build-settings*)
    (render-into scene camera width height samples depth seed pixels)
    (let ((reference (copy-seq pixels)))
      (dotimes (trial trials)
        (collect-garbage)
        (let ((bytes-before (allocation-counter))
              (gc-before (gc-clock))
              (start (get-internal-real-time)))
          (render-into scene camera width height samples depth seed pixels)
          (let* ((end (get-internal-real-time))
                 (gc-after (gc-clock))
                 (bytes-after (allocation-counter))
                 (seconds (/ (- end start) (float internal-time-units-per-second 1d0)))
                 (bytes (and bytes-before (- bytes-after bytes-before)))
                 (gc-seconds (and gc-before (/ (- gc-after gc-before)
                                              (float internal-time-units-per-second 1d0)))))
            (unless (equalp pixels reference)
              (error "Trial ~D differs from the warm-up image." (1+ trial)))
            (push (list :seconds seconds :bytes bytes :gc-seconds gc-seconds) measurements)
            (format t "Trial ~D: ~,3F s" (1+ trial) seconds)
            (when bytes
              (format t "; ~:D bytes (~,1F bytes/primary sample); ~,3F s GC"
                      bytes (/ bytes (* width height samples)) gc-seconds))
            (terpri)))))
    (setf measurements (nreverse measurements))
    (let* ((seconds (median (mapcar (lambda (m) (getf m :seconds)) measurements)))
           (bytes (when (getf (first measurements) :bytes)
                    (median (mapcar (lambda (m) (getf m :bytes)) measurements))))
           (checksum (image-checksum pixels))
           (report (list :implementation (lisp-implementation-type)
                         :implementation-version (lisp-implementation-version)
                         :build-settings coalton-raytrace::*build-settings*
                         :width width :height height :samples samples :depth depth :seed seed
                         :checksum checksum :median-seconds seconds :median-bytes bytes
                         :trials measurements)))
      (format t "Median: ~,3F s; checksum ~8,'0X~%" seconds checksum)
      (unless bytes (format t "Allocation and GC timing counters are available on SBCL only.~%"))
      (when output (write-ppm output pixels width height))
      (values report pixels))))
