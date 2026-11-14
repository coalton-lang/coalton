;;;; run.lisp
;;;;
;;;; Runs each simulation sequentially and in parallel from the same
;;;; initial state, checks that both runs end in exactly the same state,
;;;; and reports the times.

(defpackage #:coalton-parallel-numerics
  (:use #:cl)
  (:local-nicknames (#:fdtd #:coalton-parallel-numerics/fdtd)
                    (#:nbody #:coalton-parallel-numerics/nbody)
                    (#:ising #:coalton-parallel-numerics/ising)
                    (#:optics #:coalton-parallel-numerics/optics)
                    (#:raytrace #:coalton-parallel-numerics/raytrace)
                    (#:rt #:coalton/threads/runtime))
  (:export #:run-examples
           #:run-fdtd
           #:run-nbody
           #:run-ising
           #:run-optics
           #:run-raytrace
           #:check-examples))

(in-package #:coalton-parallel-numerics)

(defun call (function &rest arguments)
  (apply #'coalton:call-coalton-function function arguments))

(defun seconds ()
  (/ (get-internal-real-time) (float internal-time-units-per-second 1d0)))

(defmacro timed (&body body)
  "Evaluate BODY, and return the number of seconds that it took."
  (let ((start (gensym "START")))
    `(let ((,start (seconds)))
       ,@body
       (- (seconds) ,start))))

(defun details (control &rest arguments)
  "Format a description of a simulation's final state."
  (let ((*read-default-float-format* 'double-float))
    (apply #'format nil control arguments)))

(defun report (title sequential parallel identical details)
  (format t "~&~A~%  sequential ~7,3Fs   parallel ~7,3Fs   speedup ~5,1Fx   ~:[RESULTS DIFFER~;identical results~]~%  ~A~%"
          title sequential parallel (/ sequential parallel) identical details)
  (finish-output)
  (list title sequential parallel identical))

(defun write-pgm (path width height pixel)
  "Write a WIDTH by HEIGHT grayscale image to PATH, the shade of row I and
column J being (FUNCALL PIXEL I J), from 0 to 255."
  (with-open-file (out path :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
    (write-sequence (map '(vector (unsigned-byte 8)) #'char-code
                         (format nil "P5~%~D ~D~%255~%" width height))
                    out)
    (let ((row (make-array width :element-type '(unsigned-byte 8))))
      (dotimes (i height)
        (dotimes (j width)
          (setf (aref row j) (funcall pixel i j)))
        (write-sequence row out))))
  path)

;;; Simulations

(defun run-fdtd (&key (size 1024) (steps 1000) image)
  "Simulate an electromagnetic pulse on a SIZE by SIZE grid for STEPS time
steps. With the default parameters, the pulse is focused by the
cylinder at the end. If IMAGE is a pathname, write the final electric
field there as a PGM image."
  (let* ((initial (call fdtd:make-grid size size))
         (sequential (call fdtd:copy-grid initial))
         (parallel (call fdtd:copy-grid initial))
         (sequential-time (timed (call fdtd:simulate! sequential 0 steps nil)))
         (parallel-time (timed (call fdtd:simulate! parallel 0 steps t)))
         (identical (every (lambda (field)
                             (equalp (call field sequential) (call field parallel)))
                           (list fdtd:grid-ez fdtd:grid-hx fdtd:grid-hy))))
    (let* ((ez (call fdtd:grid-ez parallel))
           (peak (reduce #'max ez :key #'abs)))
      (when image
        (write-pgm image size size
                   (lambda (i j)
                     (let ((v (/ (aref ez (+ (* i size) j)) (max (* 0.5 peak) 1e-30))))
                       (round (* 127.5 (+ 1 (max -1 (min 1 v)))))))))
      (report (format nil "FDTD electromagnetics, ~Dx~D grid, ~D steps" size size steps)
              sequential-time parallel-time identical
              (details "field energy ~,4F, peak |Ez| ~,3F" (call fdtd:energy parallel) peak)))))

(defun run-nbody (&key (bodies 8192) (steps 10) (dt 1d-3) (seed 42))
  "Simulate BODIES bodies for STEPS time steps of length DT."
  (let* ((initial (call nbody:make-bodies bodies seed))
         (sequential (call nbody:copy-bodies initial))
         (parallel (call nbody:copy-bodies initial))
         (sequential-time (timed (call nbody:simulate! sequential dt steps nil)))
         (parallel-time (timed (call nbody:simulate! parallel dt steps t)))
         (identical (and (every #'equalp
                                (call nbody:bodies-positions sequential)
                                (call nbody:bodies-positions parallel))
                         (every #'equalp
                                (call nbody:bodies-velocities sequential)
                                (call nbody:bodies-velocities parallel))))
         (initial-energy (call nbody:energy initial))
         (final-energy (call nbody:energy parallel)))
    (report (format nil "N-body gravitation, ~D bodies, ~D steps" bodies steps)
            sequential-time parallel-time identical
            (details "total energy ~,6F, relative change ~,1E"
                     final-energy
                     (abs (/ (- final-energy initial-energy) initial-energy))))))

(defun run-ising (&key (size 2048) (sweeps 200) (temperature 2d0) (seed 1) image)
  "Simulate a SIZE by SIZE Ising model at TEMPERATURE for SWEEPS sweeps,
starting from random spins. SIZE must be even. If IMAGE is a pathname,
write the final spins there as a PGM image."
  (let* ((initial (call ising:make-lattice size seed))
         (sequential (call ising:copy-lattice initial))
         (parallel (call ising:copy-lattice initial))
         (temperature (coerce temperature 'double-float))
         (sequential-time (timed (call ising:simulate! sequential temperature 0 sweeps seed nil)))
         (parallel-time (timed (call ising:simulate! parallel temperature 0 sweeps seed t)))
         (identical (equalp (call ising:lattice-spins sequential)
                            (call ising:lattice-spins parallel))))
    (when image
      (let ((spins (call ising:lattice-spins parallel)))
        (write-pgm image size size
                   (lambda (i j)
                     (if (plusp (aref spins (+ (* i size) j))) 255 0)))))
    (report (format nil "Ising model, ~Dx~D lattice, ~D sweeps at T = ~,3F" size size sweeps temperature)
            sequential-time parallel-time identical
            (details "magnetization ~,4F, energy per site ~,4F"
                     (call ising:magnetization parallel)
                     (call ising:energy-per-site parallel)))))

(defun write-optics-ppm (path &key (width 720) (height 480) (rays 13) (steps 1000) (spread 0.24d0))
  "Write a WIDTH by HEIGHT color PPM image of one period of the elliptical
graded-index ray bundle to PATH, coloring off-axis rays by the sign and
magnitude of their launch angle and highlighting the ray emitted
parallel to the positive x-axis in gold."
  (let* ((rgb (make-array (* width height 3) :element-type '(unsigned-byte 8) :initial-element 0))
         (xmin -2.6d0) (xmax 2.6d0)
         (ymin -0.6d0) (ymax 2.6d0)
         (dtau (/ optics:ellipse-period (float steps 1d0)))
         (beam (call optics:make-beam rays (coerce spread 'double-float)))
         (angles (call optics:beam-angles beam)))
    (labels ((clamp-byte (x)
               (max 0 (min 255 (round x))))
             (set-pixel (px py r g b)
               (when (and (<= 0 px (1- width)) (<= 0 py (1- height)))
                 (let ((idx (* 3 (+ (* py width) px))))
                   (setf (aref rgb idx) r
                         (aref rgb (+ idx 1)) g
                         (aref rgb (+ idx 2)) b))))
             (blend-disk (cx cy radius r g b alpha)
               (let ((r2 (* radius radius)))
                 (loop :for dy :from (- (ceiling radius)) :to (ceiling radius)
                       :do (loop :for dx :from (- (ceiling radius)) :to (ceiling radius)
                                 :for d2 = (+ (* dx dx) (* dy dy))
                                 :when (<= d2 r2)
                                   :do (let* ((px (+ cx dx))
                                              (py (+ cy dy))
                                              (w (* alpha (- 1d0 (/ d2 (+ r2 1d0))))))
                                         (when (and (<= 0 px (1- width)) (<= 0 py (1- height)))
                                           (let ((idx (* 3 (+ (* py width) px))))
                                             (setf (aref rgb idx)
                                                   (clamp-byte (+ (* (- 1d0 w) (aref rgb idx)) (* w r)))
                                                   (aref rgb (+ idx 1))
                                                   (clamp-byte (+ (* (- 1d0 w) (aref rgb (+ idx 1))) (* w g)))
                                                   (aref rgb (+ idx 2))
                                                   (clamp-byte (+ (* (- 1d0 w) (aref rgb (+ idx 2))) (* w b)))))))))))
             (to-px (x)
               (round (* (- width 1) (/ (- x xmin) (- xmax xmin)))))
             (to-py (y)
               (round (* (- height 1) (/ (- ymax y) (- ymax ymin))))))
      ;; Background shaded by refractive index n(x, y).
      (dotimes (py height)
        (let ((y (- ymax (* (/ (float py 1d0) (max 1 (- height 1))) (- ymax ymin)))))
          (dotimes (px width)
            (let* ((x (+ xmin (* (/ (float px 1d0) (max 1 (- width 1))) (- xmax xmin))))
                   (n (call optics:ellipse-index x y))
                   (t-norm (max 0d0 (min 1d0 (/ (- n 0.5d0) 1.8d0)))))
              (set-pixel px py
                         (clamp-byte (+ 10 (* 22 t-norm)))
                         (clamp-byte (+ 14 (* 34 t-norm)))
                         (clamp-byte (+ 26 (* 58 t-norm))))))))
      ;; Trace the off-axis rays first, then the on-axis ray on top.
      (dotimes (step (1+ steps))
        (let ((xs (call optics:beam-x beam))
              (ys (call optics:beam-y beam)))
          (loop :for i :from (1- rays) :downto 1
                :for theta = (aref angles i)
                :for px = (to-px (aref xs i))
                :for py = (to-py (aref ys i))
                :do (if (plusp theta)
                        (blend-disk px py 1.2d0 255 95 145 0.35d0)
                        (blend-disk px py 1.2d0 70 195 255 0.35d0)))
          (blend-disk (to-px (aref xs 0)) (to-py (aref ys 0)) 2.2d0 255 225 65 0.9d0))
        (when (< step steps)
          (call optics:simulate! beam dtau 1 t))))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (map '(vector (unsigned-byte 8)) #'char-code
                           (format nil "P6~%~D ~D~%255~%" width height))
                      out)
      (write-sequence rgb out))
    path))

(defun run-optics (&key (rays 16384) (steps 2000) (spread 0.2d0) image)
  "Simulate a bundle of RAYS light rays emitted at the origin in an
elliptical graded-index medium for one full period in STEPS steps. If
IMAGE is a pathname, write a picture of the ray trajectories there as a
PPM image."
  (let* ((spread (coerce spread 'double-float))
         (dtau (/ optics:ellipse-period (float steps 1d0)))
         (initial (call optics:make-beam rays spread))
         (sequential (call optics:copy-beam initial))
         (parallel (call optics:copy-beam initial))
         (sequential-time (timed (call optics:simulate! sequential dtau steps nil)))
         (parallel-time (timed (call optics:simulate! parallel dtau steps t)))
         (identical (every (lambda (coord)
                             (equalp (call coord sequential) (call coord parallel)))
                           (list optics:beam-x optics:beam-y
                                 optics:beam-px optics:beam-py)))
         (ellipse-error (call optics:max-ellipse-error parallel optics:ellipse-period))
         (eikonal-error (call optics:max-eikonal-error parallel)))
    (when image
      (write-optics-ppm image :spread spread))
    (report (format nil "GRIN optics, ~D rays, ~D steps (1 period)" rays steps)
            sequential-time parallel-time identical
            (details "max orbit error ~,2E, max |p - n| ~,2E"
                     ellipse-error eikonal-error))))

(defun run-raytrace (&key (width 640) (height 360) (samples 16) (depth 8) (seed 12345) image)
  "Render the scene of examples/raytrace at WIDTH by HEIGHT pixels, tracing
SAMPLES paths of at most DEPTH bounces per pixel from the random seed
SEED. If IMAGE is a pathname, write the picture there as a PPM image."
  (let* ((sequential (make-array (* width height 3) :element-type 'double-float
                                                    :initial-element 0d0))
         (parallel (make-array (* width height 3) :element-type 'double-float
                                                  :initial-element 0d0))
         (sequential-time
           (timed (call raytrace:render! width height samples depth seed nil sequential)))
         (parallel-time
           (timed (call raytrace:render! width height samples depth seed t parallel)))
         (identical (equalp sequential parallel)))
    (when image
      (coalton-raytrace/benchmark:write-ppm image parallel width height))
    (report (format nil "Path tracing, ~Dx~D pixels, ~D samples per pixel" width height samples)
            sequential-time parallel-time identical
            (details "image checksum ~8,'0X" (coalton-raytrace/benchmark:image-checksum parallel)))))

(defun check-examples ()
  "Run small versions of the simulations, and signal an error unless their
sequential and parallel runs end in the same state."
  (let ((*standard-output* (make-broadcast-stream)))
    (dolist (result (list (run-fdtd :size 128 :steps 100)
                          (run-nbody :bodies 512 :steps 3)
                          (run-ising :size 128 :sweeps 10)
                          (run-optics :rays 256 :steps 200)
                          (run-raytrace :width 32 :height 18 :samples 2 :depth 4)))
      (unless (fourth result)
        (error "The sequential and parallel runs of ~A ended in different states."
               (first result))))
    (let* ((steps 200)
           (dtau (/ optics:ellipse-period (float steps 1d0)))
           (beam (call optics:make-beam 64 0.2d0)))
      (call optics:simulate! beam dtau steps t)
      (assert (< (call optics:max-ellipse-error beam optics:ellipse-period) 1d-5))
      (assert (< (call optics:max-eikonal-error beam) 1d-7))))
  t)

(defun run-examples (&key image-directory)
  "Run every simulation with its default parameters, and summarize. If
IMAGE-DIRECTORY is a pathname, write images of the final states there."
  (flet ((image (name)
           (and image-directory
                (merge-pathnames name (uiop:ensure-directory-pathname image-directory)))))
    ;; Start the workers, so that their start-up is not timed.
    (rt:join2 (lambda () nil) (lambda () nil))
    (format t "~&Workers: ~D~2%" (rt:worker-count))
    (let ((results (list (run-fdtd :image (image "fdtd.pgm"))
                         (run-nbody)
                         (run-ising :image (image "ising.pgm"))
                         (run-optics :image (image "optics.ppm"))
                         (run-raytrace :image (image "raytrace.ppm")))))
      (format t "~&~%~65A ~10@A ~10@A ~8@A~%" "Simulation" "Sequential" "Parallel" "Speedup")
      (loop :for (title sequential parallel identical) :in results
            :do (format t "~65A ~9,3Fs ~9,3Fs ~7,1Fx~:[  (RESULTS DIFFER)~;~]~%"
                        title sequential parallel (/ sequential parallel) identical))
      (values))))
