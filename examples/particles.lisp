(ql:quickload '(:cl-blt :losh :iterate))

(defpackage :cl-blt.examples.particles
  (:use :cl :losh :iterate))

(in-package :cl-blt.examples.particles)

;;;; Data ---------------------------------------------------------------------
(defparameter *running* nil)
(defparameter *mspf* 0)
(defparameter *mouse* nil)
(defparameter *particles* nil)


;;;; Utils --------------------------------------------------------------------
(defun noop (particle ms)
  (declare (ignore particle ms)))

(defun random-glyph ()
  (random-elt "*!#$%^&?.,-:;'/><(){}[]"))

(defun random-color ()
  (blt:hsva (random 1.0)
            (random 1.0)
            (random-range 0.5 1.0)))


(defstruct mouse
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defstruct particle
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (glyph (random-glyph) :type character)
  (color (random-color) :type (unsigned-byte 32))
  (lifetime 1000 :type fixnum)
  (transformer #'noop :type function))


(defun update-particles (ms)
  (setf *particles*
        (delete-if (lambda (particle)
                     (minusp (decf (particle-lifetime particle) ms)))
                   *particles*))
  (mapc (lambda (particle)
          (funcall (particle-transformer particle) particle ms))
        *particles*)
  'ok)

(defun transform-drop (ms-per-cell particle ms)
  (incf (particle-y particle)
        (/ ms ms-per-cell)))


(defun clear-layer (layer)
  (setf (blt:layer) layer)
  (blt:clear-area 0 0 (blt:width) (blt:height)))


(defun update-mouse-location ()
  (multiple-value-bind (x y)
      (blt:mouse)
    (setf (mouse-x *mouse*) (* 2 (truncate x 2))
          (mouse-y *mouse*) (* 2 (truncate y 2)))))


(defun make-drop-particle (x y)
  (make-particle
    :x x
    :y y
    :lifetime (random-range 600 1000)
    :transformer (losh::curry #'transform-drop (random-range 5 50))))


(defun add-particle ()
  (let ((x (coerce (mouse-x *mouse*) 'single-float))
        (y (coerce (mouse-y *mouse*) 'single-float)))
    (iterate (repeat (random-range 1000 2000))
             (push (make-drop-particle (+ x (random-range-inclusive -19.0 19.0))
                                       (+ y (random-range-inclusive -19.0 19.0)))
                   *particles*))))


;;;; Drawing ------------------------------------------------------------------
(defun draw-mspf ()
  (clear-layer 5)
  (blt:print 0 0 (format nil "MSPF: ~D" *mspf*)))

(defun draw-cursor ()
  (clear-layer 2)
  (setf (blt:cell-char (mouse-x *mouse*)
                       (mouse-y *mouse*))
        #\@))

(defun draw-particles ()
  (clear-layer 1)
  (iterate
    (for particle :in *particles*)
    (setf
      (blt:color)
      (particle-color particle)

      (blt:cell-char (truncate (particle-x particle))
                     (truncate (particle-y particle)))
      (particle-glyph particle))))

(defun draw ()
  (setf (blt:color) (blt:rgba 1.0 1.0 1.0 1.0))
  (draw-mspf)
  (draw-cursor)
  (draw-particles)
  (blt:refresh))


;;;; Config -------------------------------------------------------------------
(defun config ()
  (blt:set "font: ./examples/ProggySquare/ProggySquare.ttf, size=20x20, spacing=2x2, align=dead-center;")
  (blt:set "input.filter = keyboard, mouse")
  (blt:set "output.vsync = false")
  (blt:set "window.resizeable = true")
  (blt:set "window.cellsize = 10x10")
  (blt:set "window.size = 80x50")
  (blt:set "window.title = Particle Demo"))


;;;; Input --------------------------------------------------------------------
(defun event ()
  (if (blt:has-input-p)
    (blt:key-case (blt:read)
      ;; (:space (draw-background))
      (:mouse-move :mouse-move)
      (:mouse-left :add-particle)
      (:escape :quit)
      (:close :quit))
    :done))

(defun handle-event (event)
  (ecase event
    (:mouse-move (update-mouse-location))
    (:add-particle (add-particle))
    (:quit (setf *running* nil))))

(defun handle-events ()
  (iterate
    (for event = (event))
    (until (eql event :done))
    (when event
      (handle-event event))))


;;;; Main ---------------------------------------------------------------------
(defun main ()
  (setf *running* t
        *mouse* (make-mouse)
        *particles* '())
  (blt:with-terminal
    (config)
    (iterate
      (while *running*)
      (for time = (get-internal-real-time))
      (for prev-time :previous time :initially 0)
      (for frame-time = (* 1000 (/ (- time prev-time)
                                   internal-time-units-per-second)))
      (setf *mspf* frame-time)
      (update-particles frame-time)
      (draw)
      (handle-events))))

