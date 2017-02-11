(in-package :cl-user)

(ql:quickload '(:cl-blt :losh :iterate))

(use-package :losh)
(use-package :iterate)

;;;; Heightmap ----------------------------------------------------------------
(defconstant +world-exponent+ 9)
(defconstant +world-size+ (expt 2 +world-exponent+))


(defun allocate-heightmap ()
  (make-array (list +world-size+ +world-size+)
    :element-type 'single-float
    :initial-element 0.0
    :adjustable nil))


(defun average4 (a b c d)
  (/ (+ a b c d) 4))

(defun hm-ref (heightmap x y)
  (flet ((ref (n)
           (cond
             ((< -1 n +world-size+) n)
             ((= n +world-size+) 0)
             (t (mod n +world-size+)))))
    (aref heightmap (ref x) (ref y))))


(defun heightmap-extrema (heightmap)
  (iterate
    (for v :across-flat-array heightmap :with-index i)
    (maximize v :into max)
    (minimize v :into min)
    (finally (return (values min max)))))

(defun normalize-heightmap (heightmap)
  (multiple-value-bind (min max) (heightmap-extrema heightmap)
    (iterate
      (with span = (- max min))
      (for v :across-flat-array heightmap :with-index i)
      (setf (row-major-aref heightmap i)
            (/ (- v min) span)))))


(defun ds-init (heightmap)
  (setf (aref heightmap 0 0) 0.5))


(defun ds-square (heightmap x y radius spread)
  (setf (aref heightmap x y)
        (random-around (average4 (hm-ref heightmap (- x radius) (- y radius))
                                 (hm-ref heightmap (- x radius) (+ y radius))
                                 (hm-ref heightmap (+ x radius) (- y radius))
                                 (hm-ref heightmap (+ x radius) (+ y radius)))
                       spread)))

(defun ds-diamond (heightmap x y radius spread)
  (setf (aref heightmap x y)
        (random-around (average4 (hm-ref heightmap (- x radius) y)
                                 (hm-ref heightmap (+ x radius) y)
                                 (hm-ref heightmap x (- y radius))
                                 (hm-ref heightmap x (+ y radius)))
                       spread)))

(defun ds-squares (heightmap radius spread)
  (iterate
    (for-nested ((x :from radius :below +world-size+ :by (* 2 radius))
                 (y :from radius :below +world-size+ :by (* 2 radius))))
    (ds-square heightmap x y radius spread)))

(defun ds-diamonds (heightmap radius spread)
  (iterate
    (for i :from 0)
    (for y :from 0 :below +world-size+ :by radius)
    (iterate
      (with shift = (if (evenp i) radius 0))
      (for x :from shift :below +world-size+ :by (* 2 radius))
      (ds-diamond heightmap x y radius spread))))


(defun diamond-square (heightmap)
  (ds-init heightmap)
  (let ((spread 0.8)
        (spread-reduction 0.7))
    (recursively ((radius (floor +world-size+ 2))
                  (spread spread))
      (when (>= radius 1)
        (ds-squares heightmap radius spread)
        (ds-diamonds heightmap radius spread)
        (recur (/ radius 2)
               (* spread spread-reduction)))))
  (normalize-heightmap heightmap)
  heightmap)


;;;; GUI ----------------------------------------------------------------------
(defun terrain-char (height)
  (cond ((< height 0.2) #\#)
        ((< height 0.4) #\#)
        ((< height 0.7) #\#)
        ((< height 0.9) #\#)
        (t              #\#)))

(defparameter *heightmap* (allocate-heightmap))

(defun draw ()
  (iterate
    (for-nested ((x :from 0 :below (min +world-size+ (blt:width)))
                 (y :from 0 :below (min +world-size+ (blt:height)))))
    (for height = (aref *heightmap* x y))
    (setf
      (blt:color) (blt:rgbaf height height height 1.0)
      (blt:cell-char x y) (terrain-char height)))
  ; (blt:print 1 1 "Demo!")
  (blt:refresh))

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.cellsize = 10x10")
  (blt:set "window.title = Terrain Gen Demo"))

(defun main ()
  (blt:with-terminal
    (iterate
      (config)
      (draw)
      (blt:key-case (blt:read)
        (:space (diamond-square *heightmap*))
        (:escape (return))
        (:close (return))))))
