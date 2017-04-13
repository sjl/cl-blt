;; (ql:quickload '(:cl-blt :losh :iterate :split-sequence))
(asdf:load-system :cl-blt)
(asdf:load-system :losh)
(asdf:load-system :iterate)
(asdf:load-system :split-sequence)

(defpackage :cl-blt.examples.map
  (:use :cl :losh :iterate :bearlibterminal.quickutils))

(in-package :cl-blt.examples.map)

(defun clear-layer (&optional layer)
  (when layer
    (setf (blt:layer) layer))
  (blt:clear-area 0 0 (blt:width) (blt:height)))

(defmacro in-layer (layer &body body)
  (let ((previous-layer (gensym)))
    `(let ((,previous-layer (blt:layer)))
       (setf (blt:layer) ,layer)
       (unwind-protect (progn ,@body)
         (setf (blt:layer) ,previous-layer)))))

(defmacro in-clear-layer (layer &body body)
  `(in-layer ,layer
     (clear-layer)
     ,@body))


(defstruct tile
  (glyph #\Space :type character)
  (color 0 :type blt::color))

(defun draw-map (map)
  (in-clear-layer 0
    (iterate
      (with (map-width map-height) = (array-dimensions map))
      (for-nested ((x :from 0 :below (min map-width (truncate (blt:width) 2)))
                   (y :from 0 :below (min map-height (truncate (blt:height) 2)))))
      (for tile = (aref map x y))
      (setf (blt:color) (tile-color tile)
            (blt:cell-char (* 2 x) (* 2 y)) (tile-glyph tile)))))

(defun make-random-tile ()
  (make-tile :glyph (random-elt ",.")
             :color (blt:hsva 0.4
                              (random-range 0.4 0.9)
                              (random-range 0.4 0.9))))

(defun make-random-map (width height)
  (let ((map (make-array (list width height)
               :element-type 'tile
               :initial-element (make-random-tile))))
    (iterate
      (for (_ x y) :in-array map)
      (setf (aref map x y) (make-random-tile)))
    map))


(defun read-map-from-file (filename)
  (iterate
    (with lines = (split-sequence:split-sequence
                    #\newline
                    (read-file-into-string filename)))
    (with height = (length lines))
    (with width = (reduce #'max (mapcar #'length lines)))
    (with result = (make-array (list width height)
                     :element-type 'tile
                     :initial-element (make-tile)))
    (for y :from 0)
    (for line :in lines)
    (iterate (for x :from 0)
             (for char :in-vector line)
             (setf (aref result x y)
                   (make-tile :glyph char
                              :color (blt:hsva 1.0 0.0 1.0))))
    (finally (return result))))

(defparameter *map* nil)
(defparameter *size* 10)

(defun draw ()
  (blt:clear)
  (draw-map *map*)
  (blt:refresh))

(defun resize (new-cell-size)
  (let ((c new-cell-size)
        (cc (* 2 new-cell-size)))
    (blt:set (format nil "normal font: ./examples/UbuntuMono/UbuntuMono-R.ttf, size=~Dx~D, spacing=1x2, align=center;" c cc))
    (blt:set (format nil "italic font: ./examples/UbuntuMono/UbuntuMono-RI.ttf, size=~Dx~D, spacing=1x2, align=center;" c cc))
    (blt:set (format nil "bold font: ./examples/UbuntuMono/UbuntuMono-B.ttf, size=~Dx~D, spacing=1x2, align=center;" c cc))
    (blt:set (format nil "font: ./examples/ProggySquare/ProggySquare.ttf, size=~Dx~D, spacing=2x2, align=dead-center;" cc cc))
    (blt:set (format nil "window.cellsize = ~Dx~D" c c))))

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = 80x50")
  (blt:set "window.title = Map Demo"))

(defun main ()
  (blt:with-terminal
    (setf *size* 10)
    (setf *map* (make-random-map 10 10))
    (resize *size*)
    (config)
    (iterate
      (draw)
      (blt:key-case (blt:read)
        (:space (setf *map* (make-random-map (random-range 1 100)
                                             (random-range 1 100))))
        (:r (setf *map* (read-map-from-file "examples/sample-map.txt")))
        (:numpad-plus (incf *size* 1) (resize *size*))
        (:numpad-minus (setf *size* (max 1 (1- *size*))) (resize *size*))
        (:escape (return))
        (:close (return))))))
