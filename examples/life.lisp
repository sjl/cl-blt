(ql:quickload '(:cl-blt :losh :iterate))

(defpackage :cl-blt.examples.life
  (:use :cl :losh :iterate))

(in-package :cl-blt.examples.life)

(defconstant +world-size+ 300)

(deftype world-index ()
  `(integer 0 (,+world-size+)))

(deftype world ()
  `(simple-array bit (,+world-size+ ,+world-size+)))

(defun make-world ()
  (make-array (list +world-size+ +world-size+)
              :element-type 'bit
              :initial-element 0))

(defun randomize-world (world)
  (iterate (for (cell row col) :in-array world)
           (setf (aref world row col)
                 (if (randomp) 1 0))))

(defun clear-world (world)
  (iterate (for (cell row col) :in-array world)
           (setf (aref world row col) 0)))

(defun-inline wref (world row col)
  (aref world
        (mod row +world-size+)
        (mod col +world-size+)))

(defun count-neighbors (world row col)
  (declare (optimize speed)
           (type world world)
           (type world-index row col))
  (flet ((cell (dr dc)
           (wref world (+ row dr) (+ col dc))))
    (+ (cell -1 -1)
       (cell -1  0)
       (cell -1  1)
       (cell  0 -1)
       (cell  0  1)
       (cell  1 -1)
       (cell  1  0)
       (cell  1  1))))

(defun tick-cell (cell neighbors)
  (if (= cell 1)
    (cond
      ((<= neighbors 1) 0)
      ((<= neighbors 3) 1)
      ((>= neighbors 4) 0))
    (if (= neighbors 3)
      1
      0)))

(defun tick-world (world next)
  (iterate
    (for (cell row col) :in-array world)
    (setf (aref next row col)
          (tick-cell cell (count-neighbors world row col)))))


(defun draw (world)
  (iterate
    (with srows = (blt:height))
    (with scols = (blt:width))
    (for (cell row col) :in-array world)
    (when (and (< row srows)
               (< col scols))
      (if (zerop cell)
        (setf (blt:color) (blt:rgbaf 0.1 0.1 0.1 1.0)
              (blt:cell-char col row) #\.)
        (setf (blt:color) (blt:rgbaf 1.0 1.0 0.0 1.0)
              (blt:cell-char col row) #\*))))
  (blt:refresh))

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.cellsize = 3x4")
  (blt:set "window.title = LIFE"))

(defun input ()
  (blt:key-case (blt:read)
    (:r :randomize)
    (:c :clear)
    (:space :tick)
    (:escape :quit)
    (:close :quit)))

(defun main ()
  (blt:with-terminal
    (config)
    (iterate
      (with world = (make-world))
      (with next = (make-world))
      (initially (setf
                   (aref world 3 4) 1
                   (aref world 4 5) 1
                   (aref world 5 3) 1
                   (aref world 5 4) 1
                   (aref world 5 5) 1))
      (draw world)
      (case (input)
        (:randomize (randomize-world world))
        (:clear (clear-world world))
        (:tick (tick-world world next) (rotatef world next))
        (:quit (return))))))

