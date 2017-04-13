(ql:quickload '(:cl-blt :losh :iterate :split-sequence))

(defpackage :cl-blt.examples.box
  (:use :cl :losh :iterate))

(in-package :cl-blt.examples.box)

;;;; GUI ----------------------------------------------------------------------
(defun clear-layer (&optional layer)
  (when layer
    (setf (blt:layer) layer))
  (blt:clear-area 0 0 (blt:width) (blt:height)))


(defun draw-background ()
  (setf (blt:layer) 0)
  (iterate (for-nested ((x :from 0 :below (truncate (blt:width) 2))
                        (y :from 0 :below (truncate (blt:height) 2))))
           (for color = (random-range 0.1 0.3))
           (setf (blt:color) (blt:rgba color color color)
                 (blt:cell-char (* 2 x) (* 2 y))
                 (random-elt "abcdefghijklmnopqrstuvwxyz"))))


(defun draw-outline (x y w h
                     top bottom left right
                     top-left top-right
                     bot-left bot-right)
  (iterate (for bx :from (1+ x) :below (+ x w -1))
           (setf (blt:cell-char bx y) top
                 (blt:cell-char bx (+ y h -1)) bottom))
  (iterate (for by :from (1+ y) :below (+ y h -1))
           (setf (blt:cell-char x by) left
                 (blt:cell-char (+ x w -1) by) right))
  (setf
    (blt:cell-char x y) top-left
    (blt:cell-char (+ x w -1) y) top-right
    (blt:cell-char x (+ y h -1)) bot-left
    (blt:cell-char (+ x w -1) (+ y h -1)) bot-right))

(defun draw-fill (x y w h &optional (char #\full_block))
  (iterate (for-nested ((bx :from x :below (+ x w))
                        (by :from y :below (+ y h))))
           (setf (blt:cell-char bx by) char)))


(defun draw-box-background (x y w h &optional (color (blt:rgba 0 0 0)))
  (setf (blt:color) color)
  (draw-fill (1+ x) (1+ y) (- w 2) (- h 2))
  (draw-outline x y w h
                #\lower_half_block
                #\upper_half_block
                #\right_half_block
                #\left_half_block
                #\quadrant_lower_right
                #\quadrant_lower_left
                #\quadrant_upper_right
                #\quadrant_upper_left))

(defun draw-box-border (x y w h &optional (color (blt:rgba 255 255 255)))
  (setf (blt:color) color)
  (draw-outline x y w h
                #\box_drawings_double_horizontal
                #\box_drawings_double_horizontal
                #\box_drawings_double_vertical
                #\box_drawings_double_vertical
                #\box_drawings_double_down_and_right
                #\box_drawings_double_down_and_left
                #\box_drawings_double_up_and_right
                #\box_drawings_double_up_and_left))

(defun draw-box-contents (x y w h contents
                          &optional (color (blt:rgba 1.0 1.0 1.0)))
  (setf (blt:color) color)
  (blt:print (1+ x) (1+ y)
             (format nil "[font=normal]~A[/font]" contents)
             :width (- w 2) :height (- h 2)))

(defun draw-box (x y w h contents layer)
  (clear-layer layer)
  (clear-layer (1+ layer))

  (setf (blt:layer) layer
        (blt:composition) t)
  (draw-box-background x y w h)
  (draw-box-border x y w h)

  (setf (blt:layer) (1+ layer)
        (blt:composition) nil)
  (blt:clear-area x y w h)
  (draw-box-contents x y w h contents))


(defun make-word-wrap-format-string (width)
  ;; http://cybertiggyr.com/fmt/fmt.pdf
  ;; unfortunately we can't use ~V in here so we'll just use concat instead
  (concatenate 'string
               "~{~<~%~1,"
               ;; Format checks for strictly less than width, but it's more
               ;; natural to give the width as an inclusive range...
               (princ-to-string (1+ width))
               ":;~A~>~^ ~}"))

(defun word-wrap-line (line width)
  (format nil (make-word-wrap-format-string width)
          (split-sequence:split-sequence #\space line)))

(defun word-wrap (string width)
  (format nil "~{~A~^~%~}"
          (iterate
            (for line in (split-sequence:split-sequence #\newline string))
            (collect (word-wrap-line line width)))))


(defun read-string (x y maximum-length &key (font ""))
  (let ((result (make-array maximum-length
                  :element-type 'character
                  :fill-pointer 0)))
    ;; Have to do the `print` fuckery so non-1x1 fonts work right.
    (labels ((draw-string ()
               (blt:print x y (format nil "[font=~A]~V,,,'_A[/font]"
                                      font maximum-length result))))
      (iterate
        (clear-layer)
        (draw-string)
        (blt:refresh)
        (blt:key-case (blt:read)
          (:escape (return))
          (:close (return))
          (:enter (return result))
          (:backspace (when (plusp (length result))
                        (vector-pop result)))
          (t (let ((char (blt:character-input)))
               (when (and char (< (length result) maximum-length))
                 (vector-push char result)))))
        (blt:refresh)
        (finally-protected (clear-layer)
                           (blt:refresh))))))

(defun get-user-input (x y layer prompt maximum-length)
  (draw-box x y (+ 3 (max (length prompt)
                          maximum-length))
            6
            prompt
            layer)
  (setf (blt:layer) (+ layer 2))
  (prog1 (read-string (+ x 1)
                      (+ y 3)
                      maximum-length
                      :font "normal")
    (clear-layer layer)
    (clear-layer (1+ layer))))

(defun get-name ()
  (clear-layer 15)
  (pr (get-user-input 0 10 10 "What is your name?" 15)))


(defun draw ()
  (draw-box 3 3 20 10 (format nil "[color=red]hello~%world! how [font=italic]close[font=normal] can [font=bold]we[font=normal] get here, what if we go over oh no![/color]") 5)

  (draw-box 30 3 40 30 (word-wrap "This is an test.  It has multiple words.  And some spaces too.  It should be wrapped correctly." 10) 7)

  (blt:refresh))

(defun config ()
  (blt:set "normal font: ./examples/UbuntuMono/UbuntuMono-R.ttf, size=10x20, spacing=1x2, align=center;")
  (blt:set "italic font: ./examples/UbuntuMono/UbuntuMono-RI.ttf, size=10x20, spacing=1x2, align=center;")
  (blt:set "bold font: ./examples/UbuntuMono/UbuntuMono-B.ttf, size=10x20, spacing=1x2, align=center;")
  (blt:set "font: ./examples/ProggySquare/ProggySquare.ttf, size=20x20, spacing=2x2, align=dead-center;")
  (blt:set "window.resizeable = false")
  (blt:set "window.cellsize = 10x10")
  (blt:set "window.size = 80x50")
  (blt:set "window.title = Box Demo"))

(defun main ()
  (blt:with-terminal
    (config)
    (draw-background)
    (iterate
      (draw)
      (blt:key-case (blt:read)
        (:space (draw-background))
        (:enter (get-name))
        (:escape (return))
        (:close (return))))))
