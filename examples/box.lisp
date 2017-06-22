(ql:quickload '(:cl-blt :losh :iterate :split-sequence))

(defpackage :cl-blt.examples.box
  (:use :cl :losh :iterate :bearlibterminal.quickutils))

(in-package :cl-blt.examples.box)

(defun draw-background ()
  (setf (blt:layer) 0)
  (iterate (for-nested ((x :from 0 :below (truncate (blt:width) 2))
                        (y :from 0 :below (truncate (blt:height) 2))))
           (for color = (random-range 0.1 0.3))
           (setf (blt:color) (blt:rgba color color color)
                 (blt:cell-char (* 2 x) (* 2 y))
                 (random-elt "abcdefghijklmnopqrstuvwxyz"))))



(defun word-wrap-line (line width)
  (with-output-to-string (*standard-output*)
    (let ((pos 0)
          (spaces 0)
          (words (split-sequence:split-sequence #\space line)))
      (flet ((add (s)
               (incf pos (length s))
               (princ s))
             (linebreak ()
               (setf pos 0 spaces 0)
               (terpri)))
        (iterate
          (until (null words))
          (for word = (pop words))
          (for len = (length word))
          (cond
            ;; chomp leading whitespace
            ((and (zerop pos) (zerop len))
             nil)
            ;; if we have multiple spaces in a row, preserve them (maybe)
            ((zerop len)
             (incf spaces))
            ;; if we're dealing with a single word that's too long, reluctantly
            ;; split it into pieces
            ((and (zerop pos) (> len width))
             (add (subseq word 0 width))
             (linebreak)
             (push (subseq word width) words))
            ;; if this would send us beyond the limit, break
            ((> (+ spaces len pos) width)
             (linebreak)
             (push word words))
            ;; otherwise concat
            (t
             (add (make-string spaces :initial-element #\space))
             (add word)
             (setf spaces 1))))))))

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
        (blt::clear-layer)
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
        (finally-protected (blt::clear-layer)
                           (blt:refresh))))))

(defun get-user-input (x y layer prompt maximum-length)
  (blt:draw-box layer x y (+ 3 (max (length prompt)
                                    maximum-length))
                6
                prompt
                :border-color (blt:rgba 1.0 1.0 1.0)
                :background-color (blt:rgba 0.4 0.0 0.0))
  (setf (blt:layer) (+ layer 2))
  (prog1 (read-string (+ x 1)
                      (+ y 3)
                      maximum-length
                      :font "normal")
    (blt::clear-layer layer)
    (blt::clear-layer (1+ layer))))

(defun get-name ()
  (blt::clear-layer 15)
  (pr (get-user-input 0 10 10 "[font=normal]What is your name?[/font]" 15)))


(defun draw ()
  (setf (blt:color) (blt:rgba 1.0 1.0 0.0))

  (blt:draw-box 5 3 3 20 10
                (format nil "[font=normal][color=red]hello~%world! how [font=italic]close[font=normal] can [font=bold]we[font=normal] get here, what if we go over oh no![/color]"))

  (blt:draw-box 7 30 3 42 30
                (word-wrap (format nil "123456789x123456789x123456789x more?~% ~%~
                                    This is an test.  It has multiple words.  ~
                                    And some spaces too.  It should be wrapped correctly.~%~
                                    foo foo foo foo  foos     and a bar")
                           20)
                :border (random-elt '(:light :heavy :double)))
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
