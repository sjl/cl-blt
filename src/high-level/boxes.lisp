(in-package :bearlibterminal/high-level)

(defun draw-box-outline (x y w h
                         top bottom left right
                         top-left top-right
                         bot-left bot-right)
  (loop :for bx :from (1+ x) :below (+ x w -1)
        :do (setf (blt:cell-char bx y) top
                  (blt:cell-char bx (+ y h -1)) bottom))
  (loop :for by :from (1+ y) :below (+ y h -1)
        :do (setf (blt:cell-char x by) left
                  (blt:cell-char (+ x w -1) by) right))
  (setf
    (blt:cell-char x y) top-left
    (blt:cell-char (+ x w -1) y) top-right
    (blt:cell-char x (+ y h -1)) bot-left
    (blt:cell-char (+ x w -1) (+ y h -1)) bot-right)
  (values))

(defun draw-box-fill (x y w h)
  (loop :for bx :from x :below (+ x w)
        :do (loop :for by :from y :below (+ y h)
                  :do (setf (blt:cell-char bx by) #\full_block))))


(defun draw-box-background (x y w h color)
  (setf (blt:color) color)
  (draw-box-fill (1+ x) (1+ y) (- w 2) (- h 2))
  (draw-box-outline x y w h
                    #\lower_half_block
                    #\upper_half_block
                    #\right_half_block
                    #\left_half_block
                    #\quadrant_lower_right
                    #\quadrant_lower_left
                    #\quadrant_upper_right
                    #\quadrant_upper_left))


(defun draw-box-border-light (x y w h color)
  (setf (blt:color) color)
  (draw-box-outline x y w h
                    #\box_drawings_light_horizontal
                    #\box_drawings_light_horizontal
                    #\box_drawings_light_vertical
                    #\box_drawings_light_vertical
                    #\box_drawings_light_down_and_right
                    #\box_drawings_light_down_and_left
                    #\box_drawings_light_up_and_right
                    #\box_drawings_light_up_and_left))

(defun draw-box-border-heavy (x y w h color)
  (setf (blt:color) color)
  (draw-box-outline x y w h
                    #\box_drawings_heavy_horizontal
                    #\box_drawings_heavy_horizontal
                    #\box_drawings_heavy_vertical
                    #\box_drawings_heavy_vertical
                    #\box_drawings_heavy_down_and_right
                    #\box_drawings_heavy_down_and_left
                    #\box_drawings_heavy_up_and_right
                    #\box_drawings_heavy_up_and_left))

(defun draw-box-border-double (x y w h color)
  (setf (blt:color) color)
  (draw-box-outline x y w h
                    #\box_drawings_double_horizontal
                    #\box_drawings_double_horizontal
                    #\box_drawings_double_vertical
                    #\box_drawings_double_vertical
                    #\box_drawings_double_down_and_right
                    #\box_drawings_double_down_and_left
                    #\box_drawings_double_up_and_right
                    #\box_drawings_double_up_and_left))


(defun draw-box-contents (x y w h contents)
  (blt:print (1+ x) (1+ y) contents
             :width (- w 2)
             :height (- h 2)))


(defun draw-box (layer x y width height contents &key
                 (border :light)
                 (background-color (blt:rgba 0 0 0))
                 (border-color (blt:rgba 255 255 255)))
  "Draw a box.

  The box will be draw on `layer`, and the necessary area will be cleared before
  doing so.

  The border of the box will be one cell wide/tall.  `border` specifies the type
  of border to draw, and can be one of `:light`, `:heavy`, or `:double`, or
  `nil` for a transparent border.

  `background-color` and `border-color` specify the colors to use.  If `nil` is
  given they will not be drawn.

  The `width` and `height` measurements include the two border cells.  For
  example: a `width` of `10` would have `8` cells of content space.

  `contents` will be `print`ed inside the box with the appropriate bounds.  The
  color, font, etc will all be whatever they are currently set to.

  **EXPERIMENTAL**: This function is experimental and may change or be remove
  entirely in the future.

  "
  (save-values (blt:composition blt:layer)
    (setf (blt:layer) layer
          (blt:composition) t)

    (clear-area x y width height)

    (save-value blt:color
      (when background-color
        (draw-box-background x y width height background-color))
      (when (and border border-color)
        (ecase border
          (:light (draw-box-border-light x y width height border-color))
          (:heavy (draw-box-border-heavy x y width height border-color))
          (:double (draw-box-border-double x y width height border-color)))))

    (draw-box-contents x y width height contents)))

