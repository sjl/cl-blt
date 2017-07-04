(defpackage :bearlibterminal/low-level
  (:use)
  (:export)
  (:import-from :cl
    :&rest)
  (:nicknames :blt/ll)
  (:documentation "This package contains the low-level, SWIG-generated wrapper functions for bearlibterminal."))

(defpackage :bearlibterminal/high-level
  (:use :cl :bearlibterminal.quickutils)
  (:export
    :bearlibterminal

    :background-color
    :cell-background-color
    :cell-char
    :cell-code
    :cell-color
    :character-input
    :clear
    :clear-area
    :clear-layer
    :close
    :color
    :color-name
    :color-to-hsva
    :color-to-rgba
    :composition
    :crop
    :font
    :has-input-p
    :height
    :hsva
    :key-case
    :layer
    :mouse
    :mouse-x
    :mouse-y
    :open
    :peek
    :print
    :read
    :refresh
    :rgba
    :set
    :sleep
    :width
    :with-terminal

    :draw-box

    )
  (:shadow

    :close
    :open
    :print
    :read
    :set
    :sleep

    )
  (:nicknames :blt/hl :blt)
  (:documentation "This package contains a high-level, lispy interface to bearlibterminal.  It has the nickname `blt` for easy prefixing."))
