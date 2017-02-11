(defpackage :bearlibterminal/low-level
  (:use)
  (:export)
  (:import-from :cl
    :&rest)
  (:nicknames :blt/ll)
  (:documentation "This package contains the low-level, SWIG-generated wrapper functions for bearlibterminal."))

(defpackage :bearlibterminal/high-level
  (:use :cl
        :bearlibterminal.quickutils)
  (:export)
  (:nicknames :blt/hl :blt)
  (:documentation "This package contains a high-level, lispy interface to bearlibterminal.  It has the nickname `blt` for easy prefixing."))
