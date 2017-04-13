(load "examples/map.lisp")
(sb-ext:gc :full t)
(sb-ext:save-lisp-and-die
  "map"
  :toplevel #'cl-blt.examples.map::main
  :executable t)

