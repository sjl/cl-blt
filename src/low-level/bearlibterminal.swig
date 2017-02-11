%module bearlibterminal

%insert("lisphead") %{
(cl:in-package :bearlibterminal/low-level)

(cffi:defctype color :uint)

(cffi:define-foreign-library bearlibterminal
  (:darwin "lib/libBearLibTerminal.dylib"))

(cffi:use-foreign-library bearlibterminal)
%}

%feature("intern_function","1");
%feature("export");

%typemap(cin) color_t "color";
%typemap(cout) color_t "color";

%include "src/low-level/include/BearLibTerminal.h"
