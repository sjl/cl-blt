%module bearlibterminal

%insert("lisphead") %{
(cl:in-package :bearlibterminal/low-level)

(cffi:defctype color :uint)

(cffi:define-foreign-library blt:bearlibterminal
  (:linux (:or "libBearLibTerminal.so"
               "lib/libBearLibTerminal.so"))
  (:darwin (:or "Contents/Resources/libBearLibTerminal.dylib"
                "libBearLibTerminal.dylib"
                "lib/libBearLibTerminal.dylib")))

(cffi:use-foreign-library blt:bearlibterminal)
%}

%feature("intern_function","1");
%feature("export");

%typemap(cin)  color_t "color";
%typemap(cout) color_t "color";

%typemap(cin)  int8_t* ":string";
%typemap(cout) int8_t* ":string";

%include "src/low-level/include/BearLibTerminal.h"
