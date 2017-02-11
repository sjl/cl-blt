# API Reference

The following is a list of all user-facing parts of `cl-blt`.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `BEARLIBTERMINAL/HIGH-LEVEL`

This package contains a high-level, lispy interface to bearlibterminal.  It has the nickname `blt` for easy prefixing.

## Package `BEARLIBTERMINAL/LOW-LEVEL`

This package contains the low-level, SWIG-generated wrapper functions for bearlibterminal.

### `+--SIZEOF-WCHAR-T--+` (variable)

### `+TERMINAL-VSPRINTF-MAXIMUM-BUFFER-SIZE+` (variable)

### `+TERMINAL-WCHAR-SUFFIX+` (variable)

### `+TK-0+` (variable)

### `+TK-1+` (variable)

### `+TK-2+` (variable)

### `+TK-3+` (variable)

### `+TK-4+` (variable)

### `+TK-5+` (variable)

### `+TK-6+` (variable)

### `+TK-7+` (variable)

### `+TK-8+` (variable)

### `+TK-9+` (variable)

### `+TK-A+` (variable)

### `+TK-ALIGN-BOTTOM+` (variable)

### `+TK-ALIGN-CENTER+` (variable)

### `+TK-ALIGN-DEFAULT+` (variable)

### `+TK-ALIGN-LEFT+` (variable)

### `+TK-ALIGN-MIDDLE+` (variable)

### `+TK-ALIGN-RIGHT+` (variable)

### `+TK-ALIGN-TOP+` (variable)

### `+TK-ALT+` (variable)

### `+TK-APOSTROPHE+` (variable)

### `+TK-B+` (variable)

### `+TK-BACKSLASH+` (variable)

### `+TK-BACKSPACE+` (variable)

### `+TK-BKCOLOR+` (variable)

### `+TK-C+` (variable)

### `+TK-CELL-HEIGHT+` (variable)

### `+TK-CELL-WIDTH+` (variable)

### `+TK-CHAR+` (variable)

### `+TK-CLOSE+` (variable)

### `+TK-COLOR+` (variable)

### `+TK-COMMA+` (variable)

### `+TK-COMPOSITION+` (variable)

### `+TK-CONTROL+` (variable)

### `+TK-D+` (variable)

### `+TK-DELETE+` (variable)

### `+TK-DOWN+` (variable)

### `+TK-E+` (variable)

### `+TK-END+` (variable)

### `+TK-ENTER+` (variable)

### `+TK-EQUALS+` (variable)

### `+TK-ESCAPE+` (variable)

### `+TK-EVENT+` (variable)

### `+TK-F+` (variable)

### `+TK-F-1+` (variable)

### `+TK-F-10+` (variable)

### `+TK-F-11+` (variable)

### `+TK-F-12+` (variable)

### `+TK-F-2+` (variable)

### `+TK-F-3+` (variable)

### `+TK-F-4+` (variable)

### `+TK-F-5+` (variable)

### `+TK-F-6+` (variable)

### `+TK-F-7+` (variable)

### `+TK-F-8+` (variable)

### `+TK-F-9+` (variable)

### `+TK-FULLSCREEN+` (variable)

### `+TK-G+` (variable)

### `+TK-GRAVE+` (variable)

### `+TK-H+` (variable)

### `+TK-HEIGHT+` (variable)

### `+TK-HOME+` (variable)

### `+TK-I+` (variable)

### `+TK-INPUT-CANCELLED+` (variable)

### `+TK-INPUT-NONE+` (variable)

### `+TK-INSERT+` (variable)

### `+TK-J+` (variable)

### `+TK-K+` (variable)

### `+TK-KEY-RELEASED+` (variable)

### `+TK-KP-0+` (variable)

### `+TK-KP-1+` (variable)

### `+TK-KP-2+` (variable)

### `+TK-KP-3+` (variable)

### `+TK-KP-4+` (variable)

### `+TK-KP-5+` (variable)

### `+TK-KP-6+` (variable)

### `+TK-KP-7+` (variable)

### `+TK-KP-8+` (variable)

### `+TK-KP-9+` (variable)

### `+TK-KP-DIVIDE+` (variable)

### `+TK-KP-ENTER+` (variable)

### `+TK-KP-MINUS+` (variable)

### `+TK-KP-MULTIPLY+` (variable)

### `+TK-KP-PERIOD+` (variable)

### `+TK-KP-PLUS+` (variable)

### `+TK-L+` (variable)

### `+TK-LAYER+` (variable)

### `+TK-LBRACKET+` (variable)

### `+TK-LEFT+` (variable)

### `+TK-M+` (variable)

### `+TK-MINUS+` (variable)

### `+TK-MOUSE-CLICKS+` (variable)

### `+TK-MOUSE-LEFT+` (variable)

### `+TK-MOUSE-MIDDLE+` (variable)

### `+TK-MOUSE-MOVE+` (variable)

### `+TK-MOUSE-PIXEL-X+` (variable)

### `+TK-MOUSE-PIXEL-Y+` (variable)

### `+TK-MOUSE-RIGHT+` (variable)

### `+TK-MOUSE-SCROLL+` (variable)

### `+TK-MOUSE-WHEEL+` (variable)

### `+TK-MOUSE-X+` (variable)

### `+TK-MOUSE-X-1+` (variable)

### `+TK-MOUSE-X-2+` (variable)

### `+TK-MOUSE-Y+` (variable)

### `+TK-N+` (variable)

### `+TK-O+` (variable)

### `+TK-OFF+` (variable)

### `+TK-ON+` (variable)

### `+TK-P+` (variable)

### `+TK-PAGEDOWN+` (variable)

### `+TK-PAGEUP+` (variable)

### `+TK-PAUSE+` (variable)

### `+TK-PERIOD+` (variable)

### `+TK-Q+` (variable)

### `+TK-R+` (variable)

### `+TK-RBRACKET+` (variable)

### `+TK-RESIZED+` (variable)

### `+TK-RETURN+` (variable)

### `+TK-RIGHT+` (variable)

### `+TK-S+` (variable)

### `+TK-SEMICOLON+` (variable)

### `+TK-SHIFT+` (variable)

### `+TK-SLASH+` (variable)

### `+TK-SPACE+` (variable)

### `+TK-T+` (variable)

### `+TK-TAB+` (variable)

### `+TK-U+` (variable)

### `+TK-UP+` (variable)

### `+TK-V+` (variable)

### `+TK-W+` (variable)

### `+TK-WCHAR+` (variable)

### `+TK-WIDTH+` (variable)

### `+TK-X+` (variable)

### `+TK-Y+` (variable)

### `+TK-Z+` (variable)

### `COLOR-FROM-ARGB` (function)

    (COLOR-FROM-ARGB)

### `COLOR-FROM-NAME` (function)

    (COLOR-FROM-NAME)

### `COLOR-FROM-NAME-16` (function)

    (COLOR-FROM-NAME-16)

### `COLOR-FROM-NAME-32` (function)

    (COLOR-FROM-NAME-32)

### `COLOR-FROM-NAME-8` (function)

    (COLOR-FROM-NAME-8)

### `COLOR-FROM-WNAME` (function)

    (COLOR-FROM-WNAME)

### `DIMENSIONS-T`

`#<STANDARD-CLASS DOCPARSER:CFFI-STRUCT>`

### `TERMINAL-BKCOLOR` (function)

    (TERMINAL-BKCOLOR)

### `TERMINAL-CHECK` (function)

    (TERMINAL-CHECK)

### `TERMINAL-CLEAR` (function)

    (TERMINAL-CLEAR)

### `TERMINAL-CLEAR-AREA` (function)

    (TERMINAL-CLEAR-AREA)

### `TERMINAL-CLOSE` (function)

    (TERMINAL-CLOSE)

### `TERMINAL-COLOR` (function)

    (TERMINAL-COLOR)

### `TERMINAL-COMPOSITION` (function)

    (TERMINAL-COMPOSITION)

### `TERMINAL-CROP` (function)

    (TERMINAL-CROP)

### `TERMINAL-DELAY` (function)

    (TERMINAL-DELAY)

### `TERMINAL-GET` (function)

    (TERMINAL-GET)

### `TERMINAL-GET-16` (function)

    (TERMINAL-GET-16)

### `TERMINAL-GET-32` (function)

    (TERMINAL-GET-32)

### `TERMINAL-GET-8` (function)

    (TERMINAL-GET-8)

### `TERMINAL-HAS-INPUT` (function)

    (TERMINAL-HAS-INPUT)

### `TERMINAL-LAYER` (function)

    (TERMINAL-LAYER)

### `TERMINAL-MEASURE` (function)

    (TERMINAL-MEASURE)

### `TERMINAL-MEASURE-EXT` (function)

    (TERMINAL-MEASURE-EXT)

### `TERMINAL-MEASURE-EXT-16` (function)

    (TERMINAL-MEASURE-EXT-16)

### `TERMINAL-MEASURE-EXT-32` (function)

    (TERMINAL-MEASURE-EXT-32)

### `TERMINAL-MEASURE-EXT-8` (function)

    (TERMINAL-MEASURE-EXT-8)

### `TERMINAL-MEASUREF` (function)

    (TERMINAL-MEASUREF)

### `TERMINAL-MEASUREF-EXT` (function)

    (TERMINAL-MEASUREF-EXT)

### `TERMINAL-OPEN` (function)

    (TERMINAL-OPEN)

### `TERMINAL-PEEK` (function)

    (TERMINAL-PEEK)

### `TERMINAL-PICK` (function)

    (TERMINAL-PICK)

### `TERMINAL-PICK-BKCOLOR` (function)

    (TERMINAL-PICK-BKCOLOR)

### `TERMINAL-PICK-COLOR` (function)

    (TERMINAL-PICK-COLOR)

### `TERMINAL-PRINT` (function)

    (TERMINAL-PRINT)

### `TERMINAL-PRINT-EXT` (function)

    (TERMINAL-PRINT-EXT)

### `TERMINAL-PRINT-EXT-16` (function)

    (TERMINAL-PRINT-EXT-16)

### `TERMINAL-PRINT-EXT-32` (function)

    (TERMINAL-PRINT-EXT-32)

### `TERMINAL-PRINT-EXT-8` (function)

    (TERMINAL-PRINT-EXT-8)

### `TERMINAL-PRINTF` (function)

    (TERMINAL-PRINTF)

### `TERMINAL-PRINTF-EXT` (function)

    (TERMINAL-PRINTF-EXT)

### `TERMINAL-PUT` (function)

    (TERMINAL-PUT)

### `TERMINAL-PUT-EXT` (function)

    (TERMINAL-PUT-EXT)

### `TERMINAL-READ` (function)

    (TERMINAL-READ)

### `TERMINAL-READ-STR` (function)

    (TERMINAL-READ-STR)

### `TERMINAL-READ-STR-16` (function)

    (TERMINAL-READ-STR-16)

### `TERMINAL-READ-STR-32` (function)

    (TERMINAL-READ-STR-32)

### `TERMINAL-READ-STR-8` (function)

    (TERMINAL-READ-STR-8)

### `TERMINAL-READ-WSTR` (function)

    (TERMINAL-READ-WSTR)

### `TERMINAL-REFRESH` (function)

    (TERMINAL-REFRESH)

### `TERMINAL-SET` (function)

    (TERMINAL-SET)

### `TERMINAL-SET-16` (function)

    (TERMINAL-SET-16)

### `TERMINAL-SET-32` (function)

    (TERMINAL-SET-32)

### `TERMINAL-SET-8` (function)

    (TERMINAL-SET-8)

### `TERMINAL-SETF` (function)

    (TERMINAL-SETF)

### `TERMINAL-STATE` (function)

    (TERMINAL-STATE)

### `TERMINAL-VSPRINTF` (function)

    (TERMINAL-VSPRINTF)

### `TERMINAL-VSWPRINTF` (function)

    (TERMINAL-VSWPRINTF)

### `TERMINAL-WGET` (function)

    (TERMINAL-WGET)

### `TERMINAL-WMEASURE` (function)

    (TERMINAL-WMEASURE)

### `TERMINAL-WMEASURE-EXT` (function)

    (TERMINAL-WMEASURE-EXT)

### `TERMINAL-WMEASUREF` (function)

    (TERMINAL-WMEASUREF)

### `TERMINAL-WMEASUREF-EXT` (function)

    (TERMINAL-WMEASUREF-EXT)

### `TERMINAL-WPRINT` (function)

    (TERMINAL-WPRINT)

### `TERMINAL-WPRINT-EXT` (function)

    (TERMINAL-WPRINT-EXT)

### `TERMINAL-WPRINTF` (function)

    (TERMINAL-WPRINTF)

### `TERMINAL-WPRINTF-EXT` (function)

    (TERMINAL-WPRINTF-EXT)

### `TERMINAL-WSET` (function)

    (TERMINAL-WSET)

### `TERMINAL-WSETF` (function)

    (TERMINAL-WSETF)

