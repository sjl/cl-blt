(in-package :bearlibterminal/high-level)

; (sb-int:set-floating-point-modes :traps nil)

;;;; Utils --------------------------------------------------------------------
(defun pr (val)
  (format t "~S~%" val)
  (finish-output)
  (values))

(defmacro -<> (expr &rest forms)
  "Thread the given forms, with `<>` as a placeholder."
  ;; I am going to lose my fucking mind if I have to program lisp without
  ;; a threading macro, but I don't want to add another dep to this library, so
  ;; here we are.
  `(let* ((<> ,expr)
          ,@(mapcar (lambda (form)
                      (if (symbolp form)
                        `(<> (,form <>))
                        `(<> ,form)))
                    forms))
     <>))


(defmacro defun-inline (name &body body)
  "Like `defun`, but declaims `name` to be `inline`."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,@body)
     ',name))


;;;; Colors -------------------------------------------------------------------
(deftype color ()
  '(unsigned-byte 32))

(deftype color-byte ()
  '(unsigned-byte 8))

(deftype color-float ()
  '(single-float 0.0 1.0))


(declaim
  (ftype (function (color-byte color-byte color-byte color-byte)
                   (unsigned-byte 32)) rgba-byte%)
  (ftype (function (color-float color-float color-float color-float)
                   (unsigned-byte 32)) rgba-float%)
  (ftype (function (color-float color-float color-float)
                   (values color-float color-float color-float &optional))
         hsv-to-rgb rgb-to-hsv)
  (ftype (function
           (color)
           (values color-byte color-byte color-byte color-byte &optional))
         color-to-rgba-bytes
         color-to-hsva-bytes)
  (ftype (function
           (color)
           (values color-float color-float color-float color-float &optional))
         color-to-rgba-floats
         color-to-hsva-floats))


(defun-inline hsv-to-rgb (h s v)
  ;; https://en.wikipedia.org/wiki/HSL_and_HSV#From_HSV
  ;; look i don't know either mate i just transcribed the fuckin thing
  (let* ((h (* h 360)) ; convert 0-1 to 0-360
         (h% (/ h 60))
         (c (* v s))
         (x (* c (- 1 (abs (1- (mod h% 2))))))
         (m (- v c)))
    (multiple-value-bind (r g b)
        (cond
          ((<= h% 1) (values c x 0.0))
          ((<= h% 2) (values x c 0.0))
          ((<= h% 3) (values 0.0 c x))
          ((<= h% 4) (values 0.0 x c))
          ((<= h% 5) (values x 0.0 c))
          ((<= h% 6) (values c 0.0 x))
          (t (values 0.0 0.0 0.0)))
      (values (+ r m)
              (+ g m)
              (+ b m)))))

(defun-inline rgb-to-hsv (r g b)
  ;; http://www.rapidtables.com/convert/color/rgb-to-hsv.htm
  (let* ((c-min (min r g b))
         (c-max (max r g b))
         (delta (- c-max c-min)))
    (values
      (* (/ 60 360)
         (cond
           ((zerop delta) 0.0)
           ((= c-max r) (mod (/ (- g b) delta) 6.0))
           ((= c-max g) (+ (/ (- b r) delta) 2.0))
           ((= c-max b) (+ (/ (- r g) delta) 4.0))
           (t 0.0)))
      (if (zerop c-max)
        0.0
        (/ delta c-max))
      c-max)))


(defun-inline color-float-to-byte (n)
  (truncate (* n 255.0)))

(defun-inline color-byte-to-float (n)
  (/ n 255.0))


(defun-inline color-to-rgba-bytes (color)
  (values (ldb (byte 8 16) color)
          (ldb (byte 8 8) color)
          (ldb (byte 8 0) color)
          (ldb (byte 8 24) color)))

(defun-inline color-to-rgba-floats (color)
  (multiple-value-bind (r g b a) (color-to-rgba-bytes color)
    (values (color-byte-to-float r)
            (color-byte-to-float g)
            (color-byte-to-float b)
            (color-byte-to-float a))))

(defun-inline color-to-hsva-floats (color)
  (multiple-value-bind (r g b a) (color-to-rgba-floats color)
    (multiple-value-bind (h s v) (rgb-to-hsv r g b)
      (values h s v a))))

(defun-inline color-to-hsva-bytes (color)
  (multiple-value-bind (h s v a) (color-to-hsva-floats color)
    (values (color-float-to-byte h)
            (color-float-to-byte s)
            (color-float-to-byte v)
            (color-float-to-byte a))))


(defun color-to-rgba (color &optional float?)
  (if float?
    (color-to-rgba-floats color)
    (color-to-rgba-bytes color)))

(defun color-to-hsva (color &optional float?)
  (if float?
    (color-to-hsva-floats color)
    (color-to-hsva-bytes color)))


(defun-inline rgba-byte% (r g b a)
  (declare (optimize speed)
           (type color-byte r g b a))
  (-<> 0
    (dpb a (byte 8 24) <>)
    (dpb r (byte 8 16) <>)
    (dpb g (byte 8 8) <>)
    (dpb b (byte 8 0) <>)))

(defun-inline rgba-float% (r g b a)
  (declare (optimize speed)
           (type color-float r g b a))
  (rgba-byte% (color-float-to-byte r)
              (color-float-to-byte g)
              (color-float-to-byte b)
              (color-float-to-byte a)))


(defun-inline hsva-float% (h s v a)
  (declare (optimize speed)
           (type color-float h s v a))
  (multiple-value-bind (r g b) (hsv-to-rgb h s v)
    (rgba-float% r g b a)))

(defun-inline hsva-byte% (h s v a)
  (declare (optimize speed)
           (type color-byte h s v a))
  (hsva-float% (/ h 255.0)
               (/ s 255.0)
               (/ v 255.0)
               (/ a 255.0)))


(defun rgba% (r g b a)
  (assert (or (and (typep r 'color-byte)
                   (typep g 'color-byte)
                   (typep b 'color-byte)
                   (typep a '(or null color-byte)))
              (and (typep r 'color-float)
                   (typep g 'color-float)
                   (typep b 'color-float)
                   (typep a '(or null color-float))))
      (r g b a))
  (etypecase r
    (color-byte (rgba-byte% r g b (or a 255)))
    (color-float (rgba-float% r g b (or a 1.0)))))

(defun rgba (r g b &optional (a nil))
  (rgba% r g b a))


(defun hsva% (h s v a)
  (assert (or (and (typep h 'color-byte)
                   (typep s 'color-byte)
                   (typep v 'color-byte)
                   (typep a '(or null color-byte)))
              (and (typep h 'color-float)
                   (typep s 'color-float)
                   (typep v 'color-float)
                   (typep a '(or null color-float))))
      (h s v a))
  (etypecase h
    (color-byte (hsva-byte% h s v (or a 255)))
    (color-float (hsva-float% h s v (or a 1.0)))))

(defun hsva (h s v &optional (a nil))
  (hsva% h s v a))


(define-compiler-macro rgba (&whole form r g b &optional (a nil))
  (if (and (constantp r)
           (constantp g)
           (constantp b)
           (constantp a))
    (rgba% r g b a)
    form))

(define-compiler-macro hsva (&whole form h s v &optional (a nil))
  (if (and (constantp h)
           (constantp s)
           (constantp v)
           (constantp a))
    (hsva% h s v a)
    form))


(defun color-name (color-name)
  (blt/ll:color-from-name color-name))


;;;; Type Conversion ----------------------------------------------------------
(defun-inline boolean-to-onoff (boolean)
  (if boolean
    blt/ll:+tk-on+
    blt/ll:+tk-off+))

(defun-inline onoff-to-boolean (onoff)
  (ecase onoff
    (blt/ll:+tk-on+ t)
    (blt/ll:+tk-off+ nil)))

(defun-inline int-to-boolean (int)
  (not (zerop int)))


(defun-inline state-boolean (state)
  (int-to-boolean (blt/ll:terminal-state state)))


(defun-inline character-to-code-point (character)
  ;; These seem to work in SBCL, ABCL, CCL, and ECL, but I need to do more
  ;; digging before I'm convinced.
  (char-code character))

(defun-inline code-point-to-character (code-point)
  ;; These seem to work in SBCL, ABCL, CCL, and ECL, but I need to do more
  ;; digging before I'm convinced.
  (code-char code-point))


(defun horizontal-alignment (alignment-keyword)
  (ccase alignment-keyword
    (:default          blt/ll:+tk-align-default+)
    (:left             blt/ll:+tk-align-left+)
    (:right            blt/ll:+tk-align-right+)
    ((:middle :center) blt/ll:+tk-align-center+)))

(defun vertical-alignment (alignment-keyword)
  (ccase alignment-keyword
    (:default          blt/ll:+tk-align-default+)
    (:top              blt/ll:+tk-align-top+)
    (:bottom           blt/ll:+tk-align-bottom+)
    ((:middle :center) blt/ll:+tk-align-middle+)))


;;;; Error Checking -----------------------------------------------------------
(define-condition bearlibterminal-error (error) ())

(defun check (return-value)
  (if (zerop return-value)
    (error 'bearlibterminal-error)))


;;;; Wrappers -----------------------------------------------------------------

; Initialization and configuration: set
; Output: put ext, measure
; Input: read str

(defun open ()
  (check (blt/ll:terminal-open)))

(defun close ()
  (blt/ll:terminal-close))

(defun set (configuration-string)
  (check (blt/ll:terminal-set-8 configuration-string)))


(defun refresh ()
  (blt/ll:terminal-refresh))

(defun clear ()
  (blt/ll:terminal-clear))

(defun clear-area (x y width height)
  (blt/ll:terminal-clear-area x y width height))

(defun crop (x y width height)
  (blt/ll:terminal-crop x y width height))


(defun layer ()
  (blt/ll:terminal-state blt/ll:+tk-layer+))

(defun (setf layer) (new-value)
  (blt/ll:terminal-layer new-value)
  new-value)


(defun color ()
  (blt/ll:terminal-state blt/ll:+tk-color+))

(defun (setf color) (color)
  (blt/ll:terminal-color color))


(defun background-color ()
  (blt/ll:terminal-state blt/ll:+tk-bkcolor+))

(defun (setf background-color) (color)
  (blt/ll:terminal-bkcolor color))


(defun composition ()
  (onoff-to-boolean (blt/ll:terminal-state blt/ll:+tk-composition+)))

(defun (setf composition) (new-value)
  (blt/ll:terminal-composition (boolean-to-onoff new-value))
  new-value)


(defun mouse-x ()
  (blt/ll:terminal-state blt/ll:+tk-mouse-x+))

(defun mouse-y ()
  (blt/ll:terminal-state blt/ll:+tk-mouse-y+))

(defun mouse ()
  (values (blt/ll:terminal-state blt/ll:+tk-mouse-x+)
          (blt/ll:terminal-state blt/ll:+tk-mouse-y+)))

(defun has-input-p ()
  (int-to-boolean (blt/ll:terminal-has-input)))

(defun read ()
  (blt/ll:terminal-read))

(defun peek ()
  (blt/ll:terminal-peek))

(defun sleep (seconds)
  (blt/ll:terminal-delay (truncate (* seconds 1000))))


(defun width ()
  (blt/ll:terminal-state blt/ll:+tk-width+))

(defun height ()
  (blt/ll:terminal-state blt/ll:+tk-height+))


(defun cell-code (x y &optional (index 0))
  (let ((code (blt/ll:terminal-pick x y index)))
    (if (zerop code)
      nil
      code)))

(defun cell-char (x y &optional (index 0))
  (let ((code (cell-code x y index)))
    (when code (code-point-to-character code))))


(defun (setf cell-code) (code-point x y)
  (blt/ll:terminal-put x y code-point))

(defun (setf cell-char) (character x y)
  (blt/ll:terminal-put x y (character-to-code-point character)))


(defun cell-color (x y &optional (index 0))
  (blt/ll:terminal-pick-color x y index))

(defun cell-background-color (x y)
  (blt/ll:terminal-pick-bkcolor x y))


(defun print (x y string &key
              width
              height
              (halign :default)
              (valign :default))
  (cffi:with-foreign-objects ((measured-width :int)
                              (measured-height :int))
    (blt/ll:terminal-print-ext-8 x y
                                 (or width 0)
                                 (or height 0)
                                 (logior (horizontal-alignment halign)
                                         (vertical-alignment valign))
                                 string
                                 measured-width
                                 measured-height)
    (values (cffi:mem-ref measured-width :int)
            (cffi:mem-ref measured-height :int))))


;;;; Higher-Level API ---------------------------------------------------------
(defmacro defuck-floats (&body body)
  #+sbcl
  `(sb-int:with-float-traps-masked
     (:inexact :underflow :overflow :invalid :divide-by-zero)
     ,@body)
  #-(or sbcl)
  `(progn ,@body))

(defmacro with-terminal (&body body)
  `(defuck-floats
     (open)
     (unwind-protect
         (progn ,@body)
       (close))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-integer (event)
    (ecase event
      (:A               blt/ll:+tk-a+)
      (:B               blt/ll:+tk-b+)
      (:C               blt/ll:+tk-c+)
      (:D               blt/ll:+tk-d+)
      (:E               blt/ll:+tk-e+)
      (:F               blt/ll:+tk-f+)
      (:G               blt/ll:+tk-g+)
      (:H               blt/ll:+tk-h+)
      (:I               blt/ll:+tk-i+)
      (:J               blt/ll:+tk-j+)
      (:K               blt/ll:+tk-k+)
      (:L               blt/ll:+tk-l+)
      (:m               blt/ll:+tk-m+)
      (:n               blt/ll:+tk-n+)
      (:o               blt/ll:+tk-o+)
      (:p               blt/ll:+tk-p+)
      (:q               blt/ll:+tk-q+)
      (:r               blt/ll:+tk-r+)
      (:s               blt/ll:+tk-s+)
      (:t               blt/ll:+tk-t+)
      (:u               blt/ll:+tk-u+)
      (:v               blt/ll:+tk-v+)
      (:w               blt/ll:+tk-w+)
      (:x               blt/ll:+tk-x+)
      (:y               blt/ll:+tk-y+)
      (:z               blt/ll:+tk-z+)
      (:1               blt/ll:+tk-1+)
      (:2               blt/ll:+tk-2+)
      (:3               blt/ll:+tk-3+)
      (:4               blt/ll:+tk-4+)
      (:5               blt/ll:+tk-5+)
      (:6               blt/ll:+tk-6+)
      (:7               blt/ll:+tk-7+)
      (:8               blt/ll:+tk-8+)
      (:9               blt/ll:+tk-9+)
      (:0               blt/ll:+tk-0+)
      (:return          blt/ll:+tk-return+)
      (:enter           blt/ll:+tk-enter+)
      (:escape          blt/ll:+tk-escape+)
      (:backspace       blt/ll:+tk-backspace+)
      (:tab             blt/ll:+tk-tab+)
      (:space           blt/ll:+tk-space+)
      (:minus           blt/ll:+tk-minus+)
      (:equals          blt/ll:+tk-equals+)
      (:lbracket        blt/ll:+tk-lbracket+)
      (:rbracket        blt/ll:+tk-rbracket+)
      (:backslash       blt/ll:+tk-backslash+)
      (:semicolon       blt/ll:+tk-semicolon+)
      (:apostrophe      blt/ll:+tk-apostrophe+)
      (:grave           blt/ll:+tk-grave+)
      (:comma           blt/ll:+tk-comma+)
      (:period          blt/ll:+tk-period+)
      (:slash           blt/ll:+tk-slash+)
      (:f1              blt/ll:+tk-f-1+)
      (:f2              blt/ll:+tk-f-2+)
      (:f3              blt/ll:+tk-f-3+)
      (:f4              blt/ll:+tk-f-4+)
      (:f5              blt/ll:+tk-f-5+)
      (:f6              blt/ll:+tk-f-6+)
      (:f7              blt/ll:+tk-f-7+)
      (:f8              blt/ll:+tk-f-8+)
      (:f9              blt/ll:+tk-f-9+)
      (:f10             blt/ll:+tk-f-10+)
      (:f11             blt/ll:+tk-f-11+)
      (:f12             blt/ll:+tk-f-12+)
      (:pause           blt/ll:+tk-pause+)
      (:insert          blt/ll:+tk-insert+)
      (:home            blt/ll:+tk-home+)
      (:page-up         blt/ll:+tk-pageup+)
      (:delete          blt/ll:+tk-delete+)
      (:end             blt/ll:+tk-end+)
      (:page-down       blt/ll:+tk-pagedown+)
      (:right           blt/ll:+tk-right+)
      (:left            blt/ll:+tk-left+)
      (:down            blt/ll:+tk-down+)
      (:up              blt/ll:+tk-up+)
      (:numpad-divide   blt/ll:+tk-kp-divide+)
      (:numpad-multiply blt/ll:+tk-kp-multiply+)
      (:numpad-minus    blt/ll:+tk-kp-minus+)
      (:numpad-plus     blt/ll:+tk-kp-plus+)
      (:numpad-enter    blt/ll:+tk-kp-enter+)
      (:numpad-1        blt/ll:+tk-kp-1+)
      (:numpad-2        blt/ll:+tk-kp-2+)
      (:numpad-3        blt/ll:+tk-kp-3+)
      (:numpad-4        blt/ll:+tk-kp-4+)
      (:numpad-5        blt/ll:+tk-kp-5+)
      (:numpad-6        blt/ll:+tk-kp-6+)
      (:numpad-7        blt/ll:+tk-kp-7+)
      (:numpad-8        blt/ll:+tk-kp-8+)
      (:numpad-9        blt/ll:+tk-kp-9+)
      (:numpad-0        blt/ll:+tk-kp-0+)
      (:numpad-period   blt/ll:+tk-kp-period+)
      (:shift           blt/ll:+tk-shift+)
      (:control         blt/ll:+tk-control+)
      (:alt             blt/ll:+tk-alt+)
      (:mouse-left      blt/ll:+tk-mouse-left+)
      (:mouse-right     blt/ll:+tk-mouse-right+)
      (:mouse-middle    blt/ll:+tk-mouse-middle+)
      (:mouse-x1        blt/ll:+tk-mouse-x-1+)
      (:mouse-x2        blt/ll:+tk-mouse-x-2+)
      (:mouse-move      blt/ll:+tk-mouse-move+)
      (:mouse-scroll    blt/ll:+tk-mouse-scroll+)
      (:close           blt/ll:+tk-close+)
      (:resize          blt/ll:+tk-resized+)
      (:none            blt/ll:+tk-input-none+)
      (:cancelled       blt/ll:+tk-input-cancelled+))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-key-case-head (head data-symbol)
    (if (eq t head)
      t
      (destructuring-bind (event &rest modifiers)
          (ensure-list head)
        (let* ((up (member :up modifiers))
               (down (member :down modifiers))
               (up/down (cond ((and up down) :both)
                              (up :up)
                              (down :down)
                              (t :down)))
               (shift (ensure-boolean
                        (member :shift modifiers)))
               (alt (ensure-boolean
                      (intersection modifiers
                                    '(:alt :option :meta))))
               (control (ensure-boolean
                          (intersection modifiers
                                        '(:control :command)))))
          `(and
             ,(ecase up/down
                (:both `(eql (logand ,data-symbol
                                     ,(lognot blt/ll:+tk-key-released+))
                          ,(find-integer event)))
                (:up   `(eql ,data-symbol
                          ,(logior (find-integer event)
                                   blt/ll:+tk-key-released+)))
                (:down `(eql ,data-symbol
                          ,(find-integer event))))
             (,(if shift 'progn 'not)
              (state-boolean blt/ll:+tk-shift+))
             (,(if control 'progn 'not)
              (state-boolean blt/ll:+tk-control+))
             (,(if alt 'progn 'not)
              (state-boolean blt/ll:+tk-alt+))))))))

(defmacro key-case (data &rest clauses)
  (once-only (data)
    `(cond ,@(loop :for (head . body) :in clauses
              :collect `(,(parse-key-case-head head data) ,@body)))))

