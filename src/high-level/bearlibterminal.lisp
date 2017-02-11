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


(defun rgba (r g b a)
  (-<> 0
    (dpb a (byte 2 6) <>)
    (dpb r (byte 2 4) <>)
    (dpb g (byte 2 2) <>)
    (dpb b (byte 2 0) <>)))

(defun name (color-name)
  (blt/ll:color-from-name color-name))


(defun boolean-to-onoff (boolean)
  (if boolean
    blt/ll:+tk-on+
    blt/ll:+tk-off+))

(defun onoff-to-boolean (onoff)
  (ecase onoff
    (blt/ll:+tk-on+ t)
    (blt/ll:+tk-off+ nil)))

(defun int-to-boolean (int)
  (not (zerop int)))


(defun state-boolean (state)
  (int-to-boolean (blt/ll:terminal-state state)))


;;;; Error Checking -----------------------------------------------------------
(define-condition bearlibterminal-error (error) ())

(defun check (return-value)
  (if (zerop return-value)
    (error 'bearlibterminal-error)))


;;;; Wrappers -----------------------------------------------------------------
(defun terminal-open ()
  (check (blt/ll:terminal-open)))

(defun terminal-close ()
  (blt/ll:terminal-close))

(defun terminal-set (configuration-string)
  (check (blt/ll:terminal-set-8 configuration-string)))


(defun terminal-refresh ()
  (blt/ll:terminal-refresh))

(defun terminal-clear ()
  (blt/ll:terminal-clear))

(defun terminal-clear-area (x y width height)
  (blt/ll:terminal-clear-area x y width height))

(defun terminal-crop (x y width height)
  (blt/ll:terminal-crop x y width height))


(defun terminal-layer ()
  (blt/ll:terminal-state blt/ll:+tk-layer+))

(defun (setf terminal-layer) (new-value)
  (blt/ll:terminal-layer new-value)
  new-value)


(defun terminal-color ()
  (blt/ll:terminal-state blt/ll:+tk-color+))

(defun (setf terminal-color) (color)
  (blt/ll:terminal-color color))


(defun terminal-background-color ()
  (blt/ll:terminal-state blt/ll:+tk-bkcolor+))

(defun (setf terminal-background-color) (color)
  (blt/ll:terminal-bkcolor color))


(defun terminal-composition ()
  (onoff-to-boolean (blt/ll:terminal-state blt/ll:+tk-composition+)))

(defun (setf terminal-composition) (new-value)
  (blt/ll:terminal-composition (boolean-to-onoff new-value))
  new-value)


(defun terminal-has-input-p ()
  (int-to-boolean (blt/ll:terminal-has-input)))

(defun terminal-read ()
  (blt/ll:terminal-read))

(defun terminal-peek ()
  (blt/ll:terminal-peek))

(defun terminal-delay (seconds)
  (blt/ll:terminal-delay (truncate (* seconds 1000))))


(defun terminal-put-char (x y char)
  (blt/ll:terminal-put x y (char-code char)))

(defun terminal-put-code (x y code-point)
  (blt/ll:terminal-put x y code-point))

(defun terminal-width ()
  (blt/ll:terminal-state blt/ll:+tk-width+))

(defun terminal-height ()
  (blt/ll:terminal-state blt/ll:+tk-height+))


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
     (terminal-open)
     (unwind-protect
         (progn ,@body)
       (terminal-close))))


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


;;;; Scratch ------------------------------------------------------------------
(defun test ()
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (with-terminal
      (terminal-refresh)
      (terminal-set "input.filter = [keyboard+, mouse+]")
      (loop
        :for data = (terminal-read)
        :do (pr data)
        :while (key-case data
                 ((:a :down) (pr "A down") t)
                 ((:a :up) (pr "A up") t)
                 ((:a :control) (pr "ctrl a") t)
                 ((:b :control :shift) (pr "shift-ctrl b") t)
                 ((:b :down :up) (pr "B down or up") t)
                 (:escape nil)
                 (t (pr "something else") t))))))
