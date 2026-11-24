;;;; The mouse, through the mouse driver (interrupt 33h).
;;;;
;;;; A mouse driver has to be loaded (CTMOUSE, with FreeDOS).  The driver
;;;; keeps the position in a "virtual screen" that is 640 across in every
;;;; BIOS mode up to 13h: in 80-column text a character cell is 8 by 8 of
;;;; those units, and in the 320-wide graphics modes a pixel is 2 across.
;;;; MOUSE-POSITION gives the driver's numbers; MOUSE-CELL and MOUSE-PIXEL
;;;; turn them into character cells and pixels.
;;;;
;;;; Nothing here uses the driver's call-backs: a program asks where the
;;;; mouse is whenever it wants to know.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defun mouse-driver-p ()
  "Is there a handler on interrupt 33h?  (Calling it without one would crash.)"
  (multiple-value-bind (ax bx cx dx) (int86 #x31 :ax #x0200 :bx #x33)   ; the real-mode vector
    (declare (ignore ax bx))
    (let ((segment (word16 cx)) (offset (word16 dx)))
      (and (not (and (zerop segment) (zerop offset)))
           (/= (peek (linear segment offset)) #xCF)))))          ; an IRET is no driver

(defun mouse-init ()
  "Reset the mouse driver.  Returns the number of buttons, or NIL if there
is no driver or no mouse.  The pointer starts hidden, in the middle."
  (when (mouse-driver-p)
    (multiple-value-bind (ax bx) (int86 #x33 :ax 0)
      (when (= (word16 ax) #xFFFF)
        (let ((buttons (word16 bx)))
          (if (or (zerop buttons) (= buttons #xFFFF)) 2 buttons))))))

(defun mouse-show ()
  "Show the driver's pointer.  (Calls to MOUSE-HIDE nest: each needs a MOUSE-SHOW.)"
  (int86 #x33 :ax 1) (values))
(defun mouse-hide ()
  "Take the driver's pointer off the screen."
  (int86 #x33 :ax 2) (values))
(defmacro with-mouse-hidden (&body body)
  "Run BODY with the pointer off the screen: for drawing where it might be."
  `(progn (mouse-hide) (unwind-protect (progn ,@body) (mouse-show))))

(defun mouse-position ()
  "Where the mouse is and what is pressed.  Returns X, Y (the driver's
units), the buttons -- bit 0 is the left one, bit 1 the right, bit 2 the
middle -- and how far the wheel has turned since this was last asked
(negative is away from you; always 0 unless MOUSE-WHEEL-P has said there is one)."
  (multiple-value-bind (ax bx cx dx) (int86 #x33 :ax 3)
    (declare (ignore ax))
    (let ((wheel (hi bx)))
      (values (word16 cx) (word16 dx) (logand bx 7)
              (if (logbitp 7 wheel) (- wheel 256) wheel)))))
(defun mouse-wheel-p ()
  "Does the mouse have a wheel that the driver reports (CTMOUSE's wheel
interface)?  Asking turns the reporting on."
  (multiple-value-bind (ax bx cx) (int86 #x33 :ax #x11)
    (declare (ignore bx))
    (and (= (word16 ax) #x574D) (logbitp 0 cx))))
(defun mouse-cell ()
  "The character cell the mouse is in: column, row, the buttons and the wheel."
  (multiple-value-bind (x y buttons wheel) (mouse-position)
    ;; (the BIOS says how many columns and rows the text screen has)
    (values (min (1- (peek16 #x44A)) (floor x 8)) (min (peek #x484) (floor y 8)) buttons wheel)))
(defun mouse-pixel ()
  "The pixel the mouse is at in a graphics mode: x, y, the buttons and the wheel."
  (multiple-value-bind (x y buttons wheel) (mouse-position)
    ;; (the BIOS says how many text columns the mode has: 40 means 320 across)
    (values (if (= (peek16 #x44A) 40) (ash x -1) x) y buttons wheel)))
(defun mouse-move (x y)
  "Put the mouse at X, Y (the driver's units)."
  (int86 #x33 :ax 4 :cx x :dx y) (values))
(defun mouse-range (x0 y0 x1 y1)
  "Keep the mouse inside the rectangle from X0, Y0 to X1, Y1."
  (int86 #x33 :ax 7 :cx x0 :dx x1)
  (int86 #x33 :ax 8 :cx y0 :dx y1)
  (values))
(defun mouse-presses (&optional (button 0))
  "How often BUTTON (0 left, 1 right, 2 middle) has been pressed since this
was last asked, and where the last press was: count, x, y."
  (multiple-value-bind (ax bx cx dx) (int86 #x33 :ax 5 :bx button)
    (declare (ignore ax))
    (values (word16 bx) (word16 cx) (word16 dx))))
(defun mouse-releases (&optional (button 0))
  "As MOUSE-PRESSES, for the button being let go."
  (multiple-value-bind (ax bx cx dx) (int86 #x33 :ax 6 :bx button)
    (declare (ignore ax))
    (values (word16 bx) (word16 cx) (word16 dx))))
(defun mouse-motion ()
  "How far the mouse has moved since this was last asked, in mickeys (the
mouse's own units, whatever the screen): dx and dy, negative for left and up."
  (flet ((signed (word) (if (logbitp 15 word) (- word 65536) word)))
    (multiple-value-bind (ax bx cx dx) (int86 #x33 :ax #x0B)
      (declare (ignore ax bx))
      (values (signed (word16 cx)) (signed (word16 dx))))))
