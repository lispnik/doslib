;;;; The mouse in the editor: click to put the cursor somewhere, drag to
;;;; select, turn the wheel to move through the buffer.
;;;;
;;;;   M-x mouse-mode   turns it off, and on again
;;;;
;;;; It is an idle hook: while the editor waits for a key it asks the mouse
;;;; driver (the library's src/mouse.lisp) where the mouse is.  Pressing the
;;;; left button selects the window under the pointer and puts point and
;;;; the mark there; moving with the button down moves point, and the text
;;;; between becomes the region, as if C-SPC had been typed where the
;;;; button went down.  Then C-w, M-w and the rest work on it.
;;;;
;;;; The pointer is drawn here, by swapping the colours of the cell it is
;;;; on, rather than by the driver: the driver keeps a copy of what was
;;;; under its pointer and puts that back when the mouse moves, which is
;;;; wrong as soon as the editor has redrawn the screen.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defvar *mouse-session* -1 "The process the driver was last looked for in.")
(defvar *mouse-there* nil)
(defvar *mouse-down* nil "True while the left button is held.")
(defvar *mouse-shown* nil "The pointer as drawn: (offset attribute-written attribute-before).")

(defun mouse-ready-p ()
  "Is there a mouse?  Looked for once in each process: the driver is not
part of a saved EMACS.EXE."
  (unless (= *mouse-session* *session*)
    (setf *mouse-session* *session*
          *mouse-there* (and (mouse-init) t)
          *mouse-down* nil *mouse-shown* nil)
    (when *mouse-there* (mouse-wheel-p)))
  *mouse-there*)

(defun erase-mouse-pointer ()
  (when *mouse-shown*
    (destructuring-bind (offset written before) *mouse-shown*
      ;; (if the cell is not as we left it, the screen has been redrawn)
      (when (= (sap-ref-8 *screen* offset) written)
        (setf (sap-ref-8 *screen* offset) before)))
    (setf *mouse-shown* nil)))
(defun draw-mouse-pointer (column row)
  (let* ((offset (1+ (* 2 (+ column (* 80 row)))))
         (before (sap-ref-8 *screen* offset))
         (written (logior (ash (logand before 7) 4) (logand (ash before -4) 7))))
    (when (= written before) (setf written (logxor before #x77)))
    (setf (sap-ref-8 *screen* offset) written
          *mouse-shown* (list offset written before))))

(defun window-at-row (row)
  "The window whose text is on screen row ROW, or NIL (a mode line, the echo area)."
  (find-if (lambda (w) (<= (win-row w) row (+ (win-row w) (win-height w) -1))) *windows*))
(defun select-window (w)
  (unless (eq w *window*)
    (setf (win-point *window*) (cons (point-line) (point-col)))
    (setf *window* w *buf* (win-buf w))
    (let ((point (win-point w)))        ; this window's own place in its buffer
      (when point (goto (car point) (cdr point)))
      (setf (win-point w) nil))))
(defun column-of-visual (text visual)
  "The character of TEXT that screen column VISUAL shows (tabs counted)."
  (or (loop for col from 0 to (length text)
            when (>= (visual-col text col) visual) return col)
      (length text)))
(defun mouse-goto (column row)
  "Put point where the pointer is, in the selected window."
  (let* ((w *window*)
         (line (max 0 (min (1- (nlines)) (+ (win-top w) (- row (win-row w)))))))
    (goto line (column-of-visual (line-text line) (+ column (win-hscroll w))))))

(defun mouse-idle ()
  "The idle hook.  Returns true when the screen needs drawing again."
  (when (and (null *flush-screen*) (mouse-ready-p))     ; (not while M-x live has the display)
    (multiple-value-bind (column row buttons wheel) (mouse-cell)
      (let ((row (min 24 row)) (left (logtest buttons 1)) (changed nil))
        (cond ((and left (not *mouse-down*))            ; pressed
               (let ((w (window-at-row row)))
                 (when w
                   (select-window w)
                   (mouse-goto column row)
                   (setf (buf-mark *buf*) (cons (point-line) (point-col))
                         (buf-mark-active *buf*) nil
                         *mouse-down* t changed t))))
              ((and left *mouse-down*)                  ; held: point follows
               (let ((line (point-line)) (col (point-col)))
                 (mouse-goto column (max (win-row *window*)
                                         (min row (+ (win-row *window*) (win-height *window*) -1))))
                 (unless (and (= line (point-line)) (= col (point-col)))
                   (setf (buf-mark-active *buf*) (not (equal (buf-mark *buf*)
                                                             (cons (point-line) (point-col))))
                         changed t))))
              ((not left) (setf *mouse-down* nil)))
        (unless (zerop wheel)                           ; three lines a notch
          (goto (+ (point-line) (* 3 wheel)) (point-col))
          (setf changed t))
        (unless changed
          (let ((offset (1+ (* 2 (+ column (* 80 row))))))
            (unless (and *mouse-shown* (= (first *mouse-shown*) offset)
                         (= (sap-ref-8 *screen* offset) (second *mouse-shown*)))
              (erase-mouse-pointer)
              (draw-mouse-pointer column row))))
        changed))))

(define-command mouse-mode ()
  "Turn the mouse on or off: click to move the cursor, drag to select."
  (cond ((member 'mouse-idle *idle-hooks*)
         (remove-idle-hook 'mouse-idle)
         (erase-mouse-pointer)
         (message "The mouse is off"))
        ((mouse-ready-p)
         (add-idle-hook 'mouse-idle)
         (message "The mouse is on: click to move the cursor, drag to select"))
        (t (message "There is no mouse driver (CTMOUSE)"))))

(add-idle-hook 'mouse-idle)             ; on, wherever there is a mouse
