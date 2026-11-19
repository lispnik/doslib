;;;; Text mode: characters and colours written straight into video memory,
;;;; boxes, a saved screen to pop things up over, and keys by name.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defvar *text-screen* (int-sap #xB8000) "80x25 cells: a character byte, then a colour byte.")

(defun text-put (x y string &optional (attribute 7))
  "Write STRING at column X, row Y, in colour ATTRIBUTE (background * 16 + foreground)."
  (when (<= 0 y 24)
    (loop for c across string for i from x
          when (<= 0 i 79)
            do (setf (sap-ref-16 *text-screen* (* 2 (+ i (* 80 y))))
                     (logior (logand (char-code c) 255) (ash attribute 8)))))
  string)

(defun text-fill (x y width height &optional (attribute 7) (char #\Space))
  "Fill a rectangle with CHAR in colour ATTRIBUTE."
  (let ((line (make-string (max 0 width) :initial-element char)))
    (dotimes (row height) (text-put x (+ y row) line attribute))))

(defun text-box (x y width height &key (attribute 7) title (title-attribute attribute) shadow)
  "A filled box with a double-line border, an optional TITLE and SHADOW."
  (flet ((edge (row left fill right)
           (text-put x row (format nil "~C~A~C" (code-char left)
                                   (make-string (- width 2) :initial-element (code-char fill))
                                   (code-char right))
                     attribute)))
    (when shadow                        ; darken what is beside and below
      (loop for row from (1+ y) to (+ y height)
            do (loop for col from (+ x width) below (+ x width 2)
                     when (and (<= 0 col 79) (<= 0 row 24))
                       do (setf (sap-ref-8 *text-screen* (1+ (* 2 (+ col (* 80 row))))) 8)))
      (loop for col from (+ x 2) below (+ x width 2)
            when (and (<= 0 col 79) (<= 0 (+ y height) 24))
              do (setf (sap-ref-8 *text-screen* (1+ (* 2 (+ col (* 80 (+ y height)))))) 8)))
    (edge y 201 205 187)
    (loop for row from (1+ y) below (+ y height -1) do (edge row 186 32 186))
    (edge (+ y height -1) 200 205 188)
    (when title (text-put (+ x 2) y (format nil " ~A " title) title-attribute))))

;;; (These return no values, so that a program which ends by putting the
;;; cursor back does not leave the BIOS's registers printed at the prompt.)
(defun text-cursor (x y)
  "Put the hardware cursor at X, Y."
  (int86 #x10 :ax #x0200 :bx 0 :dx (logior (ash y 8) x))
  (values))
(defun hide-text-cursor ()
  "Make the hardware cursor invisible."
  (int86 #x10 :ax #x0100 :cx #x2000) (values))
(defun show-text-cursor ()
  "Make the hardware cursor the usual underline."
  (int86 #x10 :ax #x0100 :cx #x0607) (values))

(defun save-text-screen ()
  "The whole text screen and the cursor, for RESTORE-TEXT-SCREEN."
  (let ((cells (make-array 4000 :element-type '(unsigned-byte 8))))
    (dotimes (i 4000) (setf (aref cells i) (sap-ref-8 *text-screen* i)))
    (list cells (peek16 #x450) (peek16 #x460))))

(defun restore-text-screen (saved)
  "Put back the screen and cursor that SAVE-TEXT-SCREEN returned."
  (destructuring-bind (cells cursor shape) saved
    (dotimes (i 4000) (setf (sap-ref-8 *text-screen* i) (aref cells i)))
    (int86 #x10 :ax #x0100 :cx shape)
    (int86 #x10 :ax #x0200 :bx 0 :dx cursor)))

(defparameter *key-names*
  '((#x48 . :up) (#x50 . :down) (#x4B . :left) (#x4D . :right) (#x47 . :home) (#x4F . :end)
    (#x49 . :page-up) (#x51 . :page-down) (#x52 . :insert) (#x53 . :delete)
    (#x3B . :f1) (#x3C . :f2) (#x3D . :f3) (#x3E . :f4) (#x3F . :f5) (#x40 . :f6)
    (#x41 . :f7) (#x42 . :f8) (#x43 . :f9) (#x44 . :f10)))

(defun get-key ()
  "Wait for a key.  Returns its character -- or, for a key that has none, a
keyword such as :UP, :F1 or :DELETE (or the scan code, if it has no name)."
  (let* ((ax (word16 (wait-key))) (ascii (lo ax)) (scan (hi ax)))
    (if (or (zerop ascii) (= ascii #xE0))
        (or (cdr (assoc scan *key-names*)) scan)
        (code-char ascii))))

(defun text-input (x y width &key (attribute 7) (initial ""))
  "Let the user type a line at X, Y in a field WIDTH wide.  Enter returns
the string; Esc returns NIL."
  (let ((text initial))
    (loop
      (let ((shown (if (> (length text) (1- width)) (subseq text (- (length text) (1- width))) text)))
        (text-put x y (format nil "~VA" width shown) attribute)
        (text-cursor (+ x (length shown)) y))
      (let ((key (get-key)))
        (cond ((eql key #\Return) (return text))
              ((eql key (code-char 27)) (return nil))
              ((eql key #\Backspace) (setf text (subseq text 0 (max 0 (1- (length text))))))
              ((and (characterp key) (graphic-char-p key))
               (setf text (concatenate 'string text (string key)))))))))
