;;;; A pop-up calculator on a hot key, in the manner of the DOS "TSR"
;;;; utilities: press F12 while another program is running and a calculator
;;;; appears over it; Esc puts the screen back and the program carries on.
;;;;
;;;; The key comes from the keyboard interrupt (see DOSLIB's hotkey.lisp),
;;;; so the program underneath never sees it and needs no changes: any
;;;; program that paces itself with WAIT-TICK or looks for keys with
;;;; KEY-PRESSED-P can be interrupted.
;;;;
;;;; It takes ordinary arithmetic -- 2^10 + 1, sqrt(2)*3, 0xFF % 7, ans/3 --
;;;; and, this being Lisp, anything in parentheses is a Lisp form:
;;;; (expt 2 100), (loop for i to 10 sum i).  Integers are exact, however big.
;;;;
;;;;   (doslib-examples:run "calc")           Life, with the calculator on F12
;;;;   (doslib-examples:run "calc" "snake")   another example underneath
;;;;   (doslib-examples.calc:install)         keep it for the session
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.calc
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main #:install #:uninstall #:calculate #:popup))
(in-package #:doslib-examples.calc)

(defconstant +f12+ #x58)
(defvar *ans* 0 "The last result.")
(defvar *history* '() "The last few lines shown, newest first.")

;;; ---- arithmetic: a recursive-descent parser for infix expressions
(defvar *text*)
(defvar *pos*)
(defun peek-char* ()
  (loop while (and (< *pos* (length *text*)) (char= (char *text* *pos*) #\Space)) do (incf *pos*))
  (and (< *pos* (length *text*)) (char *text* *pos*)))
(defun take (char) (when (eql (peek-char*) char) (incf *pos*) t))
(defun expect (char) (unless (take char) (error "~C expected" char)))
(defparameter *functions*
  `(("sqrt" . sqrt) ("sin" . sin) ("cos" . cos) ("tan" . tan) ("atan" . atan) ("exp" . exp)
    ("ln" . log) ("log" . ,(lambda (x) (log x 10))) ("abs" . abs) ("fact" . ,(lambda (n)
                                                                              (loop with r = 1 for i from 2 to n
                                                                                    do (setf r (* r i))
                                                                                    finally (return r))))))
(defun primary ()
  (let ((c (peek-char*)))
    (cond ((null c) (error "Something is missing at the end"))
          ((take #\() (prog1 (expression) (expect #\))))
          ((digit-char-p c)
           (let ((start *pos*))
             (if (and (char= c #\0) (< (1+ *pos*) (length *text*)) (char-equal (char *text* (1+ *pos*)) #\x))
                 (progn (incf *pos* 2)
                        (let ((digits *pos*))
                          (loop while (and (< *pos* (length *text*)) (digit-char-p (char *text* *pos*) 16))
                                do (incf *pos*))
                          (parse-integer *text* :start digits :end *pos* :radix 16)))
                 (progn (loop while (and (< *pos* (length *text*))
                                         (or (digit-char-p (char *text* *pos*)) (char= (char *text* *pos*) #\.)))
                              do (incf *pos*))
                        (let ((number (subseq *text* start *pos*)))
                          (if (find #\. number)
                              (let ((*read-default-float-format* 'double-float))
                                (read-from-string (concatenate 'string "0" number "0")))
                              (parse-integer number)))))))
          ((alpha-char-p c)
           (let ((start *pos*))
             (loop while (and (< *pos* (length *text*)) (alphanumericp (char *text* *pos*))) do (incf *pos*))
             (let ((name (string-downcase (subseq *text* start *pos*))))
               (cond ((string= name "ans") *ans*)
                     ((string= name "pi") pi)
                     ((string= name "e") (exp 1d0))
                     ((assoc name *functions* :test #'string=)
                      (expect #\()
                      (let ((argument (expression)))
                        (expect #\))
                        (funcall (cdr (assoc name *functions* :test #'string=))
                                 ;; full precision for the ones that give floats
                                 (if (member name '("abs" "fact") :test #'string=)
                                     argument
                                     (float argument 1d0)))))
                     (t (error "I don't know ~A" name))))))
          (t (error "I can't make sense of ~C" c)))))
(defun unary () (if (take #\-) (- (unary)) (power)))
(defun power ()                         ; right-associative, binds tighter than unary minus
  (let ((base (primary)))
    (if (take #\^) (expt base (unary)) base)))
(defun term ()
  (let ((value (unary)))
    (loop (cond ((take #\*) (setf value (* value (unary))))
                ((take #\/) (setf value (/ value (unary))))
                ((take #\%) (setf value (mod value (unary))))
                (t (return value))))))
(defun expression ()
  (let ((value (term)))
    (loop (cond ((take #\+) (setf value (+ value (term))))
                ((take #\-) (setf value (- value (term))))
                (t (return value))))))
(defun calculate (text)
  "The value of TEXT: arithmetic, or -- if it starts with a parenthesis -- a Lisp form."
  (let ((text (string-trim " " text)))
    (if (and (plusp (length text)) (find (char text 0) "(#'"))
        (let ((*package* (find-package '#:doslib-examples.calc)))
          (eval (read-from-string text)))
        (let ((*text* text) (*pos* 0))
          (prog1 (expression)
            (when (peek-char*) (error "I can't make sense of ~C" (peek-char*))))))))
(defun show-value (value)
  (typecase value
    (integer (if (< -1 value (expt 2 32))
                 (format nil "~D  (~Xh)" value value)
                 (format nil "~D" value)))
    (ratio (let ((*read-default-float-format* 'double-float))
             (format nil "~A  (~A)" value (float value 1d0))))
    (float (let ((*read-default-float-format* 'double-float)) (prin1-to-string value)))
    (t (let ((*print-length* 12) (*print-pretty* nil)) (prin1-to-string value)))))

;;; ---- the window.  In text mode it is a box in the middle of the screen;
;;; in a graphics mode, a few lines at the top written by the BIOS.
(defvar *screen* (int-sap #xB8000))
(defparameter *width* 60)
(defparameter *left* 10)
(defparameter *top* 6)
(defun text-put (x y string attribute)
  (loop for c across string for i from x
        when (< i 80)
          do (setf (sap-ref-16 *screen* (* 2 (+ i (* 80 y))))
                   (logior (logand (char-code c) 255) (ash attribute 8)))))
(defun fit (string width)               ; exactly WIDTH characters, the tail if it is too long
  (let ((string (if (> (length string) width) (subseq string (- (length string) width)) string)))
    (format nil "~VA" width string)))
(defun text-window (lines input)
  (flet ((border (y left fill right)
           (text-put *left* y (format nil "~C~A~C" (code-char left)
                                      (make-string (- *width* 2) :initial-element (code-char fill))
                                      (code-char right))
                     #x1F)))
    (border *top* 201 205 187)
    (text-put (+ *left* 2) *top* " Lisp calculator " #x1E)
    (loop for line in lines for y from (1+ *top*)
          do (text-put *left* y (format nil "~C ~A ~C" (code-char 186) (fit line (- *width* 4)) (code-char 186))
                       #x1F))
    (let ((y (+ *top* 1 (length lines))))
      (text-put *left* y (format nil "~C ~A ~C" (code-char 186) (fit (concatenate 'string "> " input) (- *width* 4))
                                 (code-char 186))
                #x1E)
      (text-put *left* (1+ y) (format nil "~C ~A ~C" (code-char 186)
                                      (fit "Enter: work it out   Esc or F12: back to the program" (- *width* 4))
                                      (code-char 186))
                #x17)
      (border (+ y 2) 200 205 188)
      (int86 #x10 :ax #x0200 :bx 0      ; the cursor, after the input
             :dx (logior (ash y 8) (min 77 (+ *left* 4 (min (length input) (- *width* 6)))))))))
(defun brightest-colour ()              ; whatever the program's palette is, text must show
  (let ((best 15) (most -1))
    (dotimes (i 256 best)
      (outb #x3C7 i)
      (let ((sum (+ (inb #x3C9) (inb #x3C9) (inb #x3C9))))
        (when (> sum most) (setf most sum best i))))))
(defun graphics-window (lines input)
  (dotimes (i (* 320 40)) (setf (sap-ref-8 *vga* i) 0))    ; the BIOS only adds dots
  (let ((colour (brightest-colour)))
    (flet ((row (y string)
             (int86 #x10 :ax #x0200 :bx 0 :dx (ash y 8))
             (loop for c across (fit string 39)
                   do (int86 #x10 :ax (logior #x0E00 (logand (char-code c) 255)) :bx colour))))
      (row 0 "Lisp calculator      Esc: back")
      (row 1 (or (first (last lines)) ""))
      (row 2 (concatenate 'string "> " input)))))

(defun read-key ()                      ; a character, NIL for a key without one, or :CLOSE for F12
  (loop
    (cond ((zerop (logand (nth-value 5 (int86 #x16 :ax #x0100)) #x40))  ; a key is waiting
           (let ((code (lo (int86 #x16 :ax 0))))
             (return (if (zerop code) nil (code-char code)))))
          ((hotkey-pending-p +f12+) (return :close)))))

(defun popup ()
  "The calculator: save the screen, run until Esc, put the screen back."
  (let* ((mode (peek #x449))
         (text-p (member mode '(2 3 7)))
         (size (if text-p 4000 (* 320 40)))
         (memory (if text-p *screen* *vga*))
         (saved (make-array size :element-type '(unsigned-byte 8)))
         (cursor (peek16 #x450)) (cursor-shape (peek16 #x460))
         (input ""))
    (unless (or text-p (= mode #x13)) (return-from popup nil))  ; a mode we cannot draw in
    (dotimes (i size) (setf (aref saved i) (sap-ref-8 memory i)))
    (unwind-protect
         (progn
           (if text-p
               (int86 #x10 :ax #x0100 :cx #x0607)               ; a visible cursor
               (dotimes (i size) (setf (sap-ref-8 memory i) 0)))
           (loop
             (let ((lines (reverse (subseq *history* 0 (min 5 (length *history*))))))
               (if text-p
                   (text-window (append (make-list (- 5 (length lines)) :initial-element "") lines) input)
                   (graphics-window lines input)))
             (let ((key (read-key)))
               (cond ((or (eq key :close) (eql key (code-char 27))) (return))
                     ((eql key #\Return)
                      (unless (string= (string-trim " " input) "")
                        (push (handler-case
                                  (let ((value (calculate input)))
                                    (setf *ans* value)
                                    (format nil "~A = ~A" (string-trim " " input) (show-value value)))
                                (error (e)
                                  (format nil "~A ?  ~A" (string-trim " " input)
                                          (substitute #\Space #\Newline (princ-to-string e)))))
                              *history*)
                        (setf input "")))
                     ((eql key #\Backspace)
                      (setf input (subseq input 0 (max 0 (1- (length input))))))
                     ((and key (graphic-char-p key))
                      (setf input (concatenate 'string input (string key))))))))
      (dotimes (i size) (setf (sap-ref-8 memory i) (aref saved i)))
      (int86 #x10 :ax #x0100 :cx cursor-shape)
      (int86 #x10 :ax #x0200 :bx 0 :dx cursor)
      (clear-pending-hotkeys))))

(defun install ()
  "Put the calculator on F12, for any program that polls for hot keys."
  (install-hotkey +f12+ 'popup))
(defun uninstall () (remove-hotkey +f12+))

(defun main (&optional (underneath "life"))
  "A pop-up calculator on F12, over another example."
  (install)
  (unwind-protect (doslib-examples:run underneath)
    (uninstall)))

(doslib-examples:define-example "calc" 'main
  :category :system
  :description "A pop-up calculator on F12, over another example.")
