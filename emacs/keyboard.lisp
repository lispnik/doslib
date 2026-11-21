;;;; The keyboard: BIOS codes -> key names like "a", "C-x", "M-f", "<up>"
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defun extended-key (scan shifted)
  (flet ((row (first letters)
           (and (<= first scan (+ first (length letters) -1))
                (format nil "M-~C" (char letters (- scan first))))))
    (or (cdr (assoc scan '((#x48 . "<up>") (#x50 . "<down>") (#x4B . "<left>") (#x4D . "<right>")
                           (#x47 . "<home>") (#x4F . "<end>") (#x49 . "<prior>") (#x51 . "<next>")
                           (#x52 . "<insert>") (#x53 . "<delete>") (#x73 . "C-<left>")
                           (#x74 . "C-<right>") (#x77 . "C-<home>") (#x75 . "C-<end>")
                           (#x0F . "<backtab>") (#x03 . "C-@") (#x0E . "M-<backspace>")
                           (#x85 . "<f11>") (#x86 . "<f12>") (#x35 . "M-/")
                           (#xA5 . "M-TAB") (#x2B . "M-\\"))))
        (and (= scan #x82) (if shifted "M-_" "M--"))
        (and (<= #x3B scan #x44) (format nil "<f~D>" (- scan #x3A)))
        (and (= scan #x33) (if shifted "M-<" "M-,"))
        (and (= scan #x34) (if shifted "M->" "M-."))
        (and (= scan #x27) (if shifted "M-:" "M-;"))
        (and (= scan #x7C) shifted "M-%")
        (and (= scan #x80) shifted "M-(")
        (and (= scan #x81) shifted "M-)")
        (row #x10 "qwertyuiop") (row #x1E "asdfghjkl") (row #x2C "zxcvbnm") (row #x78 "1234567890")
        (format nil "<scan-~X>" scan))))
;;; While the editor waits for a key it can do other things: look at a
;;; network connection, move a game along.  An idle hook is a function
;;; called over and over during the wait; it returns true if it changed a
;;; buffer, and the screen is then drawn again.  The same wait is where hot
;;; keys (DOSLIB's INSTALL-HOTKEY) get their turn.  With neither, the
;;; editor sleeps in the BIOS until a key arrives.
(defvar *idle-hooks* '() "Functions to call while waiting for a key at the top level.")
(defvar *idle* nil "True while the command loop, not the minibuffer, is reading a key.")
(defun add-idle-hook (function) (pushnew function *idle-hooks*) function)
(defun remove-idle-hook (function) (setf *idle-hooks* (remove function *idle-hooks*)) nil)
(defun wait-for-key ()
  "Returns the Shift state at the moment a key was seen, if it waited."
  (when (and *idle* (or *idle-hooks* (hotkeys-installed-p)))
    ;; (Shift may be let go before we come to fetch the key, a hook or a
    ;; redisplay later: so look at once.)
    (loop (when (key-pressed-p) (return (peek #x417)))
          (let ((changed nil))
               (dolist (hook *idle-hooks*)
                 (handler-case (when (funcall hook) (setf changed t))
                   (quit-command () (setf changed t))   ; Ctrl-Break: it gets another turn
                   (error (e)           ; a broken hook must not wedge the editor
                     (remove-idle-hook hook)
                     (message "Idle hook removed after an error: ~A" e)
                     (setf changed t))))
               (when (and changed (not (key-pressed-p))) (redisplay))))))

(defun read-raw-key ()
  (if *unread*
      (pop *unread*)
      (let* ((early-shift (wait-for-key))
             ;; (a key of 0000 is what the BIOS leaves behind for Ctrl-Break)
             (ax (loop for ax = (word16 (int86 #x16 :ax #x1000))
                       unless (zerop ax) return ax))
             (ascii (lo ax)) (scan (hi ax))
             (shift (logior (or early-shift 0) (peek #x417)))
             (key (cond ((and (member ascii '(0 #xE0)) (/= scan 0)) (extended-key scan (logtest shift 3)))
                        ((= ascii 8) (if (= scan #x0E) "<backspace>" "C-h"))
                        ((= ascii 9) "TAB")
                        ((= ascii 13) "RET")
                        ((= ascii 27) "ESC")
                        ((= ascii 127) "C-<backspace>")
                        ((< ascii 27) (format nil "C-~C" (code-char (+ ascii 96))))
                        ((< ascii 32) (format nil "C-~C" (code-char (+ ascii 64))))
                        ((= ascii 32) (if (logtest shift 4) "C-SPC" "SPC"))
                        (t (string (code-char ascii))))))
        ;; Ctrl and Alt together: the BIOS reports the Alt key, and Ctrl is in the shift state
        (when (and (logtest shift 4) (> (length key) 2) (string= "M-" key :end2 2))
          (setf key (concatenate 'string "C-" key)))
        (when *defining-macro* (push key *macro-keys*))  ; keys from the keyboard only
        key)))
(defun read-key ()
  "The next key, by name.  ESC followed by a key is Meta: ESC f is M-f, ESC C-f is C-M-f."
  (let ((key (read-raw-key)))
    (if (equal key "ESC")
        (let ((next (read-raw-key)))
          (if (and (> (length next) 2) (string= "C-" next :end2 2))
              (concatenate 'string "C-M-" (subseq next 2))
              (concatenate 'string "M-" next)))
        key)))
