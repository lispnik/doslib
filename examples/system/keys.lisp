;;;; Which keys are held down: every scan code the keyboard sends
;;;;
;;;; The BIOS hands over keys one at a time as they are typed, and says
;;;; nothing when one is let go.  A game needs to know that left and fire
;;;; are both down now.  The keyboard says so itself: interrupt request 1
;;;; for every press and every release, with a scan code at port 60h (the
;;;; key's number, plus 80h for a release).  The BIOS reads it and offers
;;;; it to interrupt 15h before using it; the library's WITH-KEY-STATE puts
;;;; a few real-mode instructions there that keep every scan code, and
;;;; KEY-DOWN-P and KEYS-DOWN then answer from Lisp.
;;;;
;;;; This draws the keyboard's 88 scan codes and lights the ones held down.
;;;; Hold several at once: most keyboards manage any two, and some
;;;; combinations of three or more go missing or produce a "ghost" -- that
;;;; is the keyboard's wiring, and a game's key choices had to avoid it.
;;;;
;;;; Esc leaves (when it is let go).  Ctrl-Break still interrupts: the BIOS
;;;; sees every key too.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.keys
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.keys)

(defparameter *names*                   ; scan code -> what is on the key (US layout)
  #("" "Esc" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" "Bksp" "Tab"
    "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "[" "]" "Enter" "Ctrl" "A" "S"
    "D" "F" "G" "H" "J" "K" "L" ";" "'" "`" "LShft" "\\" "Z" "X" "C" "V"
    "B" "N" "M" "," "." "/" "RShft" "Kp*" "Alt" "Space" "Caps" "F1" "F2" "F3" "F4" "F5"
    "F6" "F7" "F8" "F9" "F10" "Num" "Scrl" "Home" "Up" "PgUp" "Kp-" "Left" "Kp5" "Right" "Kp+" "End"
    "Down" "PgDn" "Ins" "Del" "SysRq" "" "\\|" "F11" "F12"))

(defun key-name (code)
  (if (and (< code (length *names*)) (plusp (length (aref *names* code))))
      (aref *names* code)
      (format nil "~D" code)))

(defun draw (down)
  (loop for code from 1 below (length *names*)
        do (multiple-value-bind (row column) (floor (1- code) 11)
             (text-put (+ 2 (* 7 column)) (+ 4 (* 2 row))
                       (format nil "~6A" (key-name code))
                       (if (member code down) #x2F 8)))))

(defun keys ()
  (let ((most 0) (last nil) (seen '()))
    (unwind-protect
         (progn
           (video-mode 3) (hide-text-cursor)
           (text-put 2 0 "Which keys are held down" 11)
           (text-put 2 1 "Every scan code the keyboard sends, kept.  Hold several; Esc leaves." 7)
           (with-key-state
             (loop with escape = nil
                   do (let ((down (keys-down)))
                        (dolist (code down)
                          (unless (member code seen) (setf last code)))
                        (setf seen down most (max most (length down)))
                        (draw down)
                        (text-put 2 21 (format nil "Down now: ~D   Most at once: ~D   ~
                                                    Presses and releases: ~D     "
                                               (length down) most (poll-key-state))
                                  15)
                        (text-put 2 22 (format nil "Last key: ~:[none~;~:*~A (scan code ~D)~]      "
                                               (and last (key-name last)) last)
                                  15)
                        (cond ((member +key-escape+ down) (setf escape t))
                              (escape (return))))
                      (wait-frame))))
      (video-mode 3))
    (format t "~&Most keys held at once: ~D.~%" most)))

(defun main ()
  "Which keys are held down, from every scan code the keyboard sends."
  (keys))

(doslib-examples:define-example "keys" 'main
  :category :system
  :description "Which keys are held down, from every scan code the keyboard sends.")
