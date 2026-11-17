;;;; Movement
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(define-command forward-char ()
  "Move point right one character."
  (cond ((< (point-col) (length (current-line))) (goto (point-line) (1+ (point-col))))
        ((< (point-line) (1- (nlines))) (goto (1+ (point-line)) 0))
        (t (editor-error "End of buffer"))))
(define-command backward-char ()
  "Move point left one character."
  (cond ((> (point-col) 0) (goto (point-line) (1- (point-col))))
        ((> (point-line) 0) (goto (1- (point-line)) (length (line-text (1- (point-line))))))
        (t (editor-error "Beginning of buffer"))))
(defun move-lines (n)
  (unless (member *last-command* '(next-line previous-line scroll-up scroll-down))
    (setf (buf-goal *buf*) (point-col)))
  (goto (+ (point-line) n) (buf-goal *buf*)))
(define-command next-line () "Move point down one line." (move-lines 1))
(define-command previous-line () "Move point up one line." (move-lines -1))
(define-command beginning-of-line () "Move point to the start of the line." (goto (point-line) 0))
(define-command end-of-line () "Move point to the end of the line."
  (goto (point-line) (length (current-line))))
(define-command beginning-of-buffer () "Move point to the start of the buffer." (goto 0 0))
(define-command end-of-buffer () "Move point to the end of the buffer."
  (goto (1- (nlines)) (length (line-text (1- (nlines))))))
(define-command scroll-up () "Move forward one screenful."
  (move-lines (max 1 (- (win-height *window*) 2))))
(define-command scroll-down () "Move back one screenful."
  (move-lines (- (max 1 (- (win-height *window*) 2)))))
(define-command recenter () "Put the line with point in the middle of the window."
  (setf (win-top *window*) (max 0 (- (point-line) (floor (win-height *window*) 2)))))
(defun char-at (line col)
  (let ((text (line-text line))) (if (< col (length text)) (char text col) #\Newline)))
(defun at-end-p () (and (= (point-line) (1- (nlines))) (= (point-col) (length (current-line)))))
(defun at-start-p () (and (= (point-line) 0) (= (point-col) 0)))
(define-command forward-word ()
  "Move point to the end of the next word."
  (loop until (or (at-end-p) (alphanumericp (char-at (point-line) (point-col)))) do (forward-char))
  (loop while (and (not (at-end-p)) (alphanumericp (char-at (point-line) (point-col)))) do (forward-char)))
(define-command backward-word ()
  "Move point to the start of the previous word."
  (flet ((before () (if (> (point-col) 0) (char-at (point-line) (1- (point-col))) #\Newline)))
    (loop until (or (at-start-p) (alphanumericp (before))) do (backward-char))
    (loop while (and (not (at-start-p)) (alphanumericp (before))) do (backward-char))))
(define-command goto-line ()
  "Go to a line: the one given as a prefix argument (C-u 42 M-g), or asked for."
  (goto (1- (or *prefix-argument* (parse-integer (read-string "Go to line: ")))) 0))
(setf (get 'goto-line 'takes-prefix) t)
