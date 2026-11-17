;;;; Windows
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defun layout-windows ()
  (let* ((n (length *windows*)) (each (floor 24 n)) (row 0))
    (loop for w in *windows* for i from 1
          do (let ((rows (if (= i n) (- 24 row) each)))
               (setf (win-row w) row (win-height w) (1- rows))
               (incf row rows)))))
(define-command split-window () "Split the window in two."
  (when (>= (length *windows*) 4) (editor-error "Too many windows"))
  (let ((new (make-win :buf *buf* :top (win-top *window*) :point (cons (point-line) (point-col)))))
    (setf *windows* (loop for w in *windows* collect w when (eq w *window*) collect new))
    (layout-windows)))
(define-command delete-other-windows () "Make this window fill the screen."
  (setf *windows* (list *window*))
  (layout-windows))
(define-command other-window () "Select the next window."
  (setf (win-point *window*) (cons (point-line) (point-col)))
  (setf *window* (or (second (member *window* *windows*)) (first *windows*))
        *buf* (win-buf *window*))
  (let ((point (win-point *window*)))   ; this window's own place in its buffer
    (when point (goto (car point) (cdr point)))
    (setf (win-point *window*) nil)))
(define-command switch-to-repl ()
  "Show the Lisp REPL in another window and go there."
  (when (null (rest *windows*)) (split-window))
  (unless (eq (buf-mode *buf*) 'repl-mode)
    (other-window)
    (select-buffer (repl-buffer))
    (end-of-buffer)))
