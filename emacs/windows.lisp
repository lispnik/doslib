;;;; Windows
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defun layout-windows ()
  (let* ((n (length *windows*)) (each (floor (1- *lines*) n)) (row 0))
    (loop for w in *windows* for i from 1
          do (let ((rows (if (= i n) (- *lines* 1 row) each)))
               (setf (win-row w) row (win-height w) (1- rows))
               (incf row rows)))))
(define-command split-window () "Split the window in two."
  (when (>= (length *windows*) 4) (editor-error "Too many windows"))
  (let ((new (make-win :buf *buf* :top (win-top *window*) :point (cons (point-line) (point-col)))))
    (setf *windows* (loop for w in *windows* collect w when (eq w *window*) collect new))
    (layout-windows)))
;;; ---- the size of the screen (see screen.lisp)
(defun show-screen-size ()
  (redisplay)                           ; (which is what changes the screen)
  (message "~D columns, ~D lines" *columns* *lines*))
(define-command toggle-screen-lines ()
  "Switch the screen between 25 lines and 50 (43 on an EGA), in place."
  (when *flush-screen* (editor-error "Not while the editor is on another display"))
  (let ((most (case (display-adapter) ((:vga :mcga) 50) (:ega 43))))
    (unless most (editor-error "This display card has only 25 lines"))
    (screen-size 80 (if (= *lines* 25) most 25)))
  (show-screen-size))
(define-command set-screen-size ()
  "Choose the size of the text screen: 80x25, 80x28 and 80x50 on any VGA
(80x25 and 80x43 on an EGA), and whatever other text modes the VESA BIOS has."
  (when *flush-screen* (editor-error "Not while the editor is on another display"))
  (let* ((sizes (screen-sizes))
         (names (mapcar (lambda (size) (format nil "~Dx~D" (first size) (second size))) sizes))
         (name (string-trim " " (read-string (format nil "Screen size (~{~A~^ ~}): " names))))
         (size (nth (or (position name names :test #'string-equal)
                        (editor-error "There is no ~A text screen here" name))
                    sizes)))
    (setf *screen-size* size *display-stale* t)
    (show-screen-size)))
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
