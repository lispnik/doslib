;;;; Files and buffers
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defun select-buffer (b)                ; *BUFFERS* is kept most recently used first
  (setf *buffers* (cons b (remove b *buffers*)) (win-point *window*) nil)
  (setf (win-buf *window*) b (win-top *window*) 0 (win-hscroll *window*) 0 *buf* b))
(defun mode-for-file (name)
  (if (member (pathname-type name) '("lsp" "lisp" "cl" "asd" "el") :test #'string-equal)
      'lisp-mode
      'fundamental-mode))
(defun find-file-buffer (name)
  (or (find name *buffers* :key #'buf-file :test #'equalp)
      (let* ((b (make-buf (string-upcase (file-namestring name)))) (lines (buf-lines b)))
        (setf (buf-file b) name (buf-mode b) (mode-for-file name))
        (cond ((probe-file name)
               (setf (fill-pointer lines) 0 (buf-crlf b) nil)
               (with-open-file (in name :external-format :latin-1)
                 (loop for line = (read-line in nil)
                       while line
                       do (let ((n (length line)))
                            (when (and (plusp n) (char= (char line (1- n)) #\Return))
                              (setf line (subseq line 0 (1- n)) (buf-crlf b) t))
                            (vector-push-extend line lines))))
               (when (zerop (length lines)) (vector-push-extend "" lines) (setf (buf-crlf b) t))
               (message "Read ~D line~:P from ~A" (length lines) name))
              (t (message "(New file)")))
        b)))
(defun visit (name)
  "Show the file NAME in this window -- or, if it is a directory, its listing."
  (select-buffer (if (directory-name-p name)
                     (dired-buffer (directory-string name))
                     (find-file-buffer name))))
(define-command find-file ()
  "Visit a file in its own buffer; a directory is opened in Dired."
  (visit (read-string "Find file: " :complete #'complete-file)))
(defun write-buffer (b name)
  (with-open-file (out name :direction :output :if-exists :supersede :external-format :latin-1)
    (loop for line across (buf-lines b)
          do (write-string line out)
             (when (buf-crlf b) (write-char #\Return out))
             (write-char #\Newline out)))
  (setf (buf-modified b) nil)
  (message "Wrote ~D line~:P to ~A" (nlines b) name))
(define-command save-buffer ()
  "Save the buffer in its file."
  (unless (buf-file *buf*)
    (setf (buf-file *buf*) (read-string "File to save in: " :complete #'complete-file)
          (buf-name *buf*) (string-upcase (file-namestring (buf-file *buf*)))
          (buf-mode *buf*) (mode-for-file (buf-file *buf*))))
  (write-buffer *buf* (buf-file *buf*)))
(define-command write-file ()
  "Save the buffer in a file you name."
  (setf (buf-file *buf*) nil)
  (save-buffer))
(define-command load-file () "Load a file of Lisp code."
  (let ((name (read-string "Load file: " :complete #'complete-file)))
    (eval-and-show (format nil "(load ~S)" name))))
(define-command switch-to-buffer ()
  "Show another buffer in this window."
  (let* ((other (or (find-if-not (lambda (b) (find b *windows* :key #'win-buf)) *buffers*)
                    (find *buf* *buffers* :test-not #'eq) *buf*))
         (name (read-string (format nil "Switch to buffer (default ~A): " (buf-name other))
                            :complete (complete-from (mapcar #'buf-name *buffers*)))))
    (select-buffer (cond ((string= name "") other)
                         ((find-buf name))
                         (t (make-buf name))))))
(define-command kill-buffer ()
  "Get rid of the current buffer."
  (when (and (buf-modified *buf*) (buf-file *buf*)
             (not (ask-y-or-n (format nil "~A is modified; kill it anyway?" (buf-name *buf*)))))
    (error 'quit-command))
  (let ((dead *buf*))
    (setf *buffers* (remove dead *buffers*))
    (unless *buffers* (make-buf "*scratch*"))
    (dolist (w *windows*)
      (when (eq (win-buf w) dead)
        (setf (win-buf w) (first *buffers*) (win-top w) 0 (win-point w) nil)))
    (setf *buf* (win-buf *window*))))
(defun show-in-help (name text)
  "Put TEXT in the buffer NAME and show it."
  (let ((b (or (find-buf name) (make-buf name))))
    (setf (fill-pointer (buf-lines b)) 0)
    (dolist (line (split-lines text)) (vector-push-extend line (buf-lines b)))
    (setf (buf-line b) 0 (buf-col b) 0 (buf-undo b) nil (buf-modified b) nil)
    (select-buffer b)))
(define-command list-buffers ()
  "Show all the buffers."
  (show-in-help "*buffers*"
                (format nil "    Buffer                 Lines  Mode         File~%~
~:{ ~A ~22A ~5D  ~12A ~@[~A~]~%~}"
                        (mapcar (lambda (b) (list (if (buf-modified b) "**" "  ") (buf-name b)
                                                  (nlines b) (mode-name (buf-mode b)) (buf-file b)))
                                *buffers*))))
