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
(defun default-crlf (name)
  "Should a new file called NAME have DOS line endings?  Not Lisp source:
ASDF reads source files as they are, and a carriage return inside a string
that runs over several lines would become part of the string."
  (not (eq (mode-for-file name) 'lisp-mode)))
(defun find-file-buffer (name)
  (or (find name *buffers* :key #'buf-file :test #'equalp)
      (let* ((b (make-buf (string-upcase (file-namestring name)))) (lines (buf-lines b)))
        (setf (buf-file b) name (buf-mode b) (mode-for-file name))
        (cond ((probe-file name)
               (setf (fill-pointer lines) 0 (buf-crlf b) nil)
               ;; The buffer is the file's text cut at its newlines: so a
               ;; file that ends with one -- most do -- has an empty last
               ;; line, which is where point is after the final newline.
               (let ((count 0) (ended t))
                 (with-open-file (in name :external-format :latin-1)
                   (loop (multiple-value-bind (line no-newline) (read-line in nil)
                           (unless line (return))
                           (let ((n (length line)))
                             (when (and (plusp n) (char= (char line (1- n)) #\Return))
                               (setf line (subseq line 0 (1- n)) (buf-crlf b) t))
                             (vector-push-extend line lines))
                           (incf count)
                           (setf ended (not no-newline)))))
                 (when ended (vector-push-extend "" lines))
                 (when (zerop count)                ; an empty file is as good as new
                   (setf (buf-crlf b) (default-crlf name)))
                 (message "Read ~D line~:P from ~A" count name)))
              (t (setf (buf-crlf b) (default-crlf name))
                 (message "(New file)")))
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
    ;; Every line but an empty last one is written with a newline after it:
    ;; so the file ends with one newline whether or not the buffer does (and
    ;; an empty buffer is an empty file).
    (let ((last (1- (nlines b))) (count 0))
      (loop for line across (buf-lines b) for i from 0
            unless (and (= i last) (string= line ""))
              do (write-string line out)
                 (when (buf-crlf b) (write-char #\Return out))
                 (write-char #\Newline out)
                 (incf count))
      (setf (buf-modified b) nil)
      (message "Wrote ~D line~:P to ~A (~A line endings)" count name (if (buf-crlf b) "CRLF" "LF")))))
(defun name-buffer (b file)
  (setf (buf-file b) file
        (buf-name b) (string-upcase (file-namestring file))
        (buf-mode b) (mode-for-file file)))
(define-command save-buffer ()
  "Save the buffer in its file."
  (unless (buf-file *buf*)              ; never had a file: its kind decides the line endings
    (name-buffer *buf* (read-string "File to save in: " :complete #'complete-file))
    (setf (buf-crlf *buf*) (default-crlf (buf-file *buf*))))
  (write-buffer *buf* (buf-file *buf*)))
(define-command write-file ()
  "Save the buffer in a file you name.  It keeps the line endings it has."
  (let ((file (read-string "Write file: " :complete #'complete-file)))
    (when (string= file "") (error 'quit-command))
    (unless (buf-file *buf*) (setf (buf-crlf *buf*) (default-crlf file)))
    (name-buffer *buf* file)
    (write-buffer *buf* file)))
(define-command toggle-line-endings ()
  "Switch the buffer between DOS (CRLF) and Unix (LF) line endings; saving writes them."
  (setf (buf-crlf *buf*) (not (buf-crlf *buf*)) (buf-modified *buf*) t)
  (message "~A will be saved with ~A line endings" (buf-name *buf*) (if (buf-crlf *buf*) "CRLF" "LF")))
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
  (when (and (buffer-precious-p *buf*)
             (not (ask-y-or-n (format nil "~A is modified; kill it anyway?" (buf-name *buf*)))))
    (error 'quit-command))
  (let ((dead *buf*))
    (let ((farewell (mode-hook (buf-mode dead) 'kill-function)))
      (when farewell (funcall farewell dead)))
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
