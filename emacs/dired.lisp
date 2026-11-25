;;;; Dired: a directory as a buffer.  Each line is a file; keys act on it.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defstruct dentry name dir size time flag)
(defparameter *dired-name-column* 34)
(defun cwd-directory ()                 ; "D:\SBCL\"
  (current-directory-string))
(defun directory-string (text)
  "TEXT as the name of a directory: absolute, backslashes, one at the end."
  (let ((d (substitute #\\ #\/ (string-trim " " text))))
    (when (string= d "") (setf d (cwd-directory)))
    (unless (or (find #\: d) (char= (char d 0) #\\))
      (setf d (concatenate 'string (cwd-directory) d)))
    (if (char= (char d (1- (length d))) #\\) d (concatenate 'string d "\\"))))
(defun parent-directory (dir)
  (let* ((trimmed (string-right-trim "\\" dir)) (cut (position #\\ trimmed :from-end t)))
    (if cut (subseq trimmed 0 (1+ cut)) dir)))
(defun directory-name-p (name)
  (and (plusp (length name))
       (or (find (char name (1- (length name))) "\\/")
           (directory-exists-p (string-right-trim "\\/" name)))))
(defun read-directory (dir)
  "The files in DIR as a list of DENTRYs."
  (loop for (name directory-p size time) in (list-directory dir)
        collect (make-dentry :name name :dir directory-p :size size :time time)))
(defun dired-sorted (entries key)       ; directories first
  (sort (copy-list entries)
        (lambda (a b)
          (cond ((and (dentry-dir a) (not (dentry-dir b))) t)
                ((and (dentry-dir b) (not (dentry-dir a))) nil)
                (t (ecase key
                     (:name (string-lessp (dentry-name a) (dentry-name b)))
                     (:date (> (dentry-time a) (dentry-time b)))
                     (:size (> (dentry-size a) (dentry-size b)))))))))
(defun dired-line (entry)
  (if (string= (dentry-name entry) "..")
      (format nil "  ~12@A  ~16A  .." "<DIR>" "")
      (multiple-value-bind (second minute hour day month year) (decode-universal-time (dentry-time entry) 0)
        (declare (ignore second))
        (format nil "~C ~12@A  ~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D  ~A"
                (if (dentry-flag entry) #\D #\Space)
                (if (dentry-dir entry) "<DIR>" (format nil "~:D" (dentry-size entry)))
                year month day hour minute (dentry-name entry)))))
(defun dired-render (b)
  "Write the buffer's entries out as its text."
  (let ((lines (buf-lines b)) (entries (buf-entries b)))
    (setf (fill-pointer lines) 0)
    (vector-push-extend (format nil " Directory of ~A   ~D file~:P, by ~(~A~)   (? for help)" (buf-dir b)
                                (count ".." entries :key #'dentry-name :test-not #'string=)
                                (buf-sort b))
                        lines)
    (vector-push-extend "" lines)
    (dolist (entry entries) (vector-push-extend (dired-line entry) lines))
    (setf (buf-undo b) nil (buf-redo b) nil (buf-modified b) nil
          (buf-line b) (max 2 (min (buf-line b) (1- (length lines))))
          (buf-col b) (min *dired-name-column* (length (aref lines (buf-line b)))))))
(defun dired-read (b &optional keep)
  "Read the buffer's directory again; put point on the file named KEEP if it is there."
  (let ((dir (buf-dir b)))
    (setf (buf-entries b)
          (append (unless (string= (parent-directory dir) dir) (list (make-dentry :name ".." :dir t)))
                  (dired-sorted (read-directory dir) (buf-sort b))))
    (let ((index (and keep (position keep (buf-entries b) :key #'dentry-name :test #'string-equal))))
      (when index (setf (buf-line b) (+ 2 index))))
    (dired-render b)))
(defun dired-buffer (dir)
  (or (find dir *buffers* :key #'buf-dir :test #'equalp)
      (let ((b (make-buf dir)))
        (unless (directory-name-p dir) (editor-error "~A is not a directory" dir))
        (setf (buf-mode b) 'dired-mode (buf-read-only b) t (buf-dir b) dir (buf-line b) 2)
        (dired-read b)
        b)))
(defun dired-entry ()
  (or (and (>= (point-line) 2) (nth (- (point-line) 2) (buf-entries *buf*)))
      (editor-error "No file on this line")))
(defun dired-path (entry) (concatenate 'string (buf-dir *buf*) (dentry-name entry)))
(defun dired-real-entry ()              ; not the ".." line
  (let ((entry (dired-entry)))
    (when (string= (dentry-name entry) "..") (editor-error "Not on a file"))
    entry))
;;; Colour, after GNU Emacs's Dired: the heading, directories, the files
;;; flagged for deletion, and -- dimmed -- the files nobody looks for:
;;; backups and what compilers leave behind.  All of it can be changed from
;;; EMACSRC.LSP: (setf doslib-emacs:*dired-colours* nil) for none,
;;; (push "log" doslib-emacs:*dired-ignored-extensions*) to dim more,
;;; (setf (getf doslib-emacs:*dired-faces* :directory) 11) for another colour.
(defvar *dired-colours* t
  "True if Dired colours its lines by kind of file.  M-x dired-toggle-colours changes it.")
(defvar *dired-faces*
  (list :header 14 :directory 9 :flagged 12 :ignored 8 :program 10 :source 15 :details 7)
  "Dired's colours, by what is coloured: the heading line, a directory's name,
a line flagged for deletion, an ignored file's line, a program's name, the
name of Lisp source, and the size and date.  Each is a text attribute: 0 to
15 for a colour on black (8 is dark grey, 9 bright blue, 10 bright green,
12 bright red, 14 yellow, 15 white).")
(defvar *dired-ignored-extensions*
  (list "fasl" "fas" "bak" "tmp" "old" "obj" "o" "$$$" "swp")
  "A file whose name ends with a dot and one of these is dimmed in Dired: a
backup, or something a compiler made that can be made again.")
(defvar *dired-program-extensions* (list "exe" "com" "bat")
  "A file whose name ends with a dot and one of these is a program, to Dired.")
(defvar *dired-source-extensions* (list "lisp" "lsp" "asd" "cl")
  "A file whose name ends with a dot and one of these is Lisp source, to Dired.")
(defun name-extension (name)
  (let ((dot (position #\. name :from-end t)))
    (if (and dot (plusp dot)) (subseq name (1+ dot)) "")))
(defun dired-ignored-p (name)
  "Is NAME a backup or a compiler's leavings: file~, #file#, or an ignored extension?"
  (let ((n (length name)))
    (and (plusp n)
         (or (char= (char name (1- n)) #\~)
             (and (> n 1) (char= (char name 0) #\#) (char= (char name (1- n)) #\#))
             (member (name-extension name) *dired-ignored-extensions* :test #'string-equal)))))
(defun dired-face (entry)
  "What kind of thing ENTRY is, as a name in *DIRED-FACES*, or NIL for a plain file."
  (let ((name (dentry-name entry)))
    (cond ((dentry-dir entry) :directory)
          ((dired-ignored-p name) :ignored)
          ((member (name-extension name) *dired-program-extensions* :test #'string-equal) :program)
          ((member (name-extension name) *dired-source-extensions* :test #'string-equal) :source))))
(defun dired-attributes (b i n)
  (flet ((face (name) (getf *dired-faces* name 7)))
    (if (= i 0)
        (fill *attrs* (face :header) :end n)
        (let ((entry (and (>= i 2) (nth (- i 2) (buf-entries b))))
              (name-column (min n *dired-name-column*)))
          (cond ((null entry))
                ((dentry-flag entry) (fill *attrs* (face :flagged) :end n))
                ((not *dired-colours*)  ; as it was before there were colours: only directories
                 (when (dentry-dir entry) (fill *attrs* 11 :start name-column :end n)))
                (t (let ((kind (dired-face entry)))
                     (fill *attrs* (face :details) :end name-column)
                     (case kind
                       ((nil))
                       ;; an ignored file is dimmed from end to end
                       (:ignored (fill *attrs* (face :ignored) :end n))
                       (t (fill *attrs* (face kind) :start name-column :end n))))))))))
(define-command dired-toggle-colours ()
  "Turn Dired's colouring of files by their kind on or off."
  (setf *dired-colours* (not *dired-colours*))
  (message "Dired colours are ~:[off~;on~]" *dired-colours*))
(defun dired-change-directory (dir &optional keep)
  (unless (directory-name-p dir) (editor-error "~A is not a directory" dir))
  (setf (buf-dir *buf*) dir (buf-name *buf*) dir (buf-line *buf*) 2)
  (dired-read *buf* keep))
(defun check-done (ok why what name)   ; the two values of REMOVE-FILE and the like
  (unless ok (editor-error "Can't ~A ~A: ~A" what name why)))
(defun dired-target (prompt)            ; a name typed in, relative to this directory
  (let ((name (string-trim " " (read-string prompt))))
    (when (string= name "") (error 'quit-command))
    (if (or (find #\: name) (find #\\ name) (find #\/ name))
        name
        (concatenate 'string (buf-dir *buf*) name))))
(defun file-name-of (path)
  (subseq path (1+ (or (position-if (lambda (c) (find c "\\/:")) path :from-end t) -1))))

(define-command dired ()
  "Edit a directory: list its files, and open, copy, rename or delete them."
  (select-buffer (dired-buffer (directory-string
                                (read-string "Dired (directory): " :initial (cwd-directory)
                                                                   :complete #'complete-file)))))
(define-command dired-next-line () "Move to the next file."
  (goto (max 2 (1+ (point-line))) *dired-name-column*))
(define-command dired-previous-line () "Move to the previous file."
  (goto (max 2 (1- (point-line))) *dired-name-column*))
(define-command dired-find-file ()
  "Open the file on this line, or go into the directory."
  (let ((entry (dired-entry)))
    (cond ((string= (dentry-name entry) "..") (dired-up-directory))
          ((dentry-dir entry)
           (dired-change-directory (concatenate 'string (dired-path entry) "\\")))
          (t (select-buffer (find-file-buffer (dired-path entry)))))))
;;; View mode, as in GNU Emacs: a file looked at and not changed.  The
;;; buffer is read-only and a few keys that would otherwise type are for
;;; reading: SPC and Backspace page, q puts things back as they were -- the
;;; buffer goes away again if viewing is what made it -- and e makes it an
;;; ordinary buffer that can be edited.
(defun view-buffer (b)
  "Show buffer B in View mode: to be read, not changed."
  (let ((from *buf*))
    (unless (buf-view b)
      (setf (buf-view b) (list nil (and (not (eq from b)) from) (buf-read-only b))
            (buf-read-only b) t))
    (select-buffer b)
    (message "View mode: SPC and Backspace page, q leaves, e edits")))
(defun view-file-named (path)
  (let* ((old (copy-list *buffers*))
         (b (find-file-buffer path)))
    (view-buffer b)
    ;; (a buffer that viewing made is killed by q; one that was there already stays)
    (unless (member b old) (setf (first (buf-view b)) t))))
(define-command view-file ()
  "Look at a file without being able to change it (View mode: q leaves)."
  (let ((name (read-string "View file: " :complete #'complete-file)))
    (if (directory-name-p name) (visit name) (view-file-named name))))
(define-command view-exit ()
  "Leave View mode and keep the buffer, which can now be edited."
  (let ((view (buf-view *buf*)))
    (unless view (editor-error "This buffer is not in View mode"))
    (setf (buf-read-only *buf*) (third view) (buf-view *buf*) nil)
    (message "~A can be edited" (buf-name *buf*))))
(define-command view-quit ()
  "Leave View mode and go back to where the file was viewed from."
  (let ((view (buf-view *buf*)) (b *buf*))
    (unless view (editor-error "This buffer is not in View mode"))
    (destructuring-bind (made from was-read-only) view
      (setf (buf-read-only b) was-read-only (buf-view b) nil)
      (if made
          (kill-buffer)
          (setf *message* nil))
      (when (and from (member from *buffers*)) (select-buffer from)))))
(define-command toggle-read-only ()
  "Make this buffer read-only, or let it be changed again."
  (when (member (buf-mode *buf*) '(dired-mode inspector-mode))
    (editor-error "~A is always read-only" (buf-name *buf*)))
  (setf (buf-read-only *buf*) (not (buf-read-only *buf*)) (buf-view *buf*) nil)
  (message "~A ~:[can be edited~;is read-only~]" (buf-name *buf*) (buf-read-only *buf*)))
(defun view-key (keys)
  "Do what the key KEYS means in View mode; false if it means nothing special there."
  (let ((command (cdr (assoc keys '(("SPC" . scroll-up) ("<backspace>" . scroll-down)
                                    ("q" . view-quit) ("e" . view-exit)
                                    ("<" . beginning-of-buffer) (">" . end-of-buffer))
                             :test #'equal))))
    (when command
      (setf *this-command* command)
      (funcall command)
      t)))
(define-command dired-view-file ()
  "Look at the file on this line in View mode (q comes back); a directory is gone into."
  (let ((entry (dired-entry)))
    (if (or (dentry-dir entry) (string= (dentry-name entry) ".."))
        (dired-find-file)
        (view-file-named (dired-path entry)))))
(define-command dired-find-file-other-window ()
  "Open the file on this line in another window."
  (let ((path (dired-path (dired-real-entry))) (directory (dentry-dir (dired-entry))))
    (when (null (rest *windows*)) (split-window))
    (other-window)
    (visit (if directory (concatenate 'string path "\\") path))))
(define-command dired-up-directory () "Go to the directory that contains this one."
  (let* ((dir (buf-dir *buf*)) (parent (parent-directory dir)))
    (when (string= parent dir) (editor-error "~A is the top" dir))
    (dired-change-directory parent (file-name-of (string-right-trim "\\" dir)))))
(define-command dired-refresh () "Read the directory again."
  (dired-read *buf* (ignore-errors (dentry-name (dired-entry))))
  (message "Read ~A" (buf-dir *buf*)))
(define-command dired-flag-deletion () "Flag this file for deletion (x deletes the flagged files)."
  (setf (dentry-flag (dired-real-entry)) t)
  (dired-render *buf*)
  (dired-next-line))
(define-command dired-unflag () "Take the deletion flag off this file."
  (setf (dentry-flag (dired-entry)) nil)
  (dired-render *buf*)
  (dired-next-line))
(define-command dired-unflag-all () "Take the deletion flag off every file."
  (dolist (entry (buf-entries *buf*)) (setf (dentry-flag entry) nil))
  (dired-render *buf*))
(defun dired-delete-entry (entry)
  (let ((path (dired-path entry)))
    (multiple-value-bind (ok why) (remove-file path (dentry-dir entry))
      (check-done ok why "delete" (dentry-name entry)))))
(define-command dired-execute ()
  "Delete the files flagged for deletion."
  (let ((flagged (remove-if-not #'dentry-flag (buf-entries *buf*))))
    (unless flagged (editor-error "No files are flagged (d flags one)"))
    (unless (ask-y-or-n (format nil "Delete ~D file~:P (~{~A~^, ~})?" (length flagged)
                                (mapcar #'dentry-name flagged)))
      (error 'quit-command))
    (unwind-protect (mapc #'dired-delete-entry flagged)
      (dired-read *buf*))
    (message "Deleted ~D file~:P" (length flagged))))
(define-command dired-delete ()
  "Delete the file on this line (an empty directory too)."
  (let ((entry (dired-real-entry)))
    (unless (ask-y-or-n (format nil "Delete ~A?" (dentry-name entry))) (error 'quit-command))
    (dired-delete-entry entry)
    (dired-read *buf*)
    (message "Deleted ~A" (dentry-name entry))))
(define-command dired-rename ()
  "Rename the file on this line, or move it by giving a path."
  (let* ((entry (dired-real-entry))
         (target (dired-target (format nil "Rename ~A to: " (dentry-name entry)))))
    (multiple-value-bind (ok why) (move-file (dired-path entry) target (dentry-dir entry))
      (check-done ok why "rename" (dentry-name entry)))
    (dolist (b *buffers*)               ; a buffer visiting it follows it
      (when (and (buf-file b) (string-equal (buf-file b) (dired-path entry)))
        (setf (buf-file b) target (buf-name b) (string-upcase (file-name-of target)))))
    (dired-read *buf* (file-name-of target))
    (message "Renamed ~A to ~A" (dentry-name entry) target)))
(define-command dired-copy ()
  "Copy the file on this line."
  (let* ((entry (dired-real-entry))
         (target (dired-target (format nil "Copy ~A to: " (dentry-name entry))))
         (buffer (make-array 32768 :element-type '(unsigned-byte 8))) (bytes 0))
    (when (dentry-dir entry) (editor-error "~A is a directory" (dentry-name entry)))
    (when (and (probe-file target) (not (ask-y-or-n (format nil "~A exists; overwrite?" target))))
      (error 'quit-command))
    (with-open-file (in (dired-path entry) :element-type '(unsigned-byte 8))
      (with-open-file (out target :direction :output :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
        (loop for n = (read-sequence buffer in)
              until (zerop n)
              do (write-sequence buffer out :end n) (incf bytes n))))
    (dired-read *buf* (dentry-name entry))
    (message "Copied ~:D byte~:P to ~A" bytes target)))
(define-command dired-create-directory ()
  "Make a new directory."
  (let ((target (dired-target "Create directory: ")))
    (multiple-value-bind (ok why) (create-directory target)
      (check-done ok why "create" target))
    (dired-read *buf* (file-name-of target))
    (message "Created ~A" target)))
(define-command dired-load-file ()
  "Load the file on this line into Lisp: source, or a compiled fasl."
  (let ((entry (dired-real-entry)))
    (when (dentry-dir entry) (editor-error "~A is a directory" (dentry-name entry)))
    (eval-and-show (format nil "(load ~S)" (dired-path entry)))))
(define-command dired-sort ()
  "Change the order of the listing: by name, by date (newest first), by size (largest first)."
  (setf (buf-sort *buf*) (ecase (buf-sort *buf*) (:name :date) (:date :size) (:size :name)))
  (dired-read *buf* (ignore-errors (dentry-name (dired-entry)))))
(define-command dired-quit () "Leave Dired." (kill-buffer))
(define-command dired-help () "Say what the Dired keys are."
  (message "RET open  v view  o other window  ^ up  d flag  u unflag  x delete flagged  D delete  R rename  C copy  L load  + mkdir  s sort  g reread  q quit"))
