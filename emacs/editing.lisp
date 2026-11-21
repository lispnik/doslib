;;;; Editing
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defun self-insert (key) (insert (if (equal key "SPC") " " key)))
(define-command newline () "Insert a line break." (insert (string #\Newline)))
(define-command tab-to-tab-stop () "Insert spaces up to the next multiple of 8 columns."
  (insert (make-string (- 8 (mod (visual-col (current-line) (point-col)) 8)) :initial-element #\Space)))
(define-command quoted-insert () "Insert the next key literally: C-q TAB inserts a tab character."
  (let ((key (read-raw-key)))
    (insert (cond ((equal key "TAB") (string #\Tab))
                  ((equal key "SPC") " ")
                  ((= (length key) 1) key)
                  (t (editor-error "~A is not a character" key))))))
(define-command open-line () "Insert a line break after point."
  (let ((line (point-line)) (col (point-col))) (newline) (goto line col)))
(define-command delete-char ()
  "Delete the character after point."
  (when (at-end-p) (editor-error "End of buffer"))
  (let ((line (point-line)) (col (point-col)))
    (if (< col (length (current-line)))
        (delete-between line col line (1+ col))
        (delete-between line col (1+ line) 0))))
(define-command delete-backward-char ()
  "Delete the character before point."
  (backward-char) (delete-char))
(defun kill-new (text &optional before)
  (if (and (member *last-command* '(kill-line kill-word backward-kill-word kill-region kill-sexp backward-kill-sexp)) *kill-ring*)
      (setf (first *kill-ring*) (if before
                                    (concatenate 'string text (first *kill-ring*))
                                    (concatenate 'string (first *kill-ring*) text)))
      (push text *kill-ring*)))
(defun kill-between (l1 c1 l2 c2 &optional before)
  (kill-new (delete-between l1 c1 l2 c2) before)
  (goto l1 c1))
(define-command kill-line ()
  "Kill to the end of the line; at the end of a line, kill the line break."
  (let ((line (point-line)) (col (point-col)))
    (cond ((< col (length (current-line))) (kill-between line col line (length (current-line))))
          ((< line (1- (nlines))) (kill-between line col (1+ line) 0))
          (t (editor-error "End of buffer")))))
(define-command kill-word () "Kill to the end of the next word."
  (let ((line (point-line)) (col (point-col)))
    (forward-word) (kill-between line col (point-line) (point-col))))
(define-command backward-kill-word () "Kill to the start of the previous word."
  (let ((line (point-line)) (col (point-col)))
    (backward-word) (kill-between (point-line) (point-col) line col t)))
(define-command set-mark () "Put the mark where point is."
  (setf (buf-mark *buf*) (cons (point-line) (point-col)) (buf-mark-active *buf*) t)
  (message "Mark set"))
(define-command exchange-point-and-mark () "Swap point and the mark."
  (let ((mark (buf-mark *buf*)))
    (unless mark (editor-error "The mark is not set"))
    (setf (buf-mark *buf*) (cons (point-line) (point-col)) (buf-mark-active *buf*) t)
    (goto (car mark) (cdr mark))))
(define-command mark-whole-buffer () "Put point at the start and the mark at the end."
  (end-of-buffer) (set-mark) (goto 0 0))
(define-command kill-region () "Kill the text between point and the mark."
  (multiple-value-bind (l1 c1 l2 c2) (region-bounds) (kill-between l1 c1 l2 c2)))
(define-command copy-region () "Save the text between point and the mark without killing it."
  (multiple-value-bind (l1 c1 l2 c2) (region-bounds)
    (push (text-between l1 c1 l2 c2) *kill-ring*)
    (setf (buf-mark-active *buf*) nil)
    (message "Region saved")))
(define-command yank () "Insert the most recently killed text."
  (unless *kill-ring* (editor-error "The kill ring is empty"))
  (setf (buf-mark *buf*) (cons (point-line) (point-col)))
  (insert (first *kill-ring*)))
(defun revert-changes (from)
  "Undo the newest group of changes on the list FROM (a place holding an undo
or redo list); returns the records of what that did, for the other list."
  (let ((done '()))
    (loop for entry = (pop (symbol-value from))
          while (and entry (not (eq entry :boundary)))
          do (ecase (first entry)
               (:insert (destructuring-bind (l1 c1 l2 c2) (rest entry)
                          (push (list :delete l1 c1 (delete-between l1 c1 l2 c2 :undo nil)) done)
                          (goto l1 c1)))
               (:delete (destructuring-bind (line col text) (rest entry)
                          (multiple-value-bind (l c) (insert-at line col text :undo nil)
                            (push (list :insert line col l c) done)
                            (goto l c))))))
    (nreverse done)))
(defvar *changes* nil)                  ; scratch place for REVERT-CHANGES
(define-command undo ()
  "Undo the last change; repeat to undo more."
  (let ((b *buf*))
    (loop while (eq (first (buf-undo b)) :boundary) do (pop (buf-undo b)))
    (unless (buf-undo b) (editor-error "No further undo information"))
    (setf *changes* (buf-undo b))
    (let ((done (revert-changes '*changes*)))
      (setf (buf-undo b) *changes*
            (buf-redo b) (append (reverse done) (list :boundary) (buf-redo b))))
    (message "Undo!")))
(define-command redo ()
  "Redo the change that the last undo took back."
  (let ((b *buf*))
    (unless (buf-redo b) (editor-error "Nothing to redo"))
    (setf *changes* (buf-redo b))
    (let ((done (revert-changes '*changes*)))
      (setf (buf-redo b) *changes*)
      (loop while (eq (first (buf-undo b)) :boundary) do (pop (buf-undo b)))
      (setf (buf-undo b) (append (reverse done) (list :boundary) (buf-undo b))))
    (message "Redo!")))
(define-command keyboard-quit () "Cancel." (error 'quit-command))
