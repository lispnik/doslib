;;;; Keymaps
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defvar *global-map* (make-hash-table :test 'equal))
(defun mode-map (mode)
  (or (get mode 'keymap) (setf (get mode 'keymap) (make-hash-table :test 'equal))))
(defun split-keys (string)
  (loop with start = 0
        for space = (position #\Space string :start start)
        collect (subseq string start space)
        while space do (setf start (1+ space))))
(defun bind-key (keys command &optional (map *global-map*))
  "Make the key sequence KEYS (\"C-x C-f\", \"M-q\", \"<f5>\") run COMMAND.
MAP may be a keymap or the name of a mode, as in (bind-key \"C-c t\" 'foo 'lisp-mode)."
  (when (symbolp map) (setf map (mode-map map)))
  (let ((keys (split-keys keys)))
    (loop for key in (butlast keys)
          do (setf map (let ((next (gethash key map)))
                         (if (hash-table-p next)
                             next
                             (setf (gethash key map) (make-hash-table :test 'equal))))))
    (setf (gethash (car (last keys)) map) command)))
(defun read-command ()
  "Read keys until they name a command.  Returns it (or NIL) and the keys typed."
  (let ((maps (list (mode-map (buf-mode *buf*)) *global-map*)) (typed '()))
    (loop
      (let ((key (read-key)))
        (push key typed)
        (let ((found (remove nil (mapcar (lambda (map) (gethash key map)) maps)))
              (keys (format nil "~{~A~^ ~}" (reverse typed))))
          (cond ((null found) (return (values nil keys)))
                ((hash-table-p (first found))
                 (setf maps (remove-if-not #'hash-table-p found))
                 (draw-echo (concatenate 'string keys "-")))
                (t (return (values (first found) keys)))))))))
