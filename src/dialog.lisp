;;;; A restart dialog: Common Lisp's answer to "Abort, Retry, Fail?".
;;;;
;;;; (restart-dialog condition) shows the condition and the restarts that
;;;; are available in a box over whatever is on the text screen, lets the
;;;; user choose one by number, and invokes it.  The stack is still there
;;;; while the box is up, so with :EVAL-PACKAGE the user may also evaluate
;;;; Lisp forms first -- define a missing function, change a variable --
;;;; and then choose.  Call it from a HANDLER-BIND handler.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defun text-wrap (text width)
  "TEXT as a list of lines no longer than WIDTH, broken at spaces."
  (let ((lines '()) (line ""))
    (dolist (paragraph (loop with start = 0
                             for newline = (position #\Newline text :start start)
                             collect (subseq text start newline)
                             while newline do (setf start (1+ newline))))
      (dolist (word (loop with start = 0
                          for space = (position #\Space paragraph :start start)
                          collect (subseq paragraph start space)
                          while space do (setf start (1+ space))))
        (cond ((string= word ""))
              ((string= line "") (setf line word))
              ((<= (+ (length line) 1 (length word)) width)
               (setf line (concatenate 'string line " " word)))
              (t (push line lines) (setf line word)))
        (loop while (> (length line) width)       ; a word longer than a line
              do (push (subseq line 0 width) lines) (setf line (subseq line width))))
      (push line lines) (setf line ""))
    (or (nreverse (member "" lines :test-not #'string=)) (list ""))))

(defvar *dialog-line* nil "Where DIALOG-ASK asks: (x y width), while a dialog is up.")

(defun dialog-ask (prompt)
  "Ask for a line of text inside the dialog that is showing (or at the
bottom of the screen); for the :INTERACTIVE function of a restart.
Returns the string typed, or \"\" if the user pressed Esc."
  (destructuring-bind (x y width) (or *dialog-line* '(0 24 80))
    (text-put x y (format nil "~VA" width prompt) #x1E)
    (show-text-cursor)
    (prog1 (or (text-input (+ x (length prompt) 1) y (- width (length prompt) 1) :attribute #x1E) "")
      (hide-text-cursor)
      (text-put x y (make-string width :initial-element #\Space) #x1F))))

(defun restart-dialog (condition &key (restarts (compute-restarts condition)) through
                                      (title "Error") eval-package)
  "Show CONDITION and RESTARTS; invoke the one the user chooses.  THROUGH
names the last restart to offer: the program's outermost one, so that those
of whatever is running the program are left out.  With EVAL-PACKAGE, the
user may also type Lisp forms, evaluated in that package."
  (let* ((restarts (if through
                       (ldiff restarts (rest (member through restarts :key #'restart-name)))
                       restarts))
         (width 66) (x 7) (inner (- width 4))
         (message (text-wrap (princ-to-string condition) inner))
         (height (+ (length message) (length restarts) (if eval-package 1 0) 6))
         (y (max 0 (floor (- 25 height) 2)))
         (saved (save-text-screen))
         (row (+ y 1)))
    (unwind-protect
         (progn
           (hide-text-cursor)
           (text-box x y width height :attribute #x1F :title title :title-attribute #x4F :shadow t)
           (dolist (line message) (text-put (+ x 2) row line #x1F) (incf row))
           (incf row)
           (loop for restart in restarts for i from 1
                 do (text-put (+ x 2) row
                              (let ((line (format nil "~D  ~A" i restart)))
                                (subseq line 0 (min inner (length line))))
                              #x1E)
                    (incf row))
           (when eval-package
             (text-put (+ x 2) row "E  Evaluate a Lisp form first" #x1E) (incf row))
           (incf row)
           (let ((*dialog-line* (list (+ x 2) row inner))
                 (note-row (1+ row)))
             (flet ((note (text) (text-put (+ x 2) note-row
                                           (format nil "~VA" inner (subseq text 0 (min inner (length text))))
                                           #x1B)))
               (note "Press the number of your choice.")
               (loop
                 (let* ((key (get-key))
                        (digit (and (characterp key) (digit-char-p key))))
                   (cond ((and digit (<= 1 digit (length restarts)))
                          (invoke-restart-interactively (nth (1- digit) restarts)))
                         ((and eval-package (characterp key) (char-equal key #\e))
                          (let ((text (dialog-ask "Lisp:")))
                            (unless (string= (string-trim " " text) "")
                              (note (handler-case
                                        (let ((*package* (find-package eval-package))
                                              (*error-output* (make-broadcast-stream)))   ; keep the
                                          (handler-bind ((warning #'muffle-warning))     ; screen clean
                                            (format nil "=> ~S" (eval (read-from-string text)))))
                                      (error (e)
                                        (substitute #\Space #\Newline (format nil "~A" e))))))))))))))
      (restore-text-screen saved))))
