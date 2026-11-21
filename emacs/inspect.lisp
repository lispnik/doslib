;;;; The inspector: a Lisp object, part by part
;;;;
;;;; C-c I asks for an expression (by default the one at point), evaluates
;;;; it and shows the value in the buffer *inspector*: what it is, and its
;;;; parts -- the elements of a list or a vector, the slots of a structure
;;;; or an instance, the entries of a hash table, a symbol's value, function
;;;; and property list.  There:
;;;;
;;;;   RET    inspect the part on this line
;;;;   l      go back to the object this one was reached from
;;;;   d      what DESCRIBE says about the object
;;;;   g      look at the object again (it may have changed)
;;;;   q      leave the inspector
;;;;
;;;; The parts are found by the function SBCL's own INSPECT uses.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defvar *inspector-object* nil "The object the inspector is showing.")
(defvar *inspector-parts* #() "The parts on show: a vector of their values.")
(defvar *inspector-history* '() "The objects that led here, newest first.")
(defvar *inspector-return* nil "The buffer the inspector was started from.")
(defconstant +inspector-first-line+ 3 "The buffer line the first part is on.")
(defconstant +inspector-most+ 500 "No more parts than this are listed.")
(setf (get 'inspector-mode 'mode-name) "Inspector")

(defun brief (object &optional (width 60))
  "OBJECT printed on one line, cut to WIDTH characters."
  (let ((text (or (ignore-errors
                   (let ((*print-length* 8) (*print-level* 3) (*print-pretty* nil)
                         (*print-readably* nil) (*print-circle* t) (*package* *repl-package*))
                     (one-line (prin1-to-string object))))
                  "(it can't be printed)")))
    (if (> (length text) width) (concatenate 'string (subseq text 0 (- width 3)) "...") text)))
(defun object-parts (object)
  "What OBJECT is, in a line, and its parts: a list of (label . value)."
  (multiple-value-bind (description named elements)
      (let ((parts (find-symbol "INSPECTED-PARTS" "SB-IMPL")))
        (if (and parts (fboundp parts))
            (funcall parts object)
            (values (format nil "The object is of type ~S." (type-of object)) nil nil)))
    (let ((elements (if (listp elements) elements (coerce elements 'list))))
      (values (one-line description)
              (loop for element in elements for i from 0 below +inspector-most+
                    collect (if (and named (consp element))
                                (cons (string-downcase (princ-to-string (car element))) (cdr element))
                                (cons (format nil "~D" i) element)))
              (length elements)))))
(defun inspector-show (object)
  "Fill the buffer *inspector* with OBJECT and its parts, and show it."
  (multiple-value-bind (description parts count) (object-parts object)
    (setf *inspector-object* object
          *inspector-parts* (map 'vector #'cdr parts))
    (show-in-help "*inspector*"
                  (format nil "~A~%~A~:[~2*~;  The first ~D of its ~D parts:~]~%~%~:{~18A ~A~%~}"
                          (brief object 78) description
                          (> count (length parts)) (length parts) count
                          (mapcar (lambda (part) (list (car part) (brief (cdr part)))) parts)))
    (setf (buf-mode *buf*) 'inspector-mode (buf-read-only *buf*) t)
    (when parts (goto +inspector-first-line+ 0))
    (message "RET inspects the part on this line, l goes back, d describes, q leaves")))
(defun inspect-object (object)
  "Open the inspector on OBJECT."
  (unless (eq (buf-mode *buf*) 'inspector-mode)
    (setf *inspector-return* *buf* *inspector-history* '()))
  (inspector-show object))
(define-command inspect-expression ()
  "Evaluate an expression -- by default the one at point -- and inspect its value."
  (let* ((default (or (ignore-errors
                       (let ((text (sexp-around-point))) (and (not (find #\Newline text)) text)))
                      ""))
         (text (read-string "Inspect the value of: " :initial default)))
    (in-buffer-package
      (multiple-value-bind (forms status) (read-forms text)
        (unless (and (eq status :ok) forms) (editor-error "That expression can't be read"))
        (multiple-value-bind (values output condition) (eval-captured (first forms))
          (declare (ignore output))
          (when condition (editor-error "~A: ~A" (type-of condition) condition))
          (inspect-object (first values)))))))
(define-command inspector-part ()
  "Inspect the part on this line."
  (let ((index (- (point-line) +inspector-first-line+)))
    (unless (< -1 index (length *inspector-parts*)) (editor-error "There is no part on this line"))
    (push *inspector-object* *inspector-history*)
    (inspector-show (aref *inspector-parts* index))))
(define-command inspector-back ()
  "Go back to the object this one was reached from."
  (unless *inspector-history* (editor-error "This is the object the inspector started with"))
  (inspector-show (pop *inspector-history*)))
(define-command inspector-refresh () "Look at the object again."
  (let ((line (point-line)))
    (inspector-show *inspector-object*)
    (goto line 0)))
(define-command inspector-describe () "Show what DESCRIBE says about the object."
  (let ((object *inspector-object*))
    (show-in-help "*help*" (with-output-to-string (out)
                             (let ((*package* *repl-package*)) (describe object out))))))
(define-command inspector-quit () "Leave the inspector."
  (let ((back *inspector-return*))
    (setf *inspector-object* nil *inspector-parts* #() *inspector-history* '() *inspector-return* nil)
    (kill-buffer)
    (when (member back *buffers*) (select-buffer back))))
(dolist (binding '(("RET" inspector-part) ("l" inspector-back) ("g" inspector-refresh)
                   ("d" inspector-describe) ("q" inspector-quit)
                   ("n" next-line) ("p" previous-line) ("SPC" next-line)))
  (bind-key (first binding) (second binding) 'inspector-mode))
(bind-key "C-c I" 'inspect-expression)
