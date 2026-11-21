;;;; Tests of the editor's Lisp commands, run inside the editor on buffers
;;;; that are never shown.
;;;;
;;;; tests/run.py copies this file to D:\SBCL\ED.LSP; the test "emacs-lisp-edit"
;;;; loads it with C-c C-l, and it writes one line per test to ED.TXT.
;;;; By hand: start EMACS, C-c C-l, ED.LSP.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-tests.editor
  (:use #:cl #:doslib-emacs))
(in-package #:doslib-tests.editor)

(defvar *tests* '() "(name . function), in the order they were defined.")
(defmacro deftest (name &body body)
  `(progn (setf *tests* (append (remove ',name *tests* :key #'car)
                                (list (cons ',name (lambda () ,@body)))))
          ',name))
(defmacro is (form)
  `(unless ,form (error "~S is false" ',form)))
(defmacro is= (expected form)
  `(let ((value ,form))
     (unless (equal value ,expected) (error "~S is ~S, not ~S" ',form value ,expected))))

(defmacro with-text ((text &optional (line 0) (col 0) file) &body body)
  "Do BODY in a Lisp buffer that holds TEXT, with point at LINE, COL."
  `(let* ((*buffers* '()) (*kill-ring* '()) (*last-command* nil) (*message* nil)
          (*buf* (make-buf "test")))
     (insert-at 0 0 ,text)
     (setf (buf-mode *buf*) 'doslib-emacs::lisp-mode (buf-file *buf*) ,file)
     (goto ,line ,col)
     ,@body))
(defun at () (list (point-line) (point-col)))
(defun lines (&rest lines) (format nil "~{~A~^~%~}" lines))
(defun mark-at (line col)
  (setf (buf-mark *buf*) (cons line col) (buf-mark-active *buf*) t))

(deftest editor-sexp-motion
  ;;          0         1         2
  ;;          01234567890123456789012345678
  (with-text ("(defun f (x) \"a (b\" #\\( 'y) z")
    (forward-sexp) (is= '(0 27) (at))
    (forward-sexp) (is= '(0 29) (at))
    (backward-sexp) (is= '(0 28) (at))
    (backward-sexp) (is= '(0 0) (at))
    (goto 0 13)
    (forward-sexp) (is= '(0 19) (at))                   ; a string with a parenthesis in it
    (forward-sexp) (is= '(0 23) (at))                   ; the character #\(
    (forward-sexp) (is= '(0 26) (at))                   ; 'y
    (is (null (ignore-errors (forward-sexp) t)))        ; the end of the list
    (backward-sexp) (is= '(0 24) (at))
    (backward-sexp) (is= '(0 20) (at))
    (backward-sexp) (is= '(0 13) (at))
    (goto 0 10)
    (backward-up-list) (is= '(0 9) (at))
    (backward-up-list) (is= '(0 0) (at))
    (goto 0 10) (up-list) (is= '(0 12) (at))
    (goto 0 1) (down-list) (is= '(0 10) (at)))
  (with-text ((lines "(a ; (not this" "   b)" "" "(c)"))
    (forward-sexp) (is= '(1 5) (at))
    (backward-sexp) (is= '(0 0) (at))
    (goto 1 3) (backward-sexp) (is= '(0 1) (at))        ; back over the comment
    (goto 1 0)
    (end-of-defun) (is= '(2 0) (at))
    (end-of-defun) (is= '(3 3) (at))
    (beginning-of-defun) (is= '(3 0) (at))
    (beginning-of-defun) (is= '(0 0) (at)))
  (let ((*unread* (list "ESC" "C-f" "ESC" "f")))        ; ESC and a Ctrl key is C-M-
    (is= "C-M-f" (read-key))
    (is= "M-f" (read-key))))

(deftest editor-sexp-kill
  (with-text ("(a (b c) d)" 0 3)
    (kill-sexp)
    (is= "(a  d)" (buffer-string))
    (is= "(b c)" (first *kill-ring*))
    (yank)
    (is= "(a (b c) d)" (buffer-string))
    (goto 0 8) (transpose-sexps)
    (is= "(a d (b c))" (buffer-string))
    (is= '(0 10) (at)))
  (with-text ("(a b)" 0 4)
    (backward-kill-sexp)
    (is= "(a )" (buffer-string)))
  (with-text ("foo bar")
    (mark-sexp)
    (is= '(0 . 3) (buf-mark *buf*))))

(deftest editor-sexp-structure
  (with-text ("(a (b c) d)" 0 5)
    (splice-sexp)
    (is= "(a b c d)" (buffer-string))
    (is= '(0 4) (at)))
  (with-text ("(a (b c) d)" 0 6)
    (raise-sexp)
    (is= "(a c d)" (buffer-string))
    (is= '(0 3) (at)))
  (with-text ("(a (b c) d)" 0 4)
    (slurp-sexp) (is= "(a (b c d))" (buffer-string))
    (barf-sexp) (is= "(a (b c) d)" (buffer-string))
    (barf-sexp) (is= "(a (b) c d)" (buffer-string))
    (barf-sexp) (is= "(a () b c d)" (buffer-string))
    (is= '(0 4) (at)))
  (with-text ((lines "(a   ; first" " b)"))
    (goto 1 1) (barf-sexp)
    (is= (lines "(a)   ; first" "b") (buffer-string)))
  (with-text ("")
    (insert-parentheses)
    (is= "()" (buffer-string))
    (is= '(0 1) (at)))
  (with-text ((lines "  (a)" "    (b)"))
    (mark-at 1 7) (comment-dwim)
    (is= (lines "  ;; (a)" "  ;;   (b)") (buffer-string))
    (mark-at 1 7) (comment-dwim)
    (is= (lines "  (a)" "    (b)") (buffer-string)))
  (with-text ("(a)")
    (comment-dwim)
    (is= "(a)  ; " (buffer-string))
    (goto 0 0) (comment-dwim)                           ; there is one: go to it
    (is= '(0 7) (at))))

(deftest editor-indent
  (let ((messy (lines "(defun f (x)" " (if x" "1" "         2))"))
        (neat (lines "(defun f (x)" "  (if x" "      1" "      2))")))
    (with-text (messy)
      (indent-sexp)
      (is= neat (buffer-string)))
    (with-text (messy 2 1)
      (indent-defun)
      (is= neat (buffer-string))
      (is= '(2 7) (at)))                                ; point stayed on its text
    (with-text (messy)
      (mark-at 3 5) (indent-region)
      (is= neat (buffer-string)))))

(deftest editor-lisp-tools
  (is (subsetp '("mapcar" "mapcan") (symbol-completions "mapc") :test #'string=))
  (is= '("cl:mapcar") (symbol-completions "cl:mapcar"))
  (is (member "doslib:" (symbol-completions "dosl") :test #'string=))
  (is (member "doslib-emacs::reindent-line" (symbol-completions "doslib-emacs::reindent-l")
              :test #'string=))
  (is (eql 0 (search "(mapcar function" (arglist-string "mapcar"))))
  (is (null (arglist-string "if")))
  (is (eq 'if (first (doslib-emacs::expand-all '(when a b)))))
  (with-text ("(multiple-value-b" 0 17)
    (complete-symbol)
    (is= "(multiple-value-bind" (buffer-string)))
  (with-text ("(mapcar" 0 7)
    (lisp-space)
    (is= "(mapcar " (buffer-string))
    (is (search "(mapcar function" *message*)))
  (with-text ("(a (b c))" 0 3)
    (is= "(b c)" (sexp-around-point))
    (goto 0 9)
    (is= "(a (b c))" (sexp-around-point)))
  (with-text ("(+ 1 2)" 0 7)
    (eval-print-last-sexp)
    (is= (lines "(+ 1 2)" "3" "") (buffer-string)))
  ;; a file's code is read in the file's package; a buffer without a file, in the REPL's
  (with-text ((lines "(in-package #:doslib)" "(foo)") 1 0 "x.lisp")
    (is (eq (find-package "DOSLIB") (current-package))))
  (with-text ((lines "(in-package #:doslib)" "(foo)") 1 0)
    (is (eq *repl-package* (current-package)))))

(deftest editor-colours
  ;; a string or a #| |# comment that runs on over the lines
  (is= :string (lisp-attributes "(defun f () \"one"))
  (fill *attrs* 7)
  (is (null (lisp-attributes "two\" (x)" :string)))
  (is= 14 (aref *attrs* 3))
  (is= 7 (aref *attrs* 5))
  (is= 1 (lisp-attributes "(a) #| a comment"))
  (is= 1 (lisp-attributes "more of it" 1))
  (is (null (lisp-attributes "the end |# (x)" 1)))
  (with-text ((lines "(defun f ()" "  \"doc" "   more\"" "  1)"))
    (is= :string (line-start-state 2))
    (is (null (line-start-state 3)))))

(deftest editor-lexing
  ;; a parenthesis in the first column of a line inside a string starts nothing
  (with-text ((lines "(defun f ()" "  \"A docstring:" "(not a form) here\"" "  (g 1))" "(defun h () 2)") 3 4)
    (is (not (top-level-line-p 2)))
    (is (top-level-line-p 4))
    (beginning-of-defun) (is= '(0 0) (at))
    (is= '(0 . 0) (find-open-paren 3 8))
    (is= '(("f" . 0) ("h" . 4)) (buffer-definitions))
    (goto 2 3) (end-of-defun) (is= '(4 0) (at))
    ;; and a change above is noticed: without its opening quote, that line is code
    (delete-between 1 2 1 3)
    (is (top-level-line-p 2))
    (insert-at 1 2 "\"")
    (is (not (top-level-line-p 2))))
  ;; a string that is never closed: the forms below it are still forms
  (with-text ((lines "(defun f ()" "  \"oops" "  1)" "(defun g () 2)") 3 13)
    (is (top-level-line-p 3))
    (is (null (line-start-state 3)))
    (is= :string (line-start-state 2))
    (is= '(3 . 0) (find-open-paren 3 14))
    (is= '(3 . 9) (find-open-paren 3 11)))
  (with-text ((lines "#| a comment" "(not code)" "|#" "(code)"))
    (is (not (top-level-line-p 1)))
    (is (top-level-line-p 3)))
  (is (null (lex-line "(a |b \" c| d)" nil)))
  (is= :string (lex-line "(a \"b" nil))
  (is (null (lex-line "x \\\" y" nil)))
  (is= 2 (lex-line "#| a #| b" nil)))

(deftest editor-bar-symbols
  ;; |a symbol| is one expression, whatever is in it
  (with-text ("(a |b (c| d) e" 0 3)
    (forward-sexp) (is= '(0 9) (at))
    (backward-sexp) (is= '(0 3) (at))
    (is= '(0 . 0) (find-open-paren 0 12))
    (goto 0 0) (forward-sexp) (is= '(0 12) (at))
    (goto 0 3) (kill-sexp) (is= "(a  d) e" (buffer-string)))
  (with-text ("(x foo\\ bar y)" 0 3)
    (forward-sexp) (is= '(0 11) (at))
    (backward-sexp) (is= '(0 3) (at)))
  (with-text ("(x a\\(b y)" 0 7)
    (backward-sexp) (is= '(0 3) (at)))
  (with-text ("(f #\\\\)" 0 7)
    (backward-sexp) (is= '(0 0) (at))))

(deftest editor-check-parens
  (with-text ("(a (b)")
    (is (null (ignore-errors (check-parens) t)))
    (is= '(0 0) (at)))
  (with-text ("(a) b)")
    (is (null (ignore-errors (check-parens) t)))
    (is= '(0 5) (at)))
  (with-text ("(a \")\" #\\( b)")
    (check-parens)
    (is (search "balance" *message*))))

(deftest editor-electric
  (with-text ("")
    (electric-open) (is= "()" (buffer-string)) (is= '(0 1) (at))
    (electric-quote) (is= "(\"\")" (buffer-string)) (is= '(0 2) (at))
    (insert "a(")                                       ; in a string the keys are themselves
    (electric-open) (is= "(\"a((\")" (buffer-string))
    (electric-quote) (is= '(0 6) (at))                  ; over the quote that was there
    (electric-close) (is= "(\"a((\")" (buffer-string)) (is= '(0 7) (at)))
  (with-text ("()" 0 1)
    (electric-backspace)
    (is= "" (buffer-string))))

(deftest editor-definitions
  (with-text ((lines "(defun alpha (x) x)" "" "(defvar *beta* 1)" "(defstruct (gamma (:type list)) a)"))
    (is= '(("alpha" . 0) ("*beta*" . 2) ("gamma" . 3)) (buffer-definitions)))
  (is= "(defparameter *x* 2)" (doslib-emacs::defvar-again "(defvar *x* 2)"))
  (is= "(defvar *x*)" (doslib-emacs::defvar-again "(defvar *x*)")))

(deftest editor-inspector
  (multiple-value-bind (description parts count) (object-parts (list 1 "two"))
    (is (search "list" description))
    (is= 2 count)
    (is= '(("0" . 1) ("1" . "two")) parts))
  (is= "\"abc\"" (brief "abc"))
  (is= 10 (length (brief (make-string 50 :initial-element #\a) 10))))

(defun run-tests (&optional (file "ED.TXT"))
  "Run the tests; one line for each goes to FILE.  Returns how many failed."
  (let ((failed 0))
    (with-open-file (log file :direction :output :if-exists :supersede)
      (dolist (test *tests*)
        (let ((problem (handler-case (progn (funcall (cdr test)) nil)
                         (error (e) (substitute #\Space #\Newline (princ-to-string e))))))
          (when problem (incf failed))
          (format log "~:[ok~;FAIL~] ~(~A~)~@[: ~A~]~%" problem (car test) problem))))
    failed))
(run-tests)
