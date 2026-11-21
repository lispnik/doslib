;;;; Lisp by the expression: moving, killing, indenting, restructuring
;;;;
;;;; The commands here treat the text as Lisp expressions, not characters:
;;;; C-M-f and C-M-b move over one, C-M-k kills one, C-M-u and C-M-d go up
;;;; and down the levels of parentheses, C-M-a and C-M-e go to the ends of
;;;; the top-level form.  M-s, M-r, C-c ) and C-c } change the structure --
;;;; take a pair of parentheses away, raise an expression in place of its
;;;; list, move a closing parenthesis out or in -- and leave it balanced.
;;;;
;;;; (C-M- is Ctrl and Alt together; ESC and then the Ctrl key does as well.)
;;;;
;;;; A position is a line and a column, two values.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defun before-p (l1 c1 l2 c2) (or (< l1 l2) (and (= l1 l2) (< c1 c2))))
(defun code-end (line)
  "The column where the code of line LINE ends: where its comment starts, or its length."
  (let* ((text (line-text line)) (n (length text)) (in-string nil) (c 0))
    (loop while (< c n)
          do (let ((ch (char text c)))
               (cond (in-string (cond ((char= ch #\\) (incf c))
                                      ((char= ch #\") (setf in-string nil))))
                     ((char= ch #\") (setf in-string t))
                     ((and (char= ch #\#) (< (1+ c) n) (char= (char text (1+ c)) #\\)) (incf c 2))
                     ((char= ch #\;) (return-from code-end c)))
               (incf c)))
    n))
(defun skip-blank-forward (l c)
  "The next position at or after L, C that is not white space or a comment; NIL at the end."
  (loop
    (let* ((text (line-text l)) (n (length text)))
      (loop while (and (< c n) (whitespace-p (char text c))) do (incf c))
      (cond ((and (< (1+ c) n) (char= (char text c) #\#) (char= (char text (1+ c)) #\|))
             (let ((from (+ c 2)))      ; a block comment: on to its end
               (loop
                 (let ((end (search "|#" (line-text l) :start2 from)))
                   (when end (setf c (+ end 2)) (return))
                   (when (>= l (1- (nlines))) (return-from skip-blank-forward nil))
                   (setf l (1+ l) from 0)))))
            ((and (< c n) (char/= (char text c) #\;)) (return (values l c)))
            ((< l (1- (nlines))) (setf l (1+ l) c 0))
            (t (return nil))))))
(defun skip-blank-backward (l c)
  "The last position at or before L, C that follows something other than white
space or a comment; NIL at the start."
  (loop
    (let ((text (line-text l)))
      (loop while (and (> c 0) (whitespace-p (char text (1- c)))) do (decf c))
      (cond ((> c 0) (return (values l c)))
            ((> l 0) (setf l (1- l) c (code-end l)))
            (t (return nil))))))
(defun string-end (l c)                 ; C is at the opening quote
  (incf c)
  (loop
    (let ((text (line-text l)))
      (loop while (< c (length text))
            do (cond ((char= (char text c) #\\) (incf c 2))
                     ((char= (char text c) #\") (return-from string-end (values l (1+ c))))
                     (t (incf c))))
      (when (>= l (1- (nlines))) (editor-error "The string does not end"))
      (setf l (1+ l) c 0))))
(defun string-start (l c)               ; C is at the closing quote
  (loop
    (loop while (> c 0)
          do (decf c)
             (when (and (char= (char-at l c) #\")
                        (not (and (> c 0) (char= (char-at l (1- c)) #\\))))
               (return-from string-start (values l c))))
    (when (= l 0) (editor-error "The string does not begin"))
    (setf l (1- l) c (length (line-text l)))))
(defun sexp-end (l c)
  "Where the expression that starts at or after L, C ends."
  (multiple-value-setq (l c) (skip-blank-forward l c))
  (unless l (editor-error "No expression after point"))
  (let* ((text (line-text l)) (n (length text)))
    (loop                               ; ' ` , ,@ #' #. belong to what follows them
      (cond ((>= c n) (return))
            ((find (char text c) "'`,@") (incf c))
            ((and (char= (char text c) #\#) (< (1+ c) n) (find (char text (1+ c)) "'.")) (incf c 2))
            ((and (char= (char text c) #\#) (< (1+ c) n) (char= (char text (1+ c)) #\()) (incf c) (return))
            (t (return))))
    (when (>= c n) (return-from sexp-end (sexp-end l c)))       ; a quote ending a line
    (let ((ch (char text c)))
      (cond ((char= ch #\()
             (let ((end (or (find-close-paren l c) (editor-error "Unbalanced parentheses"))))
               (values (car end) (cdr end))))
            ((char= ch #\)) (editor-error "This is the end of the list"))
            ((char= ch #\") (string-end l c))
            ((and (char= ch #\#) (< (1+ c) n) (char= (char text (1+ c)) #\\))
             (values l (or (position-if #'delimiter-p text :start (min n (+ c 3))) n)))
            (t (values l (or (position-if #'delimiter-p text :start (min n (1+ c))) n)))))))
(defun sexp-start (l c)
  "Where the expression that ends at or before L, C starts."
  (multiple-value-setq (l c) (skip-blank-backward l c))
  (unless l (editor-error "No expression before point"))
  (let ((ch (char-at l (1- c))))
    (cond ((and (delimiter-p ch) (escaped-p l (1- c))) (decf c 3))      ; #\( and the like
          ((char= ch #\))
           (let ((open (or (find-open-paren l c) (editor-error "Unbalanced parentheses"))))
             (setf l (car open) c (cdr open))))
          ((char= ch #\() (editor-error "This is the start of the list"))
          ((char= ch #\") (multiple-value-setq (l c) (string-start l (1- c))))
          (t (loop while (and (> c 0) (not (delimiter-p (char-at l (1- c))))) do (decf c))))
    (loop while (and (> c 0) (find (char-at l (1- c)) "'`,@#")) do (decf c))
    (values l c)))
(defun enclosing-open (&optional (l (point-line)) (c (point-col)))
  "Where the parenthesis that opens the list around L, C is: (line . column), or NIL."
  (let ((stack '()))
    (scan-parens (safe-start l) l c
                 (lambda (open pl pc) (if open (push (cons pl pc) stack) (pop stack))))
    (first stack)))
(defun enclosing-list ()
  "The list around point: the line and column of its opening parenthesis, and of its closing one."
  (let* ((open (or (enclosing-open) (editor-error "Point is not inside a list")))
         (close (or (find-close-paren (car open) (cdr open)) (editor-error "Unbalanced parentheses"))))
    (values (car open) (cdr open) (car close) (1- (cdr close)))))
(defun top-level-line-p (i)
  (let ((text (line-text i))) (and (plusp (length text)) (char= (char text 0) #\())))
(defun defun-bounds ()
  "The top-level form around point, or the next one: its first line, and the
line and column where it ends."
  (let* ((line (point-line))
         (start (loop for i from line downto 0 when (top-level-line-p i) return i))
         (end (and start (find-close-paren start 0))))
    (when (or (null end) (before-p (car end) (cdr end) line (point-col)))
      (setf start (loop for i from (1+ line) below (nlines) when (top-level-line-p i) return i)
            end (and start (find-close-paren start 0))))
    (unless end (editor-error "No top-level form here"))
    (values start (car end) (cdr end))))

;;; ---- moving

(define-command forward-sexp () "Move over the expression after point."
  (multiple-value-bind (l c) (sexp-end (point-line) (point-col)) (goto l c)))
(define-command backward-sexp () "Move back over the expression before point."
  (multiple-value-bind (l c) (sexp-start (point-line) (point-col)) (goto l c)))
(define-command backward-up-list () "Move out of this list, to the parenthesis that opens it."
  (let ((open (or (enclosing-open) (editor-error "Point is not inside a list"))))
    (goto (car open) (cdr open))))
(define-command up-list () "Move out of this list, past the parenthesis that closes it."
  (multiple-value-bind (ol oc cl cc) (enclosing-list)
    (declare (ignore ol oc))
    (goto cl (1+ cc))))
(define-command down-list () "Move into the next list, past the parenthesis that opens it."
  (let ((line (point-line)) (col (point-col)))
    (scan-parens (safe-start line) (min (1- (nlines)) (+ line 500)) most-positive-fixnum
                 (lambda (open l c)
                   (unless (before-p l c line col)
                     (unless open (editor-error "No list to go down into"))
                     (goto l (1+ c))
                     (return-from down-list))))
    (editor-error "No list to go down into")))
(define-command beginning-of-defun () "Move to the start of this top-level form, or of the one before."
  (let ((line (loop for i from (if (zerop (point-col)) (1- (point-line)) (point-line)) downto 0
                    when (top-level-line-p i) return i)))
    (goto (or line 0) 0)))
(define-command end-of-defun () "Move to the end of this top-level form, or of the next."
  (multiple-value-bind (start l c) (defun-bounds)
    (declare (ignore start))
    (if (< l (1- (nlines))) (goto (1+ l) 0) (goto l c))))

;;; ---- killing and marking

(define-command kill-sexp () "Kill the expression after point."
  (let ((line (point-line)) (col (point-col)))
    (multiple-value-bind (l c) (sexp-end line col) (kill-between line col l c))))
(define-command backward-kill-sexp () "Kill the expression before point."
  (let ((line (point-line)) (col (point-col)))
    (multiple-value-bind (l c) (sexp-start line col) (kill-between l c line col t))))
(define-command mark-sexp () "Put the mark after the expression that follows point."
  (multiple-value-bind (l c) (sexp-end (point-line) (point-col))
    (setf (buf-mark *buf*) (cons l c) (buf-mark-active *buf*) t)))
(define-command mark-defun () "Put point at the start of this top-level form and the mark after it."
  (multiple-value-bind (start l c) (defun-bounds)
    (setf (buf-mark *buf*) (if (< l (1- (nlines))) (cons (1+ l) 0) (cons l c))
          (buf-mark-active *buf*) t)
    (goto start 0)))
(define-command transpose-sexps () "Swap the expressions before and after point."
  (let ((line (point-line)) (col (point-col)))
    (multiple-value-bind (l1 c1) (sexp-start line col)
      (multiple-value-bind (l2 c2) (sexp-end l1 c1)
        (multiple-value-bind (l4 c4) (sexp-end line col)
          (multiple-value-bind (l3 c3) (sexp-start l4 c4)
            (when (before-p l3 c3 l2 c2) (editor-error "There are not two expressions to swap here"))
            (let ((first (text-between l1 c1 l2 c2)) (gap (text-between l2 c2 l3 c3))
                  (second (text-between l3 c3 l4 c4)))
              (delete-between l1 c1 l4 c4)
              (goto l1 c1)
              (insert (concatenate 'string second gap first)))))))))

;;; ---- indenting

(defun reindent-line (line)
  "Indent line LINE as Lisp (a blank one loses its spaces); returns how far its text moved."
  (let* ((text (line-text line))
         (old (or (position-if-not #'whitespace-p text) (length text)))
         (new (if (= old (length text)) 0 (lisp-indentation line))))
    (cond ((or (null new) (= new old)) 0)
          (t (when (plusp old) (delete-between line 0 line old))
             (when (plusp new) (insert-at line 0 (make-string new :initial-element #\Space)))
             (- new old)))))
(defun reindent-lines (from to)
  "Indent lines FROM to TO; point stays on the text it was on.  Returns how many changed."
  (let ((line (point-line)) (col (point-col)) (changed 0))
    (loop for i from (max 0 from) to (min to (1- (nlines)))
          do (let ((moved (reindent-line i)))
               (unless (zerop moved) (incf changed))
               (when (= i line) (setf col (max 0 (+ col moved))))))
    (goto line col)
    changed))
(define-command indent-sexp () "Indent every line of the expression after point."
  (let ((line (point-line)))
    (multiple-value-bind (l c) (sexp-end line (point-col))
      (declare (ignore c))
      (message "~D line~:P changed" (reindent-lines (1+ line) l)))))
(define-command indent-defun () "Indent every line of the top-level form around point."
  (multiple-value-bind (start l c) (defun-bounds)
    (declare (ignore c))
    (message "~D line~:P changed" (reindent-lines (1+ start) l))))
(defun region-lines ()
  "The first and last line of the region; a line the region only touches the start of is not in it."
  (multiple-value-bind (l1 c1 l2 c2) (region-bounds)
    (declare (ignore c1))
    (values l1 (if (and (> l2 l1) (zerop c2)) (1- l2) l2))))
(define-command indent-region () "Indent every line of the region as Lisp."
  (multiple-value-bind (first last) (region-lines)
    (message "~D line~:P changed" (reindent-lines first last))))

;;; ---- comments

(defun comment-lines (first last)
  "Comment out lines FIRST to LAST; if they all are already, take the comments off."
  (flet ((indent (i) (position-if-not #'whitespace-p (line-text i))))
    (let* ((lines (loop for i from first to last when (indent i) collect i))
           (commented (and lines (every (lambda (i) (char= (char (line-text i) (indent i)) #\;)) lines)))
           (column (and lines (reduce #'min (mapcar #'indent lines)))))
      (dolist (i lines)
        (if commented
            (let* ((text (line-text i)) (at (indent i))
                   (end (or (position-if-not (lambda (ch) (char= ch #\;)) text :start at) (length text))))
              (when (and (< end (length text)) (char= (char text end) #\Space)) (incf end))
              (delete-between i at i end))
            (insert-at i column ";; ")))
      (goto (point-line) (point-col))
      (message "~D line~:P ~:[commented out~;no longer comments~]" (length lines) commented))))
(define-command comment-dwim ()
  "With a region: comment its lines out, or take their comments off.  Without:
go to this line's comment, or start one."
  (if (buf-mark-active *buf*)
      (multiple-value-bind (first last) (region-lines) (comment-lines first last))
      (let* ((line (point-line)) (text (current-line)) (at (code-end line)))
        (cond ((< at (length text))
               (goto line (or (position-if-not (lambda (ch) (find ch "; ")) text :start at)
                              (length text))))
              ((every #'whitespace-p text) (indent-line) (insert ";; "))
              (t (goto line (length text)) (insert "  ; "))))))

;;; ---- changing the structure

(defun edit-insert (l c string)         ; (STRING has no line break) point stays on its text
  (insert-at l c string)
  (when (and (= (point-line) l) (> (point-col) c)) (goto l (+ (point-col) (length string)))))
(defun edit-delete (l c n)
  (delete-between l c l (+ c n))
  (when (and (= (point-line) l) (> (point-col) c)) (goto l (max c (- (point-col) n)))))
(define-command insert-parentheses ()
  "Insert a pair of parentheses and put point between them; round the region, if there is one."
  (if (buf-mark-active *buf*)
      (multiple-value-bind (l1 c1 l2 c2) (region-bounds)
        (insert-at l2 c2 ")")
        (insert-at l1 c1 "(")
        (goto l1 (1+ c1)))
      (progn (insert "()") (backward-char))))
(define-command splice-sexp () "Take away the parentheses of the list point is in."
  (multiple-value-bind (ol oc cl cc) (enclosing-list)
    (edit-delete cl cc 1)
    (edit-delete ol oc 1)
    (reindent-lines (1+ ol) cl)))
(define-command raise-sexp () "Put the expression after point in place of the list it is in."
  (let ((line (point-line)) (col (point-col)))
    (multiple-value-bind (ol oc cl cc) (enclosing-list)
      (multiple-value-bind (el ec) (sexp-end line col)
        (multiple-value-bind (sl sc) (sexp-start el ec)
          (let ((text (text-between sl sc el ec)))
            (delete-between ol oc cl (1+ cc))
            (goto ol oc)
            (insert text)
            (goto ol oc)
            (reindent-lines (1+ ol) (+ ol (count #\Newline text)))))))))
(define-command slurp-sexp ()
  "Move this list's closing parenthesis past the expression after it: (a b) c becomes (a b c)."
  (multiple-value-bind (ol oc cl cc) (enclosing-list)
    (declare (ignore oc))
    (multiple-value-bind (el ec) (sexp-end cl (1+ cc))
      (insert-at el ec ")")             ; after the old one, which stays where it is
      (edit-delete cl cc 1)
      (reindent-lines (1+ ol) el))))
(define-command barf-sexp ()
  "Move this list's closing parenthesis back before its last expression: (a b c) becomes (a b) c."
  (multiple-value-bind (ol oc cl cc) (enclosing-list)
    (declare (ignore oc))
    (multiple-value-bind (sl sc) (sexp-start cl cc)
      (multiple-value-bind (pl pc) (skip-blank-backward sl sc)
        (edit-delete cl cc 1)
        (edit-insert pl pc (if (and (= pl sl) (= pc sc)) ") " ")"))
        (reindent-lines (1+ ol) cl)))))
