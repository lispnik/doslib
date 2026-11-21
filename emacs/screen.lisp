;;;; The screen
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

;;; The editor draws character cells -- a character and an attribute byte
;;; each, 80 by 25 -- into *SCREEN*: normally the VGA's text memory itself.
;;; To show the editor somewhere else (a second display: see
;;; emacs-extras/live.lisp), point *SCREEN* at 4000 bytes of your own and
;;; set *FLUSH-SCREEN*, called with no arguments whenever the cells have
;;; been drawn, *PLACE-CURSOR*, called with a column and a row, and
;;; *LEAVE-DISPLAY*, called when the editor exits.
(defvar *screen* (int-sap #xB8000))
(defvar *flush-screen* nil)
(defvar *place-cursor* nil)
(defvar *leave-display* nil)
(defun reset-display ()
  (setf *screen* (int-sap #xB8000) *flush-screen* nil *place-cursor* nil *leave-display* nil))
(defvar *attrs* (make-array 256 :element-type '(unsigned-byte 8)))
(defun put-string (x y string attribute &optional (width (length string)))
  (dotimes (i width)
    (let ((code (if (< i (length string)) (char-code (char string i)) 32)))
      (when (< (+ x i) 80)
        (setf (sap-ref-16 *screen* (* 2 (+ x i (* 80 y))))
              (logior (if (< code 256) code 63) (ash attribute 8)))))))
(defun set-cursor (x y)
  (if *place-cursor*
      (funcall *place-cursor* x y)
      (int86 #x10 :ax #x0200 :bx 0 :dx (logior (ash y 8) x)))
  (when *flush-screen* (funcall *flush-screen*)))
(defun whitespace-p (c) (member c '(#\Space #\Tab #\Newline)))
(defun delimiter-p (c) (or (whitespace-p c) (find c "()\"';")))
(defvar *highlight-words*
  '("let" "let*" "if" "when" "unless" "cond" "case" "lambda" "loop" "progn" "flet" "labels"
    "dolist" "dotimes" "setf" "setq" "and" "or" "not" "return" "handler-case" "in-package"
    "multiple-value-bind" "unwind-protect" "declare" "function" "quote" "require" "load"))
(defun ensure-attrs (n)
  (when (< (length *attrs*) (+ n 1))
    (setf *attrs* (make-array (* 2 (+ n 1)) :element-type '(unsigned-byte 8)))))
(defun lisp-attributes (text &optional state)
  "Fill *ATTRS* with a colour for each character of a line of Lisp.  STATE is
what the line starts inside -- :STRING, :SYMBOL, a number (how deep in #| |#
comments) or NIL -- and what the line ends inside is returned."
  (let ((i 0) (n (length text)) (depth 0))
    (labels ((paint (from to attribute) (fill *attrs* attribute :start from :end (min to n)))
             (string-end (from)         ; past the closing quote, or NIL if it is not on this line
               (loop for j from from below n
                     do (cond ((char= (char text j) #\\) (incf j))
                              ((char= (char text j) #\") (return (1+ j))))
                     finally (return nil)))
             (bar-end (from)            ; past the | that closes a |symbol|, or NIL
               (loop for j from from below n
                     do (cond ((char= (char text j) #\\) (incf j))
                              ((char= (char text j) #\|) (return (1+ j))))
                     finally (return nil)))
             (comment-end (from)        ; past the |# that brings DEPTH to 0, or NIL
               (let ((j from))
                 (loop while (< j (1- n))
                       do (cond ((and (char= (char text j) #\|) (char= (char text (1+ j)) #\#))
                                 (incf j 2)
                                 (when (zerop (decf depth)) (return-from comment-end j)))
                                ((and (char= (char text j) #\#) (char= (char text (1+ j)) #\|))
                                 (incf depth) (incf j 2))
                                (t (incf j))))
                 nil)))
      (cond ((eq state :symbol)
             (let ((end (bar-end 0)))
               (unless end (return-from lisp-attributes :symbol))
               (setf i end)))
            ((eq state :string)
             (let ((end (string-end 0)))
               (paint 0 (or end n) 14)
               (unless end (return-from lisp-attributes :string))
               (setf i end)))
            ((integerp state)
             (setf depth state)
             (let ((end (comment-end 0)))
               (paint 0 (or end n) 3)
               (unless end (return-from lisp-attributes depth))
               (setf i end))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond ((char= c #\;) (paint i n 3) (return))
                       ((char= c #\")
                        (let ((end (string-end (1+ i))))
                          (paint i (or end n) 14)
                          (unless end (return-from lisp-attributes :string))
                          (setf i end)))
                       ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\\))
                        (let ((end (or (position-if #'delimiter-p text :start (min n (+ i 3))) n)))
                          (paint i end 14) (setf i end)))
                       ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
                        (setf depth 1)
                        (let ((end (comment-end (+ i 2))))
                          (paint i (or end n) 3)
                          (unless end (return-from lisp-attributes depth))
                          (setf i end)))
                       ((and (char= c #\:) (or (= i 0) (delimiter-p (char text (1- i)))))
                        (let ((end (or (position-if #'delimiter-p text :start i) n)))
                          (paint i end 13) (setf i end)))
                       ((char= c #\\) (incf i 2))
                       ((char= c #\|)   ; |a symbol|: nothing in it is a string or a comment
                        (let ((end (bar-end (1+ i))))
                          (unless end (return-from lisp-attributes :symbol))
                          (setf i end)))
                       ((char= c #\()
                        (let* ((end (or (position-if #'delimiter-p text :start (1+ i)) n))
                               (word (subseq text (1+ i) end)))
                          (cond ((and (> (length word) 2) (string-equal "def" word :end2 3))
                                 (paint (1+ i) end 11)
                                 (let* ((name (position-if-not #'whitespace-p text :start end))
                                        (name-end (and name (or (position-if #'delimiter-p text :start name) n))))
                                   (when (and name (> name-end name)) (paint name name-end 15))))
                                ((member word *highlight-words* :test #'string-equal)
                                 (paint (1+ i) end 11)))
                          (incf i)))
                       (t (incf i)))))
      nil)))
(defun lex-line (text state)
  "What a line of Lisp ends inside -- :STRING, :SYMBOL (a |symbol| not yet
closed), a number (how deep in #| |# comments) or NIL -- given what it
starts inside."
  (let* ((text (if (simple-string-p text) text (coerce text 'simple-string)))
         (i 0) (n (length text)) (depth (if (integerp state) state 0))
         (in-string (eq state :string)) (in-symbol (eq state :symbol)))
    (declare (type simple-string text) (type fixnum i n depth)
             (optimize (speed 2) (safety 1)))
    (loop while (< i n)
          do (let ((c (schar text i)) (next (if (< (1+ i) n) (schar text (1+ i)) #\Newline)))
               (cond (in-string (cond ((char= c #\\) (incf i))
                                      ((char= c #\") (setf in-string nil))))
                     (in-symbol (cond ((char= c #\\) (incf i))
                                      ((char= c #\|) (setf in-symbol nil))))
                     ((plusp depth)
                      (cond ((and (char= c #\|) (char= next #\#)) (decf depth) (incf i))
                            ((and (char= c #\#) (char= next #\|)) (incf depth) (incf i))))
                     ((char= c #\") (setf in-string t))
                     ((char= c #\;) (return))
                     ((char= c #\\) (incf i))
                     ((and (char= c #\#) (char= next #\\)) (incf i 2))
                     ((and (char= c #\#) (char= next #\|)) (incf depth) (incf i))
                     ((char= c #\|) (setf in-symbol t)))
               (incf i)))
    (cond (in-string :string) (in-symbol :symbol) ((plusp depth) depth))))
;;; Whether a line starts inside a string or a comment is only known by
;;; reading from the top of the buffer.  That is done two ways at once.
;;; Strictly: every quote counts.  Loosely: a parenthesis in the first
;;; column starts afresh, outside everything.  The strict reading is the
;;; right one for text that is whole, and is used when the buffer ends
;;; outside any string or comment.  When it does not -- a quote has been
;;; typed and its partner has not -- the strict reading would turn all the
;;; code below inside out, so the loose one is used until the text is whole
;;; again.
;;;
;;; The answer for each line is kept with the line it was worked out from;
;;; a change makes new strings of the lines it touches, so what is
;;; remembered holds for as far as the lines are still the same objects.
(defun lex-states (&optional (b *buf*))
  "A vector with what each line of B starts inside -- :STRING, a number (how
deep in #| |# comments) or NIL -- or NIL if B is not a Lisp file."
  (when (eq (buf-mode b) 'lisp-mode)
    (flet ((lines-vector () (make-array 64 :adjustable t :fill-pointer 0)))
      (let* ((cache (or (buf-lex b)
                        (setf (buf-lex b) (list (lines-vector) (lines-vector) (lines-vector) nil))))
             (seen (first cache)) (strict (second cache)) (loose (third cache))
             (lines (buf-lines b)) (n (length lines))
             (known (min n (length seen)))
             ;; (this runs at every question about the text: straight at the vectors' storage)
             (valid (let ((old (sb-ext:array-storage-vector seen))
                          (new (sb-ext:array-storage-vector lines)))
                      (declare (type simple-vector old new) (type fixnum known)
                               (optimize (speed 2) (safety 0)))
                      (or (loop for i of-type fixnum from 0 below known
                                unless (eq (svref old i) (svref new i)) return i)
                          known))))
        (when (or (< valid n) (> (length seen) n))
          (setf (fill-pointer seen) valid (fill-pointer strict) valid (fill-pointer loose) valid)
          (let ((s (and (plusp valid) (lex-line (aref seen (1- valid)) (aref strict (1- valid)))))
                (l (and (plusp valid) (lex-line (aref seen (1- valid)) (aref loose (1- valid))))))
            (loop for i from valid below n
                  do (let ((text (aref lines i)))
                       (when (and (plusp (length text)) (char= (char text 0) #\()) (setf l nil))
                       (vector-push-extend text seen)
                       (vector-push-extend s strict) (vector-push-extend l loose)
                       (let ((same (eql s l)))
                         (setf s (lex-line text s)
                               l (if same s (lex-line text l))))))
            (setf (fourth cache) s)))   ; what the buffer ends inside, read strictly
        (if (fourth cache) loose strict)))))
(defun line-start-state (line &optional (b *buf*))
  "What line LINE of B starts inside: :STRING, a number (how deep in #| |#
comments) or NIL.  Always NIL in a buffer that is not a Lisp file."
  (let ((states (lex-states b)))
    (cond ((null states) nil)
          ((< line (length states)) (aref states line))
          (t (let ((last (1- (length states)))) (lex-line (line-text last b) (aref states last)))))))
(defun top-level-line-p (i &optional (b *buf*))
  "Does line I start a top-level form?  A parenthesis in the first column
does, unless the line starts inside a string or a comment (see LEX-STATES)."
  (let ((text (line-text i b)))
    (and (plusp (length text)) (char= (char text 0) #\() (null (line-start-state i b)))))
(defun region-bounds (&optional (b *buf*))
  "The mark and point in order: L1 C1 L2 C2."
  (let ((mark (buf-mark b)))
    (unless mark (editor-error "The mark is not set"))
    (if (or (< (car mark) (buf-line b)) (and (= (car mark) (buf-line b)) (<= (cdr mark) (buf-col b))))
        (values (car mark) (cdr mark) (buf-line b) (buf-col b))
        (values (buf-line b) (buf-col b) (car mark) (cdr mark)))))
(defun mode-name (mode) (or (get mode 'mode-name) (string-capitalize mode)))
(defun visual-col (text col)
  "The screen column that character COL of TEXT lands in (tabs go to every 8th)."
  (let ((v 0))
    (dotimes (i (min col (length text)) v)
      (setf v (if (char= (char text i) #\Tab) (* 8 (1+ (floor v 8))) (1+ v))))))
(defun draw-window (w)
  (let* ((b (win-buf w)) (current (eq w *window*)) (height (win-height w))
         (saved (and (not current) (win-point w)))
         (line (min (1- (nlines b)) (if saved (car saved) (buf-line b))))
         (col (min (length (line-text line b)) (if saved (cdr saved) (buf-col b))))
         (match nil) (state nil) (states nil) (prompt-line nil))
    (when (or (< line (win-top w)) (>= line (+ (win-top w) height)))
      (setf (win-top w) (max 0 (- line (floor height 2)))))
    (when current
      (let ((v (visual-col (line-text line b) col)))
        (cond ((< v 79) (setf (win-hscroll w) 0))       ; it fits: no need to be scrolled
              ((and (plusp (win-hscroll w)) (<= v (win-hscroll w)))
               (setf (win-hscroll w) (max 0 (- v 10))))
              ((>= v (+ (win-hscroll w) 79)) (setf (win-hscroll w) (- v 70)))))
      ;; (matching parentheses matter in text and code, not in a mode that draws its own screen)
      (unless (mode-hook (buf-mode b) 'line-attributes)
        (setf match (ignore-errors (matching-paren-position)))))
    (case (buf-mode b)
      (lisp-mode (setf states (ignore-errors (lex-states b))))
      (repl-mode (setf prompt-line (repl-prompt-line b))
       (when prompt-line
         (loop for i from prompt-line below (min (win-top w) (nlines b))
               do (setf state (lex-line (line-text i b) (and (> i prompt-line) state)))))))
    (multiple-value-bind (l1 c1 l2 c2)
        (if (and current (buf-mark-active b)) (region-bounds b) (values -1 0 -1 0))
      (dotimes (y height)
        (let ((i (+ (win-top w) y)) (row (+ (win-row w) y)))
          (if (>= i (nlines b))
              (put-string 0 row "" 7 80)
              (let* ((text (line-text i b)) (n (length text)) (h (win-hscroll w)) (v 0))
                (ensure-attrs n)
                (fill *attrs* 7 :end (1+ n))
                (case (buf-mode b)
                  ;; (a mode from elsewhere colours its lines by a function
                  ;; on its name: see MODE-HOOK in buffers.lisp)
                  ((dired-mode lisp-mode repl-mode))
                  (t (let ((colour (mode-hook (buf-mode b) 'line-attributes)))
                       (when colour (funcall colour b i text)))))
                (case (buf-mode b)
                  (dired-mode (dired-attributes b i n))
                  ;; (a string or a #| |# comment may have begun on an earlier line)
                  (lisp-mode (lisp-attributes text (and states (aref states i))))
                  ;; (in the REPL, what is being typed after the prompt may run over lines)
                  (repl-mode (setf state (lisp-attributes text (and prompt-line (> i prompt-line) state)))
                   (let ((p (search "> " text)))
                     (when (and p (< p 24) (find-package (subseq text 0 p)))
                       (fill *attrs* 10 :end (+ p 2))))))
                (when (<= l1 i l2)
                  (loop for x from (if (= i l1) c1 0) below (if (= i l2) c2 (1+ n))
                        do (setf (aref *attrs* x) (logior (logand (aref *attrs* x) 15) #x10))))
                (when (and match (= (car match) i) (<= (cdr match) n))
                  (setf (aref *attrs* (cdr match)) #x4F))
                (flet ((cell (v code attribute)
                         (let ((x (- v h)))
                           (when (< -1 x 80)
                             (setf (sap-ref-16 *screen* (* 2 (+ x (* 80 row))))
                                   (logior (if (< code 256) code 63) (ash attribute 8)))))))
                  (dotimes (k n)
                    (let ((code (char-code (char text k))) (attribute (aref *attrs* k)))
                      (if (= code 9)    ; a tab: blanks up to the next multiple of 8
                          (let ((next (* 8 (1+ (floor v 8)))))
                            (loop for vv from v below next do (cell vv 32 attribute))
                            (setf v next))
                          (progn (cell v code attribute) (incf v)))))
                  (cell v 32 (aref *attrs* n))
                  (loop for vv from (1+ v) below (+ h 80) do (cell vv 32 7))
                  ;; a $ in the margin where the line continues off the screen
                  (when (and (plusp h) (plusp v)) (cell h 36 #x70))
                  (when (> v (+ h 80)) (cell (+ h 79) 36 #x70))))))))
    (put-string 0 (+ (win-row w) height)
                (format nil " ~A ~20A L~D C~D  (~A~:[~; Def~])~@[  ~A~]~@[  ~A~]"
                        (if (buf-modified b) "**" "--") (buf-name b) (1+ line) col
                        (mode-name (buf-mode b)) (and current *defining-macro*)
                        (and (buf-file b) (if (buf-crlf b) "CRLF" "LF"))   ; its line endings
                        (buf-file b))
                (if current #x70 #x17) 80)))
(defun draw-echo (&optional (text nil prompt))
  ;; Of a prompt too long for the line, the end matters: that is where the
  ;; typing is.  Of a message, the beginning.
  (put-string 0 24
              (cond (prompt (subseq text (max 0 (- (length text) 79))))
                    ((> (length *message*) 79)
                     (concatenate 'string (subseq *message* 0 76) "..."))
                    (t (or *message* "")))
              7 80)
  (when *flush-screen* (funcall *flush-screen*)))
(defun redisplay ()
  ;; something drew graphics: back to text (unless the VGA is not ours to draw on)
  (unless (or *flush-screen* (= (peek #x449) 3)) (video-mode 3))
  (dolist (w *windows*) (draw-window w))
  (draw-echo)
  (set-cursor (- (visual-col (current-line) (buf-col *buf*)) (win-hscroll *window*))
              (+ (win-row *window*) (- (buf-line *buf*) (win-top *window*)))))
