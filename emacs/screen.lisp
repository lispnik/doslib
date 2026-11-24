;;;; The screen
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

;;; The editor draws character cells -- a character and an attribute byte
;;; each, *COLUMNS* by *LINES* of them -- into *SCREEN*: normally the VGA's
;;; text memory itself.  To show the editor somewhere else (a second
;;; display: see emacs-extras/live.lisp), point *SCREEN* at memory of your
;;; own, say how big it is with DISPLAY-CELLS, and set *FLUSH-SCREEN*,
;;; called with no arguments whenever the cells have been drawn,
;;; *PLACE-CURSOR*, called with a column and a row, and *LEAVE-DISPLAY*,
;;; called when the editor exits.
(defvar *screen* (int-sap #xB8000))
(defvar *flush-screen* nil)
(defvar *place-cursor* nil)
(defvar *leave-display* nil)
(defvar *columns* 80 "How many cells wide the screen being drawn on is.")
(defvar *lines* 25 "How many rows it has: the last is the echo area.")

;;; ---- the size of the VGA's text screen
;;; On a VGA the number of lines is a matter of the font: the same 400
;;; scan lines show 25 rows of a character 16 lines high, 28 of one 14
;;; high, or 50 of one 8 high, and the BIOS has all three fonts (INT 10h,
;;; AX=1114h, 1111h, 1112h).  Loading one is not a change of mode: nothing
;;; on the screen is lost.  More columns, or more lines than 50, are a mode
;;; of their own, which a VESA BIOS may have (modes 108h to 10Ch).
(defvar *screen-size* (list 80 25 3)
  "The text screen wanted on the VGA: columns, lines, and the mode -- 3, or
a VESA text mode.  Set it with SCREEN-SIZE.")
(defvar *display-stale* t "True when the VGA may not be as *SCREEN-SIZE* says.")
(defvar *display-mode-byte* 3 "What the BIOS calls the mode the editor's screen is in.")
(defvar *screen-size-hooks* '() "Functions called, with no arguments, when the screen has changed size.")
(defvar *monochrome* nil "True when the screen is a monochrome card's: no colours.")
(defun reset-display ()
  (setf *screen* (int-sap (text-memory-address)) *monochrome* (and (monochrome-display-p) t)
        *flush-screen* nil *place-cursor* nil *leave-display* nil
        *display-stale* t))
(defun display-cells (columns lines)
  "Say that the screen being drawn on is COLUMNS by LINES, and lay the windows out for it."
  (unless (and (= columns *columns*) (= lines *lines*))
    (setf *columns* columns *lines* lines)
    (when *windows* (layout-windows))
    (mapc #'funcall *screen-size-hooks*)))
(defun display-as-wanted-p ()
  "Is the VGA still the text screen the editor left it as?  (A program that
drew pictures, or set the mode again, has changed it.)"
  (and (not *display-stale*)
       (= (peek #x449) *display-mode-byte*)
       (= (text-columns) *columns*) (= (text-lines) *lines*)))
(defun set-display ()
  "Make the VGA the text screen *SCREEN-SIZE* asks for, or as near as the
card comes to it."
  (destructuring-bind (columns lines mode) *screen-size*
    (declare (ignore columns))
    (cond ((= mode 3)
           (unless (member (peek #x449) '(3 7)) (text-mode))
           (unless (= (text-lines) lines)
             (case (display-adapter)
               ((:vga :mcga)
                (int86 #x10 :ax (case lines (50 #x1112) (28 #x1111) (t #x1114)) :bx 0)
                ;; (the BIOS leaves the cursor shaped for the old font)
                (int86 #x10 :ax #x0100 :cx #x0607))
               ;; an EGA has 350 scan lines and two fonts: 25 rows of 14, 43 of 8
               (:ega (int86 #x10 :ax (if (= lines 43) #x1112 #x1111) :bx 0)))))
          ((not (vesa-set-mode mode))
           (setf *screen-size* (list 80 25 3))
           (text-mode))))
  ;; what the BIOS says came of it is what is drawn on
  (setf *display-mode-byte* (peek #x449) *display-stale* nil)
  (display-cells (text-columns) (text-lines)))
(defun screen-sizes ()
  "The text screens there are to choose from: a list of (columns lines mode)."
  (append (case (display-adapter)
            ((:vga :mcga) (list (list 80 25 3) (list 80 28 3) (list 80 50 3)))
            (:ega (list (list 80 25 3) (list 80 43 3)))
            (t (list (list 80 25 3))))
          (loop for mode from #x108 to #x10C
                for info = (ignore-errors (vesa-mode-info mode))
                when (and info (logbitp 0 (getf info :attributes))
                          (not (logbitp 4 (getf info :attributes)))     ; text, not graphics
                          (<= 80 (getf info :width) 132) (<= 25 (getf info :height) 60))
                  collect (list (getf info :width) (getf info :height) mode))))
(defun screen-size (columns lines)
  "Ask for a text screen of COLUMNS by LINES (80 by 25, 28 or 50 on any VGA;
others if the VESA BIOS has them).  True if there is such a screen.  For
EMACSRC.LSP: (doslib-emacs:screen-size 80 50)."
  (let ((size (find-if (lambda (size) (and (= (first size) columns) (= (second size) lines)))
                       (screen-sizes))))
    (when size
      (setf *screen-size* size *display-stale* t)
      t)))
(defvar *attrs* (make-array 256 :element-type '(unsigned-byte 8)))
(defun put-string (x y string attribute &optional (width (length string)))
  (dotimes (i width)
    (let ((code (if (< i (length string)) (char-code (char string i)) 32)))
      (when (< (+ x i) *columns*)
        (setf (sap-ref-16 *screen* (* 2 (+ x i (* *columns* y))))
              (logior (if (< code 256) code 63)
                      (ash (if *monochrome* (monochrome-attribute attribute) attribute) 8)))))))
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
;;; code below inside out, so the loose one is used from the form with the
;;; lone quote in it onwards, until the text is whole again.
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
            ;; S is what the buffer ends inside, read strictly.  If that is
            ;; something, a quote or a bar is missing -- and it is missing
            ;; in or after the last form the strict reading arrives at
            ;; cleanly.  So the strict reading holds up to that form, and
            ;; the loose one takes over from it.
            (setf (fourth cache)
                  (let ((from (and s (loop for i from (1- n) downto 0
                                           for text = (aref seen i)
                                           when (and (null (aref strict i)) (plusp (length text))
                                                     (char= (char text 0) #\())
                                             return i))))
                    (cond ((null s) strict)
                          ((or (null from) (zerop from)) loose)
                          (t (let ((mixed (subseq loose 0 n)))
                               (replace mixed strict :end2 from)
                               mixed)))))))
        (or (fourth cache) strict)))))
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
        (cond ((< v (1- *columns*)) (setf (win-hscroll w) 0))       ; it fits: no need to be scrolled
              ((and (plusp (win-hscroll w)) (<= v (win-hscroll w)))
               (setf (win-hscroll w) (max 0 (- v 10))))
              ((>= v (+ (win-hscroll w) *columns* -1)) (setf (win-hscroll w) (- v (- *columns* 10))))))
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
              (put-string 0 row "" 7 *columns*)
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
                           (when (< -1 x *columns*)
                             (setf (sap-ref-16 *screen* (* 2 (+ x (* *columns* row))))
                                   (logior (if (< code 256) code 63)
                                           (ash (if *monochrome*
                                                    (monochrome-attribute attribute)
                                                    attribute)
                                                8)))))))
                  (dotimes (k n)
                    (let ((code (char-code (char text k))) (attribute (aref *attrs* k)))
                      (if (= code 9)    ; a tab: blanks up to the next multiple of 8
                          (let ((next (* 8 (1+ (floor v 8)))))
                            (loop for vv from v below next do (cell vv 32 attribute))
                            (setf v next))
                          (progn (cell v code attribute) (incf v)))))
                  (cell v 32 (aref *attrs* n))
                  (loop for vv from (1+ v) below (+ h *columns*) do (cell vv 32 7))
                  ;; a $ in the margin where the line continues off the screen
                  (when (and (plusp h) (plusp v)) (cell h 36 #x70))
                  (when (> v (+ h *columns*)) (cell (+ h *columns* -1) 36 #x70))))))))
    (put-string 0 (+ (win-row w) height)
                (format nil " ~A ~20A L~D C~D  (~A~:[~; View~]~:[~; Def~])~@[  ~A~]~@[  ~A~]"
                        ;; (as in GNU Emacs: ** changed, %% may not be changed)
                        (cond ((buf-modified b) "**") ((buf-read-only b) "%%") (t "--"))
                        (buf-name b) (1+ line) col
                        (mode-name (buf-mode b)) (buf-view b) (and current *defining-macro*)
                        (and (buf-file b) (if (buf-crlf b) "CRLF" "LF"))   ; its line endings
                        (buf-file b))
                (if current #x70 #x17) *columns*)))
(defun draw-echo (&optional (text nil prompt))
  ;; Of a prompt too long for the line, the end matters: that is where the
  ;; typing is.  Of a message, the beginning.
  (put-string 0 (1- *lines*)
              (cond (prompt (subseq text (max 0 (- (length text) (1- *columns*)))))
                    ((> (length *message*) (1- *columns*))
                     (concatenate 'string (subseq *message* 0 (- *columns* 4)) "..."))
                    (t (or *message* "")))
              7 *columns*)
  (when *flush-screen* (funcall *flush-screen*)))
(defun redisplay ()
  ;; something drew graphics, or set the mode: back to our text screen
  ;; (unless the VGA is not ours to draw on)
  (unless (or *flush-screen* (display-as-wanted-p)) (set-display))
  (dolist (w *windows*) (draw-window w))
  (draw-echo)
  (set-cursor (- (visual-col (current-line) (buf-col *buf*)) (win-hscroll *window*))
              (+ (win-row *window*) (- (buf-line *buf*) (win-top *window*)))))
