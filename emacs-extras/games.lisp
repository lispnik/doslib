;;;; Games as editor modes: M-x blocks and M-x snake.
;;;;
;;;; Each game lives in a buffer of its own (*blocks*, *snake*) and is drawn
;;;; there as text, so the rest of the editor goes on working around it:
;;;; other windows, C-x b, C-x o, M-x.  The game's keys come from the
;;;; buffer's mode map:
;;;;
;;;;   blocks   <left> <right> move   <up> rotate   <down> one row down
;;;;            SPC drop to the bottom
;;;;   snake    the arrows steer      SPC pause
;;;;   both     p pause   n new game   q quit (the buffer is buried)
;;;;
;;;; Time comes from an idle hook: while the editor waits for a key the hook
;;;; looks at the BIOS tick count (18.2 a second) and moves the game on when
;;;; enough ticks have gone by.  A game moves only while its buffer is in the
;;;; selected window; anywhere else it says "paused" and waits.
;;;;
;;;; The rules of each game are plain functions of their arguments (rotate a
;;;; piece, does it fit, clear the full rows, move the snake), so they can be
;;;; tried on any Lisp; only GAME-TICKS touches the machine.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

;;; ------------------------------------------------------------------
;;; Blocks: the rules

(defparameter *game-well-width* 10)
(defparameter *game-well-height* 20)
;;; The seven pieces of four cells.  Each is drawn in a square box of SIZE
;;; cells a side and turns about the middle of that box.
(defparameter *game-pieces*
  '((4 (0 . 1) (1 . 1) (2 . 1) (3 . 1))         ; bar
    (2 (0 . 0) (1 . 0) (0 . 1) (1 . 1))         ; square
    (3 (1 . 0) (0 . 1) (1 . 1) (2 . 1))         ; tee
    (3 (2 . 0) (0 . 1) (1 . 1) (2 . 1))         ; hook to the right
    (3 (0 . 0) (0 . 1) (1 . 1) (2 . 1))         ; hook to the left
    (3 (1 . 0) (2 . 0) (0 . 1) (1 . 1))         ; step up
    (3 (0 . 0) (1 . 0) (1 . 1) (2 . 1)))        ; step down
  "Each piece: the side of its box, then its cells as (x . y), y downwards.")
(defun game-piece-size (kind) (first (nth kind *game-pieces*)))
(defun game-piece-cells (kind) (rest (nth kind *game-pieces*)))

(defun game-rotate-cells (cells size)
  "CELLS turned a quarter clockwise inside a box SIZE cells a side."
  (mapcar (lambda (cell) (cons (- size 1 (cdr cell)) (car cell))) cells))
(defun game-make-board ()
  (make-array (list *game-well-height* *game-well-width*) :initial-element 0))
(defun game-fits-p (board cells x y)
  "True if CELLS, with their box at X, Y, are inside the well and on empty squares."
  (destructuring-bind (height width) (array-dimensions board)
    (every (lambda (cell)
             (let ((cx (+ x (car cell))) (cy (+ y (cdr cell))))
               (and (< -1 cx width) (< -1 cy height) (zerop (aref board cy cx)))))
           cells)))
(defun game-merge (board cells x y mark)
  "Write MARK into the squares of BOARD that CELLS cover.  Returns BOARD."
  (dolist (cell cells board)
    (setf (aref board (+ y (cdr cell)) (+ x (car cell))) mark)))
(defun game-clear-rows (board)
  "Take the full rows out of BOARD; what is above them comes down.  Returns how many."
  (destructuring-bind (height width) (array-dimensions board)
    (let ((to (1- height)) (cleared 0))
      (loop for from from (1- height) downto 0
            do (if (loop for x below width always (plusp (aref board from x)))
                   (incf cleared)
                   (progn (unless (= to from)
                            (dotimes (x width) (setf (aref board to x) (aref board from x))))
                          (decf to))))
      (loop for y from to downto 0
            do (dotimes (x width) (setf (aref board y x) 0)))
      cleared)))
(defun game-drop-distance (board cells x y)
  "How many rows the piece at X, Y can still fall."
  (loop for d from 0 while (game-fits-p board cells x (+ y d 1)) finally (return d)))
(defun game-rotated-place (board cells size x y)
  "Turn the piece; if it then sticks into a wall or a block, try it a square or
two to either side.  Returns the new cells and the new X, or NIL."
  (let ((turned (game-rotate-cells cells size)))
    (dolist (dx '(0 -1 1 -2 2) nil)
      (when (game-fits-p board turned (+ x dx) y)
        (return (values turned (+ x dx)))))))
(defun game-row-points (rows level)     ; 50, 150, 300, 500 times the level
  (* 25 rows (1+ rows) level))
(defun game-level (rows) (1+ (floor rows 8)))

;;; ------------------------------------------------------------------
;;; Snake: the rules

(defparameter *game-field-width* 30)
(defparameter *game-field-height* 20)
(defun game-snake-step (snake direction food width height)
  "Move SNAKE (a list of (x . y), head first) one square in DIRECTION (dx . dy).
Returns the new snake and :MOVED or :ATE -- or the old snake and :DEAD if the
head would leave the field or run into the body."
  (let* ((head (first snake))
         (new (cons (+ (car head) (car direction)) (+ (cdr head) (cdr direction))))
         (ate (equal new food))
         ;; the tail moves out of the way, unless the snake is growing
         (body (if ate snake (butlast snake))))
    (if (or (not (< -1 (car new) width)) (not (< -1 (cdr new) height))
            (member new body :test #'equal))
        (values snake :dead)
        (values (cons new body) (if ate :ate :moved)))))
(defun game-snake-free-cells (snake width height)
  (loop for y below height
        nconc (loop for x below width
                    unless (member (cons x y) snake :test #'equal) collect (cons x y))))
(defun game-opposite-p (a b)
  (and (= (car a) (- (car b))) (= (cdr a) (- (cdr b)))))

;;; ------------------------------------------------------------------
;;; A game in a buffer

(defstruct (game (:constructor %make-game))
  buf kind                              ; its buffer; :BLOCKS or :SNAKE
  live                                  ; being played: the idle hook looks after it
  paused away over                      ; paused with p; not in the selected window; lost
  (last 0)                              ; the tick count when it last moved
  (score 0)
  board piece cells size (x 0) (y 0) next (rows 0)      ; blocks
  snake direction turns food)                           ; snake
(defvar *game-list* '() "The games that have a buffer.")
(defvar *game-ascii* nil
  "True: draw with ASCII characters only, not the PC's block and line characters.")

(defun game-ticks () (peek32 #x46C))    ; the BIOS timer: 18.2 ticks a second
(defun game-of (b) (find b *game-list* :key #'game-buf))
(defun game-here ()
  (or (game-of *buf*) (editor-error "There is no game in this buffer")))
(defun game-interval (g)
  "Ticks between two moves: both games get faster as they go on."
  (ecase (game-kind g)
    (:blocks (max 1 (- 11 (game-level (game-rows g)))))
    (:snake (max 2 (- 4 (floor (game-score g) 80))))))

;;; Blocks
(defun game-blocks-spawn (g)
  "Bring the next piece in at the top; if there is no room the game is over."
  (let* ((kind (game-next g)) (size (game-piece-size kind)))
    (setf (game-piece g) kind (game-cells g) (game-piece-cells kind) (game-size g) size
          (game-x g) (floor (- *game-well-width* size) 2) (game-y g) 0
          (game-next g) (random (length *game-pieces*)))
    (unless (game-fits-p (game-board g) (game-cells g) (game-x g) (game-y g))
      (setf (game-over g) t))))
(defun game-blocks-lock (g)
  "The piece has landed: it becomes part of the heap, full rows go, the next one comes."
  ;; (a settled square remembers which piece it came from, for its colour)
  (game-merge (game-board g) (game-cells g) (game-x g) (game-y g) (1+ (game-piece g)))
  (let ((cleared (game-clear-rows (game-board g))))
    (when (plusp cleared)
      (incf (game-score g) (game-row-points cleared (game-level (game-rows g))))
      (incf (game-rows g) cleared)))
  (game-blocks-spawn g))
(defun game-blocks-move (g dx)
  (when (game-fits-p (game-board g) (game-cells g) (+ (game-x g) dx) (game-y g))
    (incf (game-x g) dx)))
(defun game-blocks-fall (g)
  (if (game-fits-p (game-board g) (game-cells g) (game-x g) (1+ (game-y g)))
      (incf (game-y g))
      (game-blocks-lock g)))
(defun game-blocks-rotate (g)
  (multiple-value-bind (cells x)
      (game-rotated-place (game-board g) (game-cells g) (game-size g) (game-x g) (game-y g))
    (when cells (setf (game-cells g) cells (game-x g) x))))
(defun game-blocks-drop (g)
  (let ((d (game-drop-distance (game-board g) (game-cells g) (game-x g) (game-y g))))
    (incf (game-y g) d)
    (incf (game-score g) d)             ; a point for every row it was dropped
    (game-blocks-lock g)))

;;; Snake
(defun game-snake-place-food (g)
  (let ((free (game-snake-free-cells (game-snake g) *game-field-width* *game-field-height*)))
    (setf (game-food g) (and free (nth (random (length free)) free)))
    (unless free (setf (game-over g) t))))      ; the snake fills the field: nothing left to do
(defun game-snake-turn (g direction)
  "Remember a turn for the next move.  Two may be waiting, so that a quick
left-then-up is not lost; a turn straight back into the neck is ignored."
  (let ((before (or (first (last (game-turns g))) (game-direction g))))
    (when (and (< (length (game-turns g)) 2)
               (not (equal direction before)) (not (game-opposite-p direction before)))
      (setf (game-turns g) (append (game-turns g) (list direction))))))
(defun game-snake-advance (g)
  (when (game-turns g) (setf (game-direction g) (pop (game-turns g))))
  (multiple-value-bind (snake what)
      (game-snake-step (game-snake g) (game-direction g) (game-food g)
                       *game-field-width* *game-field-height*)
    (setf (game-snake g) snake)
    (ecase what
      (:moved)
      (:ate (incf (game-score g) 10) (game-snake-place-food g))
      (:dead (setf (game-over g) t)))))

(defun game-reset (g)
  "Start G again from nothing."
  (setf (game-paused g) nil (game-over g) nil (game-away g) nil (game-score g) 0
        (game-last g) (game-ticks) (game-live g) t)
  (ecase (game-kind g)
    (:blocks (setf (game-board g) (game-make-board) (game-rows g) 0
                   (game-next g) (random (length *game-pieces*)))
     (game-blocks-spawn g))
    (:snake (let ((x (floor *game-field-width* 2)) (y (floor *game-field-height* 2)))
              (setf (game-snake g) (loop for i below 4 collect (cons (- x i) y))
                    (game-direction g) '(1 . 0) (game-turns g) '()))
     (game-snake-place-food g)))
  (add-idle-hook 'game-idle))
(defun game-step (g)
  (ecase (game-kind g)
    (:blocks (game-blocks-fall g))
    (:snake (game-snake-advance g))))

;;; ------------------------------------------------------------------
;;; Drawing: the game as lines of text

(defun game-glyph (name)
  "The characters a thing is drawn with: a square of the game is two columns wide."
  (flet ((pc (&rest codes) (map 'string #'code-char codes)))
    (if *game-ascii*
        (ecase name
          (:piece "[]") (:heap "##") (:empty " .") (:blank "  ") (:wall "|") (:floor "-")
          (:corner-1 "+") (:corner-2 "+") (:corner-3 "+") (:corner-4 "+")
          (:head "@@") (:body "oo") (:food "**"))
        (ecase name
          (:piece (pc 219 219)) (:heap (pc 178 178)) (:empty (pc 32 250)) (:blank "  ")
          (:wall (pc 186)) (:floor (pc 205))
          (:corner-1 (pc 201)) (:corner-2 (pc 187)) (:corner-3 (pc 200)) (:corner-4 (pc 188))
          (:head (pc 219 219)) (:body (pc 177 177)) (:food (pc 4 4))))))
(defun game-status (g)
  (cond ((game-over g) "GAME OVER -- n starts a new game")
        ((not (game-live g)) "stopped -- n starts a new game")
        ((or (game-paused g) (game-away g)) "paused")
        (t "")))
(defun game-rule (left right width)     ; a line of a frame
  (with-output-to-string (out)
    (write-string (game-glyph left) out)
    (dotimes (i width) (write-string (game-glyph :floor) out))
    (write-string (game-glyph right) out)))
(defun game-blocks-lines (g)
  (let* ((board (game-board g)) (cells (game-cells g)) (next (game-piece-cells (game-next g)))
         (panel (list "Next:" :next0 :next1 "" (format nil "Score  ~D" (game-score g))
                      (format nil "Rows   ~D" (game-rows g))
                      (format nil "Level  ~D" (game-level (game-rows g)))
                      "" "<left> <right>  move" "<up>     rotate" "<down>   down a row"
                      "SPC      drop" "p        pause" "n        new game" "q        quit")))
    (flet ((falling-p (x y)
             (and (not (game-over g))
                  (member (cons (- x (game-x g)) (- y (game-y g))) cells :test #'equal)))
           (preview (row)
             (with-output-to-string (out)
               (write-string "  " out)
               (dotimes (x 4)
                 (write-string (game-glyph (if (member (cons x row) next :test #'equal) :piece :blank))
                               out)))))
      (append
       (list (format nil "  Blocks    ~A" (game-status g)))
       (loop for y below *game-well-height*
             collect (with-output-to-string (out)
                       (write-string "  " out)
                       (write-string (game-glyph :wall) out)
                       (dotimes (x *game-well-width*)
                         (write-string (game-glyph (cond ((falling-p x y) :piece)
                                                         ((plusp (aref board y x)) :heap)
                                                         (t :empty)))
                                       out))
                       (write-string (game-glyph :wall) out)
                       (let ((item (nth y panel)))
                         (when item
                           (write-string "    " out)
                           (write-string (case item
                                           (:next0 (preview 0)) (:next1 (preview 1)) (t item))
                                         out)))))
       (list (concatenate 'string "  " (game-rule :corner-3 :corner-4 (* 2 *game-well-width*))))))))
(defun game-snake-lines (g)
  (let* ((snake (game-snake g)) (head (first snake)) (food (game-food g))
         (panel (list (format nil "Score   ~D" (game-score g))
                      (format nil "Length  ~D" (length snake))
                      "" "arrows  steer" "p, SPC  pause" "n  new game" "q  quit")))
    (append
     (list (format nil " Snake    ~A" (game-status g))
           (concatenate 'string " " (game-rule :corner-1 :corner-2 (* 2 *game-field-width*))))
     (loop for y below *game-field-height*
           collect (with-output-to-string (out)
                     (write-string " " out)
                     (write-string (game-glyph :wall) out)
                     (dotimes (x *game-field-width*)
                       (let ((cell (cons x y)))
                         (write-string (game-glyph (cond ((equal cell head) :head)
                                                         ((member cell snake :test #'equal) :body)
                                                         ((equal cell food) :food)
                                                         (t :blank)))
                                       out)))
                     (write-string (game-glyph :wall) out)
                     (let ((item (nth y panel)))
                       (when item (write-string "  " out) (write-string item out)))))
     (list (concatenate 'string " " (game-rule :corner-3 :corner-4 (* 2 *game-field-width*)))))))
(defun game-render (g)
  "Write the game out as the text of its buffer, the way Dired does."
  (let* ((b (game-buf g)) (lines (buf-lines b)))
    (setf (fill-pointer lines) 0)
    (dolist (line (ecase (game-kind g)
                    (:blocks (game-blocks-lines g))
                    (:snake (game-snake-lines g))))
      (vector-push-extend line lines))
    ;; Point follows the action, so that a window too short for the whole
    ;; game scrolls to where the piece or the snake's head is.
    (setf (buf-undo b) nil (buf-redo b) nil (buf-modified b) nil (buf-mark-active b) nil
          (buf-col b) 0
          (buf-line b) (ecase (game-kind g)
                         (:blocks (min *game-well-height* (1+ (game-y g))))
                         (:snake (+ 2 (cdr (first (game-snake g)))))))
    ;; The top line says how the game stands, but a short window may not
    ;; show the top line: say it again, briefly, on the line point is on.
    (let ((tag (cond ((game-over g) "[game over]") ((not (game-live g)) "[stopped]")
                     ((or (game-paused g) (game-away g)) "[paused]"))))
      (when tag
        (let ((line (aref lines (buf-line b))))     ; in place of the panel there, if need be
          (setf (aref lines (buf-line b))
                (format nil "~66A ~A" (subseq line 0 (min 66 (length line))) tag)))))
    ;; A window that shows the game but is not the selected one has a point
    ;; of its own: that follows too.
    (dolist (w *windows*)
      (when (and (eq (win-buf w) b) (not (eq w *window*)))
        (setf (win-point w) (cons (buf-line b) 0))))))

;;; Colour (the mode's LINE-ATTRIBUTES function: an attribute byte for each
;;; character of a line, into *ATTRS*, as the window is drawn).  The text
;;; says where things are; the game says what they are.
(defparameter *game-piece-colours* #(11 14 13 6 9 10 12 7)
  "The colour of each kind of piece; the last is for squares that came from none.")
(defun game-status-attributes (g line text)
  "Colour what the top line and the tag at the end of a line say.  LINE is the line's number."
  (let ((n (length text)) (colour (if (game-over g) 12 14)))
    (when (= line 0) (fill *attrs* 15 :end (min n 9)) (fill *attrs* colour :start (min n 9) :end n))
    (when (and (> line 0) (> n 67) (char= (char text 67) #\[)) (fill *attrs* colour :start 67 :end n))))
(defun game-blocks-attributes (b line text)
  (let ((g (game-of b)) (n (length text)))
    (when (and g (game-board g))
      (game-status-attributes g line text)
      (when (and (<= 1 line (1+ *game-well-height*)) (>= n (+ 4 (* 2 *game-well-width*))))
        (let ((y (1- line)) (right (+ 3 (* 2 *game-well-width*))))
          (fill *attrs* 3 :start 2 :end (1+ right))            ; the walls and the floor
          (when (< y *game-well-height*)
            (dotimes (x *game-well-width*)
              (let* ((mark (aref (game-board g) y x))
                     (colour (cond ((and (not (game-over g))
                                         (member (cons (- x (game-x g)) (- y (game-y g))) (game-cells g)
                                                 :test #'equal))
                                    (aref *game-piece-colours* (game-piece g)))
                                   ((plusp mark) (aref *game-piece-colours* (1- mark)))
                                   (t 8))))
                (fill *attrs* colour :start (+ 3 (* 2 x)) :end (+ 5 (* 2 x))))))
          (when (<= 2 line 3)                                   ; the next piece
            (fill *attrs* (aref *game-piece-colours* (game-next g)) :start (1+ right) :end (min n 60))))))))
(defun game-snake-attributes (b line text)
  (let ((g (game-of b)) (n (length text)) (right (+ 2 (* 2 *game-field-width*))))
    (when (and g (game-snake g))
      (game-status-attributes g line text)
      (when (and (>= line 1) (> n right))
        (fill *attrs* 3 :start 1 :end (1+ right))
        (let ((y (- line 2)) (head (first (game-snake g))))
          (when (< -1 y *game-field-height*)
            (dotimes (x *game-field-width*)
              (let* ((cell (cons x y))
                     (colour (cond ((equal cell head) (if (game-over g) 12 10))
                                   ((member cell (game-snake g) :test #'equal) 2)
                                   ((equal cell (game-food g)) 12))))
                (when colour (fill *attrs* colour :start (+ 2 (* 2 x)) :end (+ 4 (* 2 x))))))))))))

;;; ------------------------------------------------------------------
;;; Time: the idle hook

(defun game-idle ()
  "Move on every game that is due.  True if a buffer was changed.  When no
game is being played any more (q, or its buffer was killed) the hook goes."
  (let ((changed nil) (now (game-ticks)))
    (dolist (g *game-list*)
      (cond ((not (member (game-buf g) *buffers*))      ; its buffer went without being killed
             (setf (game-live g) nil *game-list* (remove g *game-list*)))
            ((not (game-live g)))
            ((not (eq (win-buf *window*) (game-buf g)))  ; not where the player is: wait
             (unless (game-away g)
               (setf (game-away g) t changed t)
               (game-render g)))
            ((game-away g)                              ; the player is back
             (setf (game-away g) nil (game-last g) now changed t)
             (game-render g))
            ((or (game-paused g) (game-over g)))
            ((< now (game-last g)) (setf (game-last g) now))    ; midnight: the count starts again
            ((>= (- now (game-last g)) (game-interval g))
             ;; One move, however long it has been (a minibuffer was open, say).
             (setf (game-last g) now changed t)
             (game-step g)
             (game-render g))))
    (unless (some #'game-live *game-list*) (remove-idle-hook 'game-idle))
    changed))

;;; ------------------------------------------------------------------
;;; Commands

(defun game-start (kind name mode)
  "Show the game's buffer; make it, or start a new game in it, if need be."
  (let* ((b (or (find-buf name) (make-buf name))) (g (game-of b)))
    (unless g
      (setf g (%make-game :buf b :kind kind))
      (push g *game-list*))
    (setf (buf-mode b) mode (buf-read-only b) t)
    (select-buffer b)
    (if (and (game-live g) (not (game-over g)))
        (setf (game-paused g) nil (game-away g) nil (game-last g) (game-ticks))  ; go on with it
        (game-reset g))
    (game-render g)))
(define-command blocks ()
  "Play Blocks: turn and place the falling pieces so that rows fill up and vanish."
  (game-start :blocks "*blocks*" 'game-blocks-mode))
(define-command snake ()
  "Play Snake: eat, grow, and keep off the walls and off yourself."
  (game-start :snake "*snake*" 'game-snake-mode))

(defun game-act (blocks-action snake-direction)
  "What a game key does: nothing unless the game is running."
  (let ((g (game-here)))
    (cond ((or (game-over g) (not (game-live g))) (message "n starts a new game"))
          ((game-paused g) (message "Paused: p goes on"))
          (t (ecase (game-kind g)
               (:blocks (funcall blocks-action g))
               (:snake (when snake-direction (game-snake-turn g snake-direction))))
             (game-render g)))))
(define-command game-left () "Blocks: move the piece left.  Snake: turn left."
  (game-act (lambda (g) (game-blocks-move g -1)) '(-1 . 0)))
(define-command game-right () "Blocks: move the piece right.  Snake: turn right."
  (game-act (lambda (g) (game-blocks-move g 1)) '(1 . 0)))
(define-command game-up () "Blocks: turn the piece.  Snake: turn upwards."
  (game-act #'game-blocks-rotate '(0 . -1)))
(define-command game-down () "Blocks: move the piece down a row.  Snake: turn downwards."
  (game-act (lambda (g) (game-blocks-fall g) (setf (game-last g) (game-ticks))) '(0 . 1)))
(define-command game-pause () "Pause the game, or go on with it."
  (let ((g (game-here)))
    (if (or (game-over g) (not (game-live g)))
        (message "n starts a new game")
        (setf (game-paused g) (not (game-paused g)) (game-last g) (game-ticks)))
    (game-render g)))
(define-command game-drop () "Blocks: drop the piece to the bottom.  Snake: pause."
  (if (eq (game-kind (game-here)) :snake)
      (game-pause)
      (game-act (lambda (g) (game-blocks-drop g) (setf (game-last g) (game-ticks))) nil)))
(define-command game-new () "Start a new game."
  (let ((g (game-here)))
    (game-reset g)
    (game-render g)))
(define-command game-quit () "Stop the game and put its buffer away."
  (let* ((g (game-here)) (b (game-buf g))
         (other (find b *buffers* :test-not #'eq)))
    (setf (game-live g) nil)
    (unless (some #'game-live *game-list*) (remove-idle-hook 'game-idle))
    (game-render g)
    (when other                         ; bury it: last in the list of buffers
      (select-buffer other)
      (setf *buffers* (append (remove b *buffers*) (list b))))))

(defun game-killed (b)
  "The game's buffer is being killed: forget the game, and the hook with the last one."
  (let ((g (game-of b)))
    (when g (setf (game-live g) nil *game-list* (remove g *game-list*)))
    (unless (some #'game-live *game-list*) (remove-idle-hook 'game-idle))))
(setf (get 'game-blocks-mode 'mode-name) "Blocks" (get 'game-snake-mode 'mode-name) "Snake"
      (get 'game-blocks-mode 'line-attributes) 'game-blocks-attributes
      (get 'game-snake-mode 'line-attributes) 'game-snake-attributes
      (get 'game-blocks-mode 'kill-function) 'game-killed
      (get 'game-snake-mode 'kill-function) 'game-killed)
(dolist (mode '(game-blocks-mode game-snake-mode))
  (dolist (binding '(("<left>" game-left) ("<right>" game-right) ("<up>" game-up) ("<down>" game-down)
                     ("SPC" game-drop) ("p" game-pause) ("n" game-new) ("q" game-quit)))
    (bind-key (first binding) (second binding) mode)))

;;; ------------------------------------------------------------------
;;; For trying things out (M-: (doslib-emacs::game-debug-rows) in the game's buffer)

(defun game-debug-rows (&optional (g (game-of *buf*)))
  "Blocks: fill the rows the falling piece would land in, all but the squares
it would cover and those it has to pass through on the way -- so that
dropping it now completes its lowest row at least."
  (unless (or (null g) (game-over g) (not (eq (game-kind g) :blocks)))
    (let* ((board (game-board g)) (cells (game-cells g))
           (y (+ (game-y g) (game-drop-distance board cells (game-x g) (game-y g)))))
      (dolist (row (remove-duplicates (mapcar (lambda (cell) (+ y (cdr cell))) cells)))
        (dotimes (x *game-well-width*)
          (unless (some (lambda (cell) (and (= (+ (game-x g) (car cell)) x) (>= (+ y (cdr cell)) row)))
                        cells)
            (setf (aref board row x) 8))))     ; 8: from no piece, drawn grey
      (game-render g)))
  (values))
(defun game-debug-food (&optional (g (game-of *buf*)))
  "Snake: put the food two squares in front of the head, if that is in the field."
  (unless (or (null g) (game-over g) (not (eq (game-kind g) :snake)))
    (let* ((head (first (game-snake g))) (d (game-direction g))
           (cell (cons (+ (car head) (* 2 (car d))) (+ (cdr head) (* 2 (cdr d))))))
      (when (and (< -1 (car cell) *game-field-width*) (< -1 (cdr cell) *game-field-height*)
                 (not (member cell (game-snake g) :test #'equal)))
        (setf (game-food g) cell))
      (game-render g)))
  (values))
