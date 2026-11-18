;;;; A game with a bug in it, to be fixed while it runs.
;;;;
;;;; Things fall; catch the $ for points and mind the o.  Sooner or later
;;;; a * falls, and the function that scores a catch has never heard of
;;;; it.  In most languages that is the end of the game.  Here the game
;;;; stops in mid-catch and shows what went wrong and what can be done:
;;;; press E and write the missing case --
;;;;
;;;;     (defun drop-value (kind) (case kind (#\$ 10) (#\* 50) (t -5)))
;;;;
;;;; -- then choose "call DROP-VALUE again".  The catch that failed now
;;;; scores, with the score, the lives and everything still falling exactly
;;;; as they were.  L at any time gives a line of Lisp, to change the game
;;;; as it plays: (setf *lives* 9), (setf *speed* 1), a new DRAW-PLAYER...
;;;;
;;;; Left and right arrows move, L is Lisp, Esc leaves.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.hotfix
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.hotfix)

(defvar *score* 0)
(defvar *lives* 3)
(defvar *player* 38)                    ; the column of the catcher's middle
(defvar *drops* '())                    ; each one: (column row kind)
(defvar *ticks* 0)
(defvar *speed* 2 "Timer ticks per step: smaller is faster.")
(defvar *caught* 0)

;;; These are the functions the player is invited to redefine while the
;;; game runs.  Without the declaration the compiler may assume, when it
;;; compiles the rest of this file, that they are what they are here --
;;; that DROP-VALUE can only return -5 or 10, say -- and a new definition
;;; would then be caught breaking a promise it never made.
(declaim (notinline drop-value draw-player drop-colour new-drop))

(defun drop-value (kind)                ; what a catch is worth -- and the bug:
  (ecase kind                           ; nobody told it about #\*
    (#\$ 10)
    (#\o -5)))

(defun draw-player ()
  (text-put (- *player* 2) 22 "\\___/" 14))
(defun drop-colour (kind) (case kind (#\$ 10) (#\* 11) (t 12)))
(defun draw ()
  (text-fill 0 1 80 23 7)
  (dolist (drop *drops*)
    (destructuring-bind (column row kind) drop
      (text-put column row (string kind) (drop-colour kind))))
  (draw-player)
  (text-put 0 0 (format nil " Score ~5D   Lives ~D   Caught ~D~50T<- -> move   L Lisp   Esc leave ~80T"
                        *score* *lives* *caught*)
            #x70))

(defun catch-drop (kind)
  "Score a catch.  If the scoring fails, say how the game could go on."
  (loop
    (restart-case
        (progn (incf *score* (drop-value kind))
               (incf *caught*)
               (return))
      (retry ()
        :report "Call DROP-VALUE again (after you have fixed it).")
      (use-value (points)
        :report "Score this one catch by hand."
        :interactive (lambda () (list (or (parse-integer (dialog-ask "Points:") :junk-allowed t) 0)))
        (incf *score* points)
        (return))
      (let-it-go ()
        :report "Let this one go, unscored."
        (return)))))

(defun new-drop ()
  (if (= *ticks* 24)                    ; the first star falls straight into the catcher
      (list *player* 1 #\*)
      (list (+ 1 (random 78)) 1 (char "$$$$oo*" (random 7)))))

(defun step-game ()
  (incf *ticks*)
  (when (zerop (mod *ticks* 3)) (push (new-drop) *drops*))
  (let ((still-falling '()))
    (dolist (drop *drops*)
      (destructuring-bind (column row kind) drop
        (cond ((and (= row 21) (<= (abs (- column *player*)) 2))
               (catch-drop kind)
               (when (char= kind #\o) (decf *lives*)))
              ((< row 22) (push (list column (1+ row) kind) still-falling)))))
    (setf *drops* (nreverse still-falling))))

(defun lisp-line ()
  "One line of Lisp, evaluated in the running game."
  (let ((saved (save-text-screen)))
    (unwind-protect
         (progn
           (text-put 0 24 (format nil "~80A" "Lisp:") #x1E)
           (show-text-cursor)
           (let ((text (text-input 6 24 73 :attribute #x1E)))
             (hide-text-cursor)
             (when (and text (string/= (string-trim " " text) ""))
               (text-put 0 24
                         (format nil "~80A"
                                 (let ((result (handler-case
                                                   (let ((*package* (find-package '#:doslib-examples.hotfix))
                                                         (*error-output* (make-broadcast-stream)))
                                                     (handler-bind ((warning #'muffle-warning))
                                                       (format nil "=> ~S" (eval (read-from-string text)))))
                                                 (error (e) (substitute #\Space #\Newline (format nil "~A" e))))))
                                   (subseq result 0 (min 62 (length result)))))
                         #x1B)
               (text-put 64 24 "  (press a key)" #x1B)
               (get-key))))
      (restore-text-screen saved))))

(defun play ()
  (loop
    (when (<= *lives* 0) (return :out-of-lives))
    (loop while (key-pressed-p)
          do (let ((key (get-key)))
               (case key
                 (:left (setf *player* (max 2 (- *player* 2))))
                 (:right (setf *player* (min 77 (+ *player* 2))))
                 ((#\l #\L) (lisp-line))
                 (t (when (eql key (code-char 27)) (return-from play :left))))))
    (step-game)
    (draw)
    (wait-tick (max 1 *speed*))))

(defun main ()
  "A game with a bug, fixed from inside its own error while it runs."
  (setf *score* 0 *lives* 3 *player* 38 *drops* '() *ticks* 0 *caught* 0 *speed* 2)
  (text-fill 0 0 80 25 7)
  (hide-text-cursor)
  (let ((ending
          (unwind-protect
               (restart-case
                   (handler-bind ((error (lambda (c)
                                           (restart-dialog c :title "The game has a bug"
                                                             :through 'stop-game
                                                             :eval-package '#:doslib-examples.hotfix))))
                     (play))
                 (stop-game ()
                   :report "Stop the game."
                   :stopped))
            (text-fill 0 0 80 25 7)
            (text-cursor 0 0)
            (show-text-cursor))))
    (format t "~&Game over (~(~A~)): score ~D, ~D caught.~%" ending *score* *caught*)))

(doslib-examples:define-example "hotfix" 'main
  :category :lisp
  :description "A game with a bug, fixed from inside its own error while it runs.")
