;;;; Pong for two machines, over TCP.
;;;;
;;;; One machine hosts and the other joins it:
;;;;
;;;;   (doslib-examples:run "netpong")                 host: waits on port 7777
;;;;   (doslib-examples:run "netpong" "192.168.1.20")  join the host at that address
;;;;
;;;; The host plays the left paddle and runs the game: the ball, the score.
;;;; The other machine sends where its paddle is, a line whenever it moves,
;;;; and draws what the host tells it, a line a frame:
;;;;
;;;;   client -> host   P 11                  my paddle's top row
;;;;   host -> client   S 40 12 9 11 3 2      ball x y, paddles left right, scores
;;;;
;;;; Up and down arrows move; Esc leaves; first to 5 wins.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.netpong
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib
        #:doslib-sockets)
  (:export #:main))
(in-package #:doslib-examples.netpong)

(defconstant +paddle+ 5)                ; its height
(defvar *ball-x*) (defvar *ball-y*) (defvar *dx*) (defvar *dy*)
(defvar *left*) (defvar *right*)        ; top rows of the paddles
(defvar *left-score*) (defvar *right-score*)

(defvar *drawn*)                        ; (ball-x ball-y left right) as on the screen; NIL: nothing yet

(defun draw (title)
  ;; Clearing and redrawing the whole screen 18 times a second flickers: after
  ;; the first frame only what moved is rubbed out and drawn again.
  (flet ((net (row) (when (oddp row) (text-put 39 row (string (code-char 179)) 8)))
         (paddle (x top char attribute)
           (dotimes (i +paddle+) (text-put x (+ top i) (string char) attribute))))
    (cond ((null *drawn*)
           (text-fill 0 0 80 25 7)
           (text-put 0 0 (format nil " ~A~55TUp/Down move   Esc leaves ~80T" title) #x70)
           (loop for row from 1 to 24 do (net row)))
          (t
           (destructuring-bind (x y left right) *drawn*
             (text-put x y " " 7)
             (when (= x 39) (net y))
             (unless (= left *left*) (paddle 2 left #\Space 7))
             (unless (= right *right*) (paddle 77 right #\Space 7)))))
    (text-put 30 0 (format nil "~D  :  ~D " *left-score* *right-score*) #x70)
    (paddle 2 *left* (code-char 219) 11)
    (paddle 77 *right* (code-char 219) 14)
    (text-put *ball-x* *ball-y* "O" 15)
    (setf *drawn* (list *ball-x* *ball-y* *left* *right*))))

(defun serve-ball (direction)
  (setf *ball-x* 40 *ball-y* (+ 6 (random 12)) *dx* direction *dy* (if (zerop (random 2)) 1 -1)))

(defun step-ball ()
  (incf *ball-x* *dx*) (incf *ball-y* *dy*)
  (when (or (<= *ball-y* 1) (>= *ball-y* 24)) (setf *dy* (- *dy*)))
  (flet ((on-paddle-p (top) (<= top *ball-y* (+ top +paddle+ -1))))
    (cond ((and (= *ball-x* 3) (on-paddle-p *left*)) (setf *dx* 1))
          ((and (= *ball-x* 76) (on-paddle-p *right*)) (setf *dx* -1))
          ((<= *ball-x* 0) (incf *right-score*) (serve-ball 1))
          ((>= *ball-x* 79) (incf *left-score*) (serve-ball -1)))))

(defun read-lines (stream partial)
  "The complete lines that have arrived, and the incomplete rest.  A third
value is true if the other side has gone."
  (let ((lines '()))
    (loop for c = (handler-case (read-char-no-hang stream nil :eof) (error () :eof))
          while c
          do (cond ((eq c :eof) (return-from read-lines (values (nreverse lines) partial t)))
                   ((char= c #\Newline) (push partial lines) (setf partial ""))
                   ((char/= c #\Return) (setf partial (concatenate 'string partial (string c))))))
    (values (nreverse lines) partial nil)))

(defun numbers (line)                   ; "S 40 12 9" -> (40 12 9)
  (let ((*read-eval* nil))
    (ignore-errors (with-input-from-string (in (subseq line 1))
                     (loop for n = (read in nil) while n when (integerp n) collect n)))))

(defun send (stream control &rest arguments)    ; false if the other side has gone
  (ignore-errors (apply #'format stream control arguments) (finish-output stream) t))

(defun move-paddle (top)                ; from the keyboard; NIL if Esc was pressed
  (loop while (key-pressed-p)
        do (let ((key (get-key)))
             (case key
               (:up (setf top (max 1 (- top 2))))
               (:down (setf top (min (- 25 +paddle+) (+ top 2))))
               (t (when (eql key (code-char 27)) (return-from move-paddle nil))))))
  top)

(defun play-as-host (stream)
  (let ((partial ""))
    (serve-ball 1)
    (loop
      (setf *left* (or (move-paddle *left*) (return :left)))
      (multiple-value-bind (lines rest gone) (read-lines stream partial)
        (setf partial rest)
        (dolist (line lines)
          (when (and (plusp (length line)) (char= (char line 0) #\P))
            (setf *right* (max 1 (min (- 25 +paddle+) (or (first (numbers line)) *right*))))))
        (when gone (return :other-left)))
      (step-ball)
      (unless (send stream "S ~D ~D ~D ~D ~D ~D~%"
                    *ball-x* *ball-y* *left* *right* *left-score* *right-score*)
        (return :other-left))
      (draw "Pong: you are on the left")
      (when (or (>= *left-score* 5) (>= *right-score* 5)) (return :over))
      (wait-tick 1))))

(defun play-as-guest (stream)
  (let ((partial "") (sent nil))
    (loop
      (setf *right* (or (move-paddle *right*) (return :left)))
      (unless (eql sent *right*)
        (unless (send stream "P ~D~%" *right*) (return :other-left))
        (setf sent *right*))
      (multiple-value-bind (lines rest gone) (read-lines stream partial)
        (setf partial rest)
        (let ((state (numbers (or (car (last lines)) ""))))    ; only the newest matters
          (when (= (length state) 6)
            (destructuring-bind (x y left right left-score right-score) state
              (declare (ignore right))
              (setf *ball-x* x *ball-y* y *left* left *left-score* left-score *right-score* right-score))
            (draw "Pong: you are on the right")))
        ;; The host hangs up straight after the frame with the winning point:
        ;; that frame has to count before the hang-up does.
        (when (or (>= *left-score* 5) (>= *right-score* 5)) (return :over))
        (when gone (return :other-left))))))

(defun main (&optional host (port 7777))
  "Pong for two machines over TCP: host it, or join a host."
  (setf *left* 10 *right* 10 *left-score* 0 *right-score* 0 *ball-x* 40 *ball-y* 12 *dx* 1 *dy* 1
        *drawn* nil)
  ;; The socket comes before the screen is cleared: making the first one brings
  ;; the network up, which may print ("Configuring through DHCP").
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)) (connection nil) (ending nil))
    (hide-text-cursor)
    (text-fill 0 0 80 25 7)
    (unwind-protect
         (handler-case
             (progn
               (cond (host
                      (text-put 2 2 (format nil "Joining the game at ~A, port ~D ..." host port) 11)
                      (socket-connect socket
                                      (if (every (lambda (c) (or (digit-char-p c) (char= c #\.))) host)
                                          (make-inet-address host)
                                          (host-ent-address (get-host-by-name host)))
                                      port)
                      (setf connection socket))
                     (t
                      (setf (sockopt-reuse-address socket) t)
                      (socket-bind socket #(0 0 0 0) port)
                      (socket-listen socket 1)
                      (text-put 2 2 (format nil "Waiting for the other player on port ~D.  Esc gives up."
                                            port)
                                11)
                      (loop until (or ending
                                      (socket-ready-p socket 0))
                            do (when (and (key-pressed-p) (eql (get-key) (code-char 27)))
                                 (setf ending :left)))
                      (unless ending (setf connection (socket-accept socket)))))
               (when connection
                 (let ((stream (socket-make-stream connection :input t :output t :external-format :latin-1)))
                   (setf ending (if host (play-as-guest stream) (play-as-host stream)))
                   (ignore-errors (close stream)))))
           (error (e) (setf ending e)))
      (ignore-errors (socket-close socket))
      (text-fill 0 0 80 25 7)
      (text-cursor 0 0)
      (show-text-cursor))
    (format t "~&~A~@[  Score ~{~D : ~D~}.~]~%"
            (case ending
              (:over (if (eq (>= *left-score* 5) (null host)) "You won!" "You lost."))
              (:left "You left the game.")
              (:other-left "The other player left.")
              (t (format nil "The game stopped: ~A" ending)))
            (and connection (list *left-score* *right-score*)))
    (values)))

(doslib-examples:define-example "netpong" 'main
  :category :network
  :description "Pong for two machines over TCP: host it, or join a host.")
