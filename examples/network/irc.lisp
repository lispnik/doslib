;;;; An IRC client: a channel in the top of the screen, a line to type
;;;; in at the bottom.
;;;;
;;;;   (doslib-examples:run "irc" "irc.libera.chat" 6667 "mynick" "#lisp")
;;;;
;;;; What you type is said in the channel.  Lines that start with a slash
;;;; are commands: /join #channel, /nick name, /me does something,
;;;; /msg nick text, /raw anything, /quit.  Esc also leaves.
;;;;
;;;; DOS has no threads, so the program takes turns: it looks at the
;;;; network, then at the keyboard, and so on round.  IRC here is the old
;;;; plain-text protocol on port 6667; there is no TLS.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.irc
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib
        #:doslib-sockets)
  (:export #:main))
(in-package #:doslib-examples.irc)

(defvar *stream* nil)
(defvar *nick* "")
(defvar *channel* "")
(defvar *server* "")
(defvar *log* '() "Lines on the screen, newest first: (colour . text).")
(defvar *input* "")
(defvar *running* t)

(defun redraw ()
  (loop for row from 22 downto 0
        for line in *log*
        do (text-put 0 row (format nil "~80A" (cdr line)) (car line))
        finally (loop for r from row downto 0 do (text-put 0 r (make-string 80 :initial-element #\Space) 7)))
  (text-put 0 23 (format nil " ~A on ~A   ~A~53T/join /nick /me /msg /quit~80T" *nick* *server* *channel*)
            #x70)
  (let ((shown (if (> (length *input*) 76) (subseq *input* (- (length *input*) 76)) *input*)))
    (text-put 0 24 (format nil "> ~78A" shown) 15)
    (text-cursor (+ 2 (length shown)) 24)))
(defun say (colour control &rest arguments)
  (dolist (line (text-wrap (apply #'format nil control arguments) 79))
    (push (cons colour line) *log*))
  (setf *log* (subseq *log* 0 (min 200 (length *log*))))
  (redraw))

(defun send (control &rest arguments)   ; one protocol line
  (apply #'format *stream* control arguments)
  (write-char #\Return *stream*) (write-char #\Newline *stream*)
  (finish-output *stream*))

(defun parse (line)
  "An IRC line as its sender's nick, command, and parameters (the last may have spaces)."
  (let ((prefix nil) (position 0))
    (when (and (plusp (length line)) (char= (char line 0) #\:))
      (setf position (or (position #\Space line) (length line))
            prefix (subseq line 1 position)))
    (let ((parameters '()))
      (loop
        (setf position (or (position #\Space line :start position :test-not #'char=) (length line)))
        (when (>= position (length line)) (return))
        (when (char= (char line position) #\:)
          (push (subseq line (1+ position)) parameters) (return))
        (let ((end (or (position #\Space line :start position) (length line))))
          (push (subseq line position end) parameters)
          (setf position end)))
      (setf parameters (nreverse parameters))
      (values (and prefix (subseq prefix 0 (position #\! prefix)))
              (string-upcase (or (first parameters) ""))
              (rest parameters)))))

(defun handle-line (line)
  (multiple-value-bind (who command parameters) (parse line)
    (let ((text (car (last parameters))))
      (cond ((string= command ""))      ; an empty line: nothing to show
            ((string= command "PING") (send "PONG :~A" (or text "")))
            ((string= command "PRIVMSG")
             (if (and (> (length text) 8) (string= (subseq text 0 8) (format nil "~CACTION " (code-char 1))))
                 (say 13 "* ~A ~A" who (string-trim (string (code-char 1)) (subseq text 8)))
                 (say (if (string-equal (first parameters) *nick*) 14 7) "<~A> ~A" who text)))
            ((string= command "JOIN") (say 10 "--> ~A has joined ~A" who text))
            ((string= command "PART") (say 10 "<-- ~A has left ~A" who (first parameters)))
            ((string= command "QUIT") (say 10 "<-- ~A has quit (~A)" who (or text "")))
            ((string= command "NICK")
             (when (string-equal who *nick*) (setf *nick* text))
             (say 10 "--- ~A is now known as ~A" who text))
            ((string= command "433")    ; the nick is taken: try another
             (setf *nick* (concatenate 'string *nick* "_"))
             (say 12 "--- that nick is in use; trying ~A" *nick*)
             (send "NICK ~A" *nick*))
            ((string= command "001")    ; welcome: we are in
             (say 11 "--- ~A" text)
             (send "JOIN ~A" *channel*))
            ((string= command "353") (say 11 "--- in ~A: ~A" (third parameters) text))
            ;; the server hangs up after an ERROR; the main loop notices that and stops
            ((string= command "ERROR") (say 12 "--- ~A" text))
            ((and (= (length command) 3) (every #'digit-char-p command))
             (say 8 "--- ~A" (or text "")))
            (t (say 8 "--- ~A" line))))))

(defun handle-input (text)
  (flet ((word-and-rest (start)
           (let ((space (position #\Space text :start start)))
             (values (subseq text start space) (if space (string-left-trim " " (subseq text space)) "")))))
    (cond ((string= text ""))
          ((char/= (char text 0) #\/)
           (send "PRIVMSG ~A :~A" *channel* text)
           (say 15 "<~A> ~A" *nick* text))
          (t (multiple-value-bind (command argument) (word-and-rest 1)
               (cond ((string-equal command "quit") (send "QUIT :~A" (if (string= argument "") "bye" argument))
                      (setf *running* nil))
                     ;; a command with its argument missing would only confuse the server
                     ((and (string= argument "") (member command '("join" "nick" "msg") :test #'string-equal))
                      (say 12 "--- /~A needs something after it" command))
                     ((string-equal command "join") (send "PART ~A" *channel*) (setf *channel* argument)
                      (send "JOIN ~A" argument))
                     ((string-equal command "nick") (send "NICK ~A" argument))
                     ((string-equal command "me")
                      (send "PRIVMSG ~A :~CACTION ~A~C" *channel* (code-char 1) argument (code-char 1))
                      (say 13 "* ~A ~A" *nick* argument))
                     ((string-equal command "msg")
                      (multiple-value-bind (to message) (word-and-rest (+ 2 (length command)))
                        (send "PRIVMSG ~A :~A" to message)
                        (say 14 "-> ~A: ~A" to message)))
                     ((string-equal command "raw") (send "~A" argument))
                     (t (say 12 "--- there is no /~A" command))))))))

(defun main (&optional (server "irc.libera.chat") (port 6667) (nick "doslisp") (channel "#doslib"))
  "An IRC client: a channel above, a line to type in below."
  (setf *nick* nick *channel* channel *server* server *log* '() *input* "" *running* t)
  (text-fill 0 0 80 25 7)
  (show-text-cursor)
  (say 11 "--- connecting to ~A, port ~A ..." server port)
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp))
        (partial ""))
    (unwind-protect
         (handler-case
             (progn
               (socket-connect socket
                               (if (every (lambda (c) (or (digit-char-p c) (char= c #\.))) server)
                                   (make-inet-address server)
                                   (host-ent-address (get-host-by-name server)))
                               port)
               (setf *stream* (socket-make-stream socket :input t :output t :external-format :latin-1))
               (say 11 "--- connected")
               (send "NICK ~A" *nick*)
               (send "USER ~A 0 * :SBCL on DOS" *nick*)
               (loop while *running*
                     do ;; the network: whatever has arrived, a line at a time
                        (loop for c = (read-char-no-hang *stream* nil :eof)
                              while c
                              do (cond ((eq c :eof) (say 12 "--- the server closed the connection")
                                        ;; wait, or the screen is cleared before it can be read
                                        (say 7 "Press a key.") (get-key)
                                        (setf *running* nil) (return))
                                       ((char= c #\Newline)
                                        (handle-line (string-right-trim '(#\Return) partial))
                                        (setf partial ""))
                                       (t (setf partial (concatenate 'string partial (string c))))))
                        ;; the keyboard
                        (loop while (and *running* (key-pressed-p))
                              do (let ((key (get-key)))
                                   (cond ((eql key #\Return)
                                          (let ((text *input*)) (setf *input* "") (handle-input text))
                                          (redraw))
                                         ((eql key (code-char 27)) (send "QUIT :bye") (setf *running* nil))
                                         ((eql key #\Backspace)
                                          (setf *input* (subseq *input* 0 (max 0 (1- (length *input*)))))
                                          (redraw))
                                         ((and (characterp key) (graphic-char-p key))
                                          (setf *input* (concatenate 'string *input* (string key)))
                                          (redraw)))))))
           (error (e) (say 12 "--- ~A" e) (say 7 "Press a key.") (get-key)))
      (when *stream* (ignore-errors (close *stream* :abort t)) (setf *stream* nil))
      (ignore-errors (socket-close socket))
      (text-fill 0 0 80 25 7)
      (text-cursor 0 0))
    (values)))                          ; else the last key pressed is printed at the prompt

(doslib-examples:define-example "irc" 'main
  :category :network
  :description "An IRC client: a channel above, a line to type in below.")
