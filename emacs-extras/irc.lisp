;;;; The IRC client as an editor mode.
;;;;
;;;;   M-x irc     asks for a server (host or host:port), a nick and a channel
;;;;
;;;; The server gets a buffer (*irc host*), every channel you are in gets one
;;;; named after it (#lisp), and so does everybody who writes to you
;;;; privately.  The last line of each is a prompt: type there and press RET
;;;; to say it in that buffer's channel.  A line that starts with a slash is
;;;; a command:
;;;;
;;;;   /join #channel    /part [#channel]    /nick name    /me does something
;;;;   /msg nick text    /query nick         /raw anything /quit [text]
;;;;
;;;; M-p and M-n recall what you typed before.  /quit, or killing the server
;;;; buffer, closes the connection; killing a channel's buffer leaves the
;;;; channel.
;;;;
;;;; DOS has no threads.  The connection is looked at from an idle hook, that
;;;; is, while the editor waits for a key: what has arrived is put in the
;;;; right buffer whichever buffer you are editing, and a buffer whose point
;;;; is on its prompt keeps it there, so it scrolls as a chat window should.
;;;; Line parsing is borrowed from the full-screen client in
;;;; examples/network/irc.lisp.  Plain IRC on port 6667; there is no TLS.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

;;; One connection at a time.  A TARGET is a channel or a person, with the
;;; buffer that shows it; MEMBERS (channels only) says where to show a QUIT
;;; or a change of nick, which do not name a channel.
(defstruct irc-conn socket host nick buffer (partial "") targets registered channel)
(defstruct irc-target name buffer members (joined t))
(defvar *irc* nil "The IRC connection, or NIL.")
(defvar *irc-changed* nil "Set when a buffer changes, so that the idle hook can say so.")
(defvar *irc-octets* (make-array 2048 :element-type '(unsigned-byte 8)))
(defvar *irc-last-server* "")
(defvar *irc-last-nick* "")

(defun irc-channel-p (name) (and (plusp (length name)) (find (char name 0) "#&+!")))
(defun irc-find-target (conn name)
  (find name (irc-conn-targets conn) :key #'irc-target-name :test #'string-equal))
(defun irc-buffers (conn)
  (remove-if-not (lambda (b) (member b *buffers*))
                 (cons (irc-conn-buffer conn) (mapcar #'irc-target-buffer (irc-conn-targets conn)))))

;;; Buffers.  The last line is always the prompt and what is being typed
;;; after it; everything that arrives goes in above that line.
(defun irc-set-prompt (b prompt)
  (let* ((last (1- (nlines b))) (line (line-text last b)) (old (buf-prompt b)))
    (setf (aref (buf-lines b) last)
          (concatenate 'string prompt (if (and old (eql (search old line) 0)) (subseq line (length old)) ""))
          (buf-prompt b) prompt (buf-undo b) nil (buf-redo b) nil (buf-modified b) nil)))
(defun irc-make-buffer (name prompt)
  "A buffer called NAME in IRC mode; one left over from an earlier connection is used again."
  (let* ((old (find-buf name))
         (b (if (and old (eq (buf-mode old) 'irc-mode)) old (make-buf name))))
    (setf (buf-mode b) 'irc-mode)
    (irc-set-prompt b prompt)
    (setf (buf-line b) (1- (nlines b)) (buf-col b) (length (line-text (buf-line b) b)))
    b))
(defun irc-append (b control &rest arguments)
  "Add a line (wrapped, if it is long) above the prompt of B.  Points and marks
on the prompt line move down with it; a window that was showing the prompt
still shows it."
  (let ((text (remove-if (lambda (c) (< (char-code c) 32))        ; colour codes and the like
                         (apply #'format nil control arguments))))
    (dolist (piece (or (text-wrap text (1- *columns*)) (list "")))
      (let ((at (1- (nlines b))) (mark (buf-mark b)))
        (vector-insert (buf-lines b) at piece)
        (when (>= (buf-line b) at) (incf (buf-line b)))
        (when (and mark (>= (car mark) at)) (setf (buf-mark b) (cons (1+ (car mark)) (cdr mark))))
        (dolist (w *windows*)
          (let ((point (win-point w)))
            (when (and (eq (win-buf w) b) point (>= (car point) at))
              (setf (win-point w) (cons (1+ (car point)) (cdr point)))))))))
  ;; undo records hold line numbers, and those have just changed
  (setf (buf-undo b) nil (buf-redo b) nil (buf-modified b) nil)
  ;; keep the prompt on the bottom line of a window that follows the talk;
  ;; left alone, the redisplay would centre it and waste half the window
  (dolist (w *windows*)
    (when (and (eq (win-buf w) b) (win-height w))
      (let ((line (if (and (not (eq w *window*)) (win-point w)) (car (win-point w)) (buf-line b))))
        (when (and (= line (1- (nlines b))) (>= line (+ (win-top w) (win-height w))))
          (setf (win-top w) (- (nlines b) (win-height w)))))))
  (setf *irc-changed* t))

(defun irc-target (conn name)
  "The target called NAME, made with its buffer if there is none yet."
  (let ((target (irc-find-target conn name)))
    (when (and target (not (member (irc-target-buffer target) *buffers*)))    ; its buffer was killed
      (setf (irc-conn-targets conn) (remove target (irc-conn-targets conn)) target nil))
    (or target
        (let ((new (make-irc-target :name name :buffer (irc-make-buffer name (format nil "~A> " name)))))
          (push new (irc-conn-targets conn))
          (setf *irc-changed* t)
          new))))

;;; The connection
(defun irc-close (reason)
  "Close the connection, if there is one, and say why in its buffers."
  (let ((conn *irc*))
    (when conn
      (setf *irc* nil)
      (remove-idle-hook 'irc-idle)
      (ignore-errors (doslib-sockets:socket-close (irc-conn-socket conn)))
      (dolist (b (irc-buffers conn))
        (irc-append b "*** Disconnected from ~A: ~A" (irc-conn-host conn) reason))
      (message "IRC: disconnected from ~A: ~A" (irc-conn-host conn) reason)
      (setf *irc-changed* t))))
(defun irc-send (control &rest arguments)
  "Send one protocol line."
  (let ((conn *irc*))
    (unless conn (editor-error "Not connected to an IRC server (M-x irc connects)"))
    (let* ((line (remove-if (lambda (c) (member c '(#\Return #\Newline)))
                            (apply #'format nil control arguments)))
           (octets (make-array (+ 2 (length line)) :element-type '(unsigned-byte 8))))
      (dotimes (i (length line))
        (let ((code (char-code (char line i)))) (setf (aref octets i) (if (< code 256) code 63))))
      (setf (aref octets (length line)) 13 (aref octets (1+ (length line))) 10)
      (handler-case
          (loop while (plusp (length octets))   ; a send may take only part of it
                do (let ((sent (doslib-sockets:socket-send (irc-conn-socket conn) octets nil)))
                     (unless (and sent (plusp sent)) (error "nothing could be sent"))
                     (setf octets (subseq octets sent))))
        (error (e)
          (irc-close (one-line (princ-to-string e)))
          (editor-error "IRC: the connection is lost: ~A" e))))))

(defun irc-strip-rank (name)            ; @op and +voice in a list of names
  (string-left-trim "@+%~&" name))
(defun irc-words (text)
  (loop with start = 0
        for from = (position #\Space text :start start :test-not #'char=)
        while from
        collect (subseq text from (setf start (or (position #\Space text :start from) (length text))))))
(defun irc-notify (b control &rest arguments)
  "Say something in the echo area, unless the buffer it is about is the current one."
  (unless (eq b *buf*) (apply #'message control arguments)))

(defun irc-handle-line (conn line)
  "Act on one line from the server."
  (multiple-value-bind (who command parameters) (doslib-examples.irc::parse line)
    (let* ((text (or (car (last parameters)) "")) (to (or (first parameters) ""))
           (me (irc-conn-nick conn)) (server (irc-conn-buffer conn))
           (mine (and who (string-equal who me))))
      (cond ((string= command ""))      ; an empty line
            ((string= command "PING") (irc-send "PONG :~A" text))
            ((string= command "PONG"))
            ((string= command "PRIVMSG")
             (let* ((private (not (irc-channel-p to)))
                    (b (irc-target-buffer (irc-target conn (if private (or who "?") to)))))
               (cond ((and (> (length text) 8)
                           (string= (subseq text 0 8) (format nil "~CACTION " (code-char 1))))
                      (irc-append b "* ~A ~A" who (subseq text 8)))
                     ((and (plusp (length text)) (char= (char text 0) (code-char 1)))
                      (irc-append server "--- ~A asks: ~A" who text))      ; some other CTCP request
                     (t (irc-append b "<~A> ~A" who text)
                        (when (or private (search me text :test #'char-equal))
                          (irc-notify b "IRC: <~A> ~A  (in buffer ~A)" who text (buf-name b)))))))
            ((string= command "NOTICE")
             (let ((target (and who (irc-find-target conn (if (irc-channel-p to) to who)))))
               (irc-append (if target (irc-target-buffer target) server) "-~A- ~A" (or who "server") text)))
            ((string= command "JOIN")
             (let ((target (irc-target conn text)))
               (cond (mine
                      (setf (irc-target-joined target) t (irc-target-members target) '())
                      (irc-append (irc-target-buffer target) "--> you have joined ~A"
                                  (irc-target-name target))
                      ;; go there, if the user is looking at IRC anyway; someone
                      ;; editing a file would not thank us for taking it away
                      (if (eq (buf-mode *buf*) 'irc-mode)
                          (select-buffer (irc-target-buffer target))
                          (message "IRC: you have joined ~A (C-x b ~:*~A shows it)"
                                   (irc-target-name target))))
                     (t (pushnew who (irc-target-members target) :test #'string-equal)
                        (irc-append (irc-target-buffer target) "--> ~A has joined ~A"
                                    who (irc-target-name target))))))
            ((string= command "PART")
             (let ((target (irc-find-target conn to)))
               (when target
                 (cond (mine (setf (irc-target-joined target) nil)
                             (irc-append (irc-target-buffer target) "<-- you have left ~A" to))
                       (t (setf (irc-target-members target)
                                (remove who (irc-target-members target) :test #'string-equal))
                          (irc-append (irc-target-buffer target) "<-- ~A has left ~A~@[ (~A)~]"
                                      who to (second parameters)))))))
            ((string= command "QUIT")
             (dolist (target (irc-conn-targets conn))
               (when (or (member who (irc-target-members target) :test #'string-equal)
                         (string-equal who (irc-target-name target)))
                 (setf (irc-target-members target)
                       (remove who (irc-target-members target) :test #'string-equal))
                 (irc-append (irc-target-buffer target) "<-- ~A has quit (~A)" who text))))
            ((string= command "NICK")
             (when mine
               (setf (irc-conn-nick conn) text *irc-last-nick* text)
               (irc-append server "--- you are now known as ~A" text))
             (dolist (target (irc-conn-targets conn))
               (let ((b (irc-target-buffer target)))
                 (cond (mine (irc-append b "--- you are now known as ~A" text))
                       ((member who (irc-target-members target) :test #'string-equal)
                        (setf (irc-target-members target)
                              (substitute text who (irc-target-members target) :test #'string-equal))
                        (irc-append b "--- ~A is now known as ~A" who text))
                       ((string-equal who (irc-target-name target))  ; a private talk follows the person
                        (setf (irc-target-name target) text (buf-name b) text)
                        (irc-set-prompt b (format nil "~A> " text))
                        (irc-append b "--- ~A is now known as ~A" who text))))))
            ((string= command "433")    ; the nick is taken
             (cond ((irc-conn-registered conn)
                    (irc-append server "--- ~A: ~A" (second parameters) text)
                    (irc-notify nil "IRC: ~A: ~A" (second parameters) text))
                   (t (setf (irc-conn-nick conn) (concatenate 'string me "_"))  ; not in yet: try another
                      (irc-append server "--- that nick is in use; trying ~A" (irc-conn-nick conn))
                      (irc-send "NICK ~A" (irc-conn-nick conn)))))
            ((string= command "001")    ; welcome: we are in, under the name the server says
             (setf (irc-conn-registered conn) t)
             (when (and (string/= to "") (string/= to "*")) (setf (irc-conn-nick conn) to))
             (irc-append server "--- ~A" text)
             (when (plusp (length (irc-conn-channel conn))) (irc-send "JOIN ~A" (irc-conn-channel conn))))
            ((string= command "353")    ; who is in a channel
             (let ((target (irc-find-target conn (or (third parameters) ""))))
               (when target
                 (dolist (name (irc-words text))
                   (pushnew (irc-strip-rank name) (irc-target-members target) :test #'string-equal)))
               (irc-append (if target (irc-target-buffer target) server)
                           "--- in ~A: ~A" (third parameters) text)))
            ((string= command "366"))   ; the end of that list
            ((member command '("331" "332") :test #'string=)
             (let ((target (irc-find-target conn (or (second parameters) ""))))
               (irc-append (if target (irc-target-buffer target) server)
                           "--- topic of ~A: ~A" (second parameters) text)))
            ((string= command "ERROR") (irc-append server "*** ~A" text))   ; the server hangs up next
            ((and (= (length command) 3) (every #'digit-char-p command))
             (irc-append server "--- ~{~A~^ ~}" (rest parameters))
             (when (find (char command 0) "45")         ; an error: the user should see it now
               (irc-notify server "IRC: ~{~A~^ ~}" (rest parameters))))
            (t (irc-append server "--- ~A" line))))))

(defun irc-poll (conn)
  "Take what has arrived from the server, without waiting for any."
  ;; (IRC-BUFFER-KILLED sees to killed buffers at once; these two checks are
  ;; for a buffer that went some other way)
  (cond ((not (member (irc-conn-buffer conn) *buffers*))        ; the server buffer is gone
         (ignore-errors (irc-send "QUIT :bye"))
         (irc-close "its buffer was killed"))
        (t
         (dolist (target (irc-conn-targets conn))       ; so was a channel's: leave the channel
           (unless (member (irc-target-buffer target) *buffers*)
             (setf (irc-conn-targets conn) (remove target (irc-conn-targets conn)))
             (when (and (irc-target-joined target) (irc-channel-p (irc-target-name target)))
               (irc-send "PART ~A" (irc-target-name target)))))
         (let ((socket (irc-conn-socket conn)))
           (when (doslib-sockets:socket-ready-p socket 0)
             (let ((n (nth-value 1 (doslib-sockets:socket-receive socket *irc-octets* nil))))
               (cond ((or (null n) (<= n 0)) (irc-close "the server closed the connection"))
                     (t (let ((data (make-string n)))
                          (dotimes (i n) (setf (char data i) (code-char (aref *irc-octets* i))))
                          (setf data (concatenate 'string (irc-conn-partial conn) data))
                          (loop for newline = (position #\Newline data)
                                while (and newline (eq *irc* conn))     ; a line may end it all
                                do (let ((line (string-right-trim '(#\Return) (subseq data 0 newline))))
                                     (setf data (subseq data (1+ newline)))
                                     (irc-handle-line conn line)))
                          ;; a "line" with no end is not IRC: do not let it grow for ever
                          (setf (irc-conn-partial conn) (if (> (length data) 8192) "" data)))))))))))
(defun irc-idle ()
  "The idle hook: true if a buffer changed."
  (let ((conn *irc*) (*irc-changed* nil))
    (cond ((null conn) (remove-idle-hook 'irc-idle) nil)
          (t (handler-case (irc-poll conn)
               (error (e)               ; whatever went wrong, the editor must go on
                 (if *irc*
                     (irc-close (one-line (princ-to-string e)))
                     (setf *irc-changed* t))))
             *irc-changed*))))

;;; What the user types
(defun irc-here ()
  "The target the current buffer shows, or :SERVER, or NIL if it belongs to no live connection."
  (and *irc*
       (if (eq *buf* (irc-conn-buffer *irc*))
           :server
           (find *buf* (irc-conn-targets *irc*) :key #'irc-target-buffer))))
(defun irc-say-target ()                ; where plain text typed in this buffer goes
  (let ((here (irc-here)))
    (cond ((null *irc*) (editor-error "Not connected to an IRC server (M-x irc connects)"))
          ((null here) (editor-error "This buffer is from an earlier connection"))
          ((eq here :server) (editor-error "This is the server's buffer: /join #channel, or /msg nick text"))
          ((not (irc-target-joined here))
           (editor-error "You have left ~A (/join goes back)" (irc-target-name here)))
          (t (irc-target-name here)))))
(defun irc-quit (text)
  (unless *irc* (editor-error "Not connected to an IRC server"))
  (ignore-errors (irc-send "QUIT :~A" (if (string= text "") "bye" text)))
  (irc-close "you quit"))
(defun irc-input (text)
  "Do what a line typed at the prompt says."
  (flet ((word-and-rest (string)
           (let* ((string (string-left-trim " " string)) (space (position #\Space string)))
             (values (subseq string 0 space) (if space (string-left-trim " " (subseq string space)) "")))))
    (cond ((string= text ""))
          ((char/= (char text 0) #\/)
           (irc-send "PRIVMSG ~A :~A" (irc-say-target) text)
           (irc-append *buf* "<~A> ~A" (irc-conn-nick *irc*) text))
          (t (multiple-value-bind (command argument) (word-and-rest (subseq text 1))
               (flet ((is (name) (string-equal command name)))
                 (cond ((is "quit") (irc-quit argument))
                       ((null *irc*) (editor-error "Not connected to an IRC server (M-x irc connects)"))
                       ((is "join")
                        (let ((here (irc-here)))
                          (when (and (string= argument "") (irc-target-p here) (not (irc-target-joined here)))
                            (setf argument (irc-target-name here))))   ; back into the channel just left
                        (when (string= argument "") (editor-error "/join needs a channel after it"))
                        (irc-send "JOIN ~A" (if (irc-channel-p argument)
                                                argument
                                                (concatenate 'string "#" argument))))
                       ((is "part")
                        (let ((here (irc-here)))
                          (cond ((string/= argument "") (irc-send "PART ~A" argument))
                                ((and (irc-target-p here) (irc-channel-p (irc-target-name here)))
                                 (irc-send "PART ~A" (irc-target-name here)))
                                (t (editor-error "/part needs a channel after it here")))))
                       ((is "nick")
                        (when (string= argument "") (editor-error "/nick needs a name after it"))
                        (irc-send "NICK ~A" argument))
                       ((is "me")
                        (irc-send "PRIVMSG ~A :~CACTION ~A~C" (irc-say-target) (code-char 1) argument
                                  (code-char 1))
                        (irc-append *buf* "* ~A ~A" (irc-conn-nick *irc*) argument))
                       ((is "msg")
                        (multiple-value-bind (to message) (word-and-rest argument)
                          (when (string= message "") (editor-error "/msg needs a nick and a text after it"))
                          (irc-send "PRIVMSG ~A :~A" to message)
                          ;; shown in the buffer of that talk; a channel we are not in has none
                          (if (and (irc-channel-p to) (not (irc-find-target *irc* to)))
                              (irc-append *buf* "-> ~A: ~A" to message)
                              (let ((b (irc-target-buffer (irc-target *irc* to))))
                                (irc-append b "<~A> ~A" (irc-conn-nick *irc*) message)
                                (irc-notify b "Sent; the talk with ~A is in the buffer ~A"
                                            to (buf-name b))))))
                       ((is "query")
                        (when (or (string= argument "") (irc-channel-p argument))
                          (editor-error "/query needs a nick after it"))
                        (select-buffer (irc-target-buffer (irc-target *irc* (word-and-rest argument))))
                        (end-of-buffer))
                       ((is "raw") (irc-send "~A" argument))
                       (t (editor-error "There is no /~A: /join /part /nick /me /msg /query /raw /quit"
                                        command)))))))))

(define-command irc-return ()
  "Send the line typed after the prompt to this buffer's channel.  A line that
starts with / is a command: /join /part /nick /me /msg /query /raw /quit."
  (let ((last (1- (nlines))) (prompt (or (buf-prompt *buf*) "")))
    (if (< (point-line) last)
        (end-of-buffer)                 ; RET in the history: go to the prompt
        (let* ((line (line-text last))
               (text (string-trim " " (if (eql (search prompt line) 0) (subseq line (length prompt)) line)))
               (b *buf*))
          (setf (aref (buf-lines b) last) prompt
                (buf-undo b) nil (buf-redo b) nil (buf-modified b) nil (buf-history-index b) -1)
          (goto last (length prompt))
          (unless (or (string= text "") (equal text (first (buf-history b)))) (push text (buf-history b)))
          (irc-input text)))))

(define-command irc ()
  "Connect to an IRC server: it asks for the server (host or host:port), your
nick and a channel to join.  Each channel gets a buffer; type at its prompt."
  (when *irc*
    (if (ask-y-or-n (format nil "Already connected to ~A; disconnect?" (irc-conn-host *irc*)))
        (irc-quit "")
        (error 'quit-command)))
  (let* ((where (string-trim " " (read-string "IRC server (host or host:port): "
                                              :initial *irc-last-server*)))
         (colon (position #\: where))
         (host (subseq where 0 colon))
         (port (if colon (or (parse-integer where :start (1+ colon) :junk-allowed t) 0) 6667))
         (nick (string-trim " " (read-string "Nick: " :initial *irc-last-nick*)))
         (channel (string-trim " " (read-string "Channel to join (none: just RET): "))))
    (when (string= host "") (error 'quit-command))
    (unless (< 0 port 65536) (editor-error "~A is not a port number" (subseq where (1+ colon))))
    (when (or (string= nick "") (find #\Space nick)) (editor-error "A nick is one word"))
    (unless (or (string= channel "") (irc-channel-p channel))
      (setf channel (concatenate 'string "#" channel)))
    (setf *irc-last-server* where *irc-last-nick* nick)
    (draw-echo (format nil "Connecting to ~A, port ~D ..." host port))
    (let ((socket (make-instance 'doslib-sockets:inet-socket :type :stream :protocol :tcp)))
      (handler-case
          (doslib-sockets:socket-connect
           socket
           (if (every (lambda (c) (or (digit-char-p c) (char= c #\.))) host)
               (doslib-sockets:make-inet-address host)
               (doslib-sockets:host-ent-address (doslib-sockets:get-host-by-name host)))
           port)
        (error (e)
          (ignore-errors (doslib-sockets:socket-close socket))
          (editor-error "Can't connect to ~A: ~A" where e)))
      (let ((b (irc-make-buffer (format nil "*irc ~A*" host) "> ")))
        (setf *irc* (make-irc-conn :socket socket :host host :nick nick :buffer b :channel channel))
        (irc-append b "*** Connected to ~A, port ~D, as ~A" host port nick)
        (select-buffer b)
        (end-of-buffer)
        (irc-send "NICK ~A" nick)
        (irc-send "USER ~A 0 * :SBCL on DOS" nick)
        (add-idle-hook 'irc-idle)
        (message "Connected to ~A.  Type at the prompt; /quit disconnects." host)))))

;;; Colours: what kind of line it is shows in how it starts.  (The rest of a
;;; long line that was wrapped is plain.)
(defun irc-line-attributes (b i text)
  (let* ((n (length text)) (prompt (buf-prompt b))
         (me (if *irc* (irc-conn-nick *irc*) *irc-last-nick*))
         (close (and (plusp n) (char= (char text 0) #\<) (position #\> text))))
    (flet ((starts (prefix) (eql (search prefix text) 0)))
      (cond ((and prompt (= i (1- (nlines b))) (starts prompt))
             (fill *attrs* 10 :end (length prompt)))                  ; the prompt
            ((starts "*** ") (fill *attrs* 12 :end n))                ; connected, disconnected
            ((starts "--- ") (fill *attrs* 8 :end n))                 ; what the server says: dim
            ((or (starts "--> ") (starts "<-- ")) (fill *attrs* 3 :end n))    ; coming and going
            ((starts "* ") (fill *attrs* 13 :end n))                  ; /me
            ((starts "-") (fill *attrs* 6 :end n))                    ; a notice
            (close
             (cond ((string-equal me text :start2 1 :end2 close) (fill *attrs* 15 :end n))   ; said by you
                   (t (fill *attrs* 11 :end (1+ close))
                      (when (and (plusp (length me)) (search me text :start2 close :test #'char-equal))
                        (fill *attrs* 14 :start (1+ close) :end n)))))))))     ; said to you
;;; A buffer is being killed: the server's ends the connection, a channel's
;;; leaves the channel.  Nothing here may stop the buffer from going.
(defun irc-buffer-killed (b)
  (let ((conn *irc*))
    (when conn
      (ignore-errors
       (if (eq b (irc-conn-buffer conn))
           (irc-quit "")
           (let ((target (find b (irc-conn-targets conn) :key #'irc-target-buffer)))
             (when target
               (setf (irc-conn-targets conn) (remove target (irc-conn-targets conn)))
               (when (and (irc-target-joined target) (irc-channel-p (irc-target-name target)))
                 (irc-send "PART ~A" (irc-target-name target))))))))))

(setf (get 'irc-mode 'mode-name) "IRC"
      (get 'irc-mode 'line-attributes) 'irc-line-attributes
      (get 'irc-mode 'kill-function) 'irc-buffer-killed)
(bind-key "RET" 'irc-return 'irc-mode)
(bind-key "M-p" 'repl-previous-input 'irc-mode)         ; the prompt is kept as the REPL keeps its own,
(bind-key "M-n" 'repl-next-input 'irc-mode)             ; so its commands for it work here
(bind-key "C-a" 'repl-beginning-of-line 'irc-mode)
(bind-key "<home>" 'repl-beginning-of-line 'irc-mode)
