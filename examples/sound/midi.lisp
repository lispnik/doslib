;;;; MIDI out through an MPU-401 interface: a phrase for piano, bass, strings
;;;; and drums, sent a byte at a time to whatever synthesizer is plugged in,
;;;; with every byte shown as it goes.
;;;;
;;;;   (doslib-examples:run "midi")           play, if there is an interface
;;;;   (doslib-examples:run "midi" "show")    show the bytes even if there is none
;;;; From DOS: DEMO MIDI, DEMO MIDI SHOW.  The phrase goes round until Esc (or
;;;; Q) is pressed.  Without an interface it says so in one line and returns.
;;;;
;;;; MIDI is not sound: it is a serial line, 31250 bits a second, down which
;;;; go the things a player does at a keyboard.  "Key 60 went down this hard
;;;; on channel 1" is three bytes: 90h 3Ch 64h.  The first byte of a message
;;;; has its top bit set and says what it is (top four bits) and for which of
;;;; sixteen channels (the other four); the bytes after it have the top bit
;;;; clear.  The ones used here:
;;;;
;;;;   8n key velocity      note off          9n key velocity     note on
;;;;   Bn controller value  a control: 7 is the volume; 123 means "all notes off"
;;;;   Cn program           choose the channel's instrument
;;;;
;;;; Key 60 is middle C and a step is a semitone.  The sound is entirely the
;;;; business of the box at the other end.
;;;;
;;;; THE INTERFACE.  Roland's MPU-401 (1984) was the card and box that gave a
;;;; PC MIDI sockets, and its two ports became the standard that later sound
;;;; cards copied: data at 330h, and at 331h a status to read and commands to
;;;; write.  Status bit 6 clear means the interface will take a byte; bit 7
;;;; clear means it has one for us to read at 330h.  The real MPU-401 was
;;;; clever -- it could keep time and play back tracks by itself -- but
;;;; almost nothing used that.  Programs sent command 3Fh, "UART mode",
;;;; after which the interface is only a serial port: every byte written to
;;;; 330h goes out of the socket.  Clones implement nothing else.
;;;;
;;;; Finding it: wait for bit 6 to clear, write the reset command FFh to 331h,
;;;; and an interface answers with the acknowledge byte FEh at 330h.  (One
;;;; that was left in UART mode obeys the reset but does not answer, so the
;;;; reset is tried twice.)  Then 3Fh, which is acknowledged too.  A port
;;;; with nothing behind it reads FFh: bit 6 never clears, and after a
;;;; bounded number of looks the program gives up.  On the way out it sends
;;;; note-offs for what is sounding, "all notes off" on each channel used,
;;;; and the reset again, which leaves UART mode.
;;;;
;;;; GENERAL MIDI.  "Program 5" meant a different instrument on every
;;;; synthesizer until 1991, when General MIDI fixed a list: 128 programs in
;;;; families of eight (1-8 pianos, 33-40 basses, 41-56 strings, 57-64 brass
;;;; ...), at least 24 notes at once, and channel 10 for percussion, where
;;;; the key chooses the drum (36 bass drum, 38 snare, 42 closed hi-hat).
;;;; The lists count from 1 and the bytes from 0: "program 33, Acoustic
;;;; Bass" is sent as C1h 20h.  A file written to that list sounds right,
;;;; near enough, on any General MIDI device; this phrase is.
;;;;
;;;; WHY GAMES HAD BOTH.  In the late 1980s a PC could have an AdLib card (the
;;;; OPL2 chip of the "adlib" and "tune" examples: cheap, FM, later on every
;;;; Sound Blaster) or, for several times the price, a Roland MT-32 -- a
;;;; separate synthesizer with sampled attacks, eight parts and a rhythm
;;;; part, plugged into an MPU-401.  The music was usually composed on the
;;;; MT-32 and then arranged again for the AdLib's nine thin voices; the
;;;; game's set-up program asked which you had, and a driver for each turned
;;;; the same score into FM register writes or into MIDI bytes.  The MT-32
;;;; is older than General MIDI and its programs are in another order (and
;;;; its parts are on channels 2 to 9), so later games carried a third
;;;; arrangement for General MIDI modules such as Roland's Sound Canvas.
;;;;
;;;; What is on the screen: each message as it is sent -- the step of the
;;;; phrase, the bytes in hexadecimal, and what they mean -- and counts of
;;;; messages and bytes.  The phrase is four bars (C, A minor, F, G), a
;;;; semiquaver every two ticks of the BIOS's clock (136 crotchets a minute):
;;;; that clock is coarse, but a ninth of a second is the finest division
;;;; here and the keyboard is looked at every tick.
;;;;
;;;; QEMU has no MPU-401 (its sb16 device does not answer at 330h, and there
;;;; is no other device to add one), so under QEMU this says there is no
;;;; interface; "show" is the way to see what would be sent.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.midi
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main #:mpu-present-p #:mpu-send #:phrase-events))
(in-package #:doslib-examples.midi)

;;; ---- the interface
(defconstant +data+ #x330)
(defconstant +status+ #x331 "Read: the status.  Written: a command.")
(defconstant +patience+ 20000 "How many times to look at the status before giving up.")

(defun mpu-ready-p ()
  "Wait until the interface will take a byte (status bit 6 clear).  False
if it never does: there is none, or it is stuck."
  (loop repeat +patience+
        thereis (not (logbitp 6 (inb +status+)))))

(defun mpu-command (command)
  "Write COMMAND to the interface and wait for its acknowledge, FEh.  True if it came."
  (when (mpu-ready-p)
    (outb +status+ command)
    (loop repeat +patience+
          thereis (and (not (logbitp 7 (inb +status+)))         ; a byte for us:
                       (= (inb +data+) #xFE)))))                ; is it the acknowledge?

(defun mpu-present-p ()
  "Reset the interface at 330h and put it in UART mode.  True if it
acknowledged both; false if there is no interface."
  (and (or (mpu-command #xFF) (mpu-command #xFF))
       (mpu-command #x3F)))

(defvar *pretend* nil "True when there is no interface and the bytes are only shown.")
(defvar *bytes* 0 "Bytes sent.")
(defvar *refused* 0 "Bytes the interface was never ready for.")

(defun mpu-send (byte)
  "Send BYTE out of the MIDI socket, when the interface is ready for it."
  (incf *bytes*)
  (unless *pretend*
    ;; anything that has come in at the MIDI IN socket is read and dropped
    (loop repeat 16 until (logbitp 7 (inb +status+)) do (inb +data+))
    (if (mpu-ready-p)
        (outb +data+ byte)
        (incf *refused*))))

;;; ---- the phrase
(defparameter *parts*
  ;; channel (1 to 16), General MIDI program (1 to 128; none on the drum
  ;; channel), its name, velocity, and the notes: (step length key ...), steps
  ;; and lengths in semiquavers, a key by name (C4 is middle C) or number
  '((1 1 "Acoustic Grand Piano" 100
     (0 4 e5) (4 4 g5) (8 6 c6) (14 2 g5)   (16 4 a5) (20 4 e5) (24 8 c5)
     (32 4 f5) (36 4 a5) (40 6 c6) (46 2 a5)   (48 4 g5) (52 4 b5) (56 8 d6))
    (2 33 "Acoustic Bass" 110
     (0 6 c2) (8 4 c2) (12 4 g2)   (16 6 a1) (24 4 a1) (28 4 e2)
     (32 6 f1) (40 4 f1) (44 4 c2)   (48 6 g1) (56 4 g1) (60 4 d2))
    (3 49 "String Ensemble 1" 64
     (0 16 c4 e4 g4)   (16 16 a3 c4 e4)   (32 16 a3 c4 f4)   (48 16 b3 d4 g4))
    (10 nil "Percussion" 110            ; 36 bass drum, 38 snare, 42 closed hi-hat
     (0 2 36 42) (2 1 42) (4 2 38 42) (6 1 42) (8 2 36 42) (10 1 42) (12 2 38 42) (14 1 42)
     (16 2 36 42) (18 1 42) (20 2 38 42) (22 1 42) (24 2 36 42) (26 1 42) (28 2 38 42) (30 1 42)
     (32 2 36 42) (34 1 42) (36 2 38 42) (38 1 42) (40 2 36 42) (42 1 42) (44 2 38 42) (46 1 42)
     (48 2 36 42) (50 1 42) (52 2 38 42) (54 1 42) (56 2 36 42) (58 1 38) (60 1 38) (62 1 38)))
  "The four parts of the phrase.")

(defconstant +steps+ 64 "The phrase's length in semiquavers: four bars.")
(defconstant +step-ticks+ 2 "BIOS ticks to a semiquaver.")

(defun key-number (key)
  "The MIDI number of KEY: a number, or a name like C4, F#3 or BB2 (C4 is 60)."
  (if (integerp key)
      key
      (let* ((text (string-upcase key)) (end (1- (length text))))
        (+ (position (char text 0) "C D EF G A B")
           (* 12 (1+ (digit-char-p (char text end))))
           (loop for c across (subseq text 1 end) sum (if (char= c #\#) 1 -1))))))

(defun key-name (number)
  (format nil "~A~D" (svref #("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B") (mod number 12))
          (1- (floor number 12))))

(defun phrase-events ()
  "The phrase as messages in the order they are sent: a list of (STEP . BYTES).
At the same step, note-offs come before note-ons."
  (let ((events '()))
    (loop for (channel nil nil velocity . notes) in *parts*
          do (loop for (step length . keys) in notes
                   do (dolist (key keys)
                        (let ((number (key-number key)))
                          (push (list step 1 (+ #x90 (1- channel)) number velocity) events)
                          (push (list (+ step length) 0 (+ #x80 (1- channel)) number 64)
                                events)))))
    (mapcar (lambda (event) (cons (first event) (cddr event)))
            (stable-sort (nreverse events)
                         (lambda (a b) (or (< (first a) (first b))
                                           (and (= (first a) (first b))
                                                (< (second a) (second b)))))))))

;;; ---- the screen
(defconstant +log-top+ 4)
(defconstant +log-lines+ 18)
(defvar *log* '() "The last lines of the log, newest first: (text . attribute).")

(defun describe-message (bytes)
  "What the MIDI message BYTES means, in words."
  (destructuring-bind (status &optional (one 0) (two 0)) bytes
    (let ((channel (1+ (logand status 15))))
      (flet ((name (key)                ; on channel 10 a key is a drum
               (if (= channel 10)
                   (format nil "~D ~A" key
                           (case key (36 "bass drum") (38 "snare") (42 "hi-hat") (t "")))
                   (key-name key))))
      (case (logand status #xF0)
        (#x80 (format nil "channel ~2D  note off  ~A" channel (name one)))
        (#x90 (format nil "channel ~2D  note on   ~12A velocity ~D" channel (name one) two))
        (#xB0 (format nil "channel ~2D  control ~D = ~D~A" channel one two
                      (case one (7 ": volume") (123 ": all notes off") (t ""))))
        (#xC0 (format nil "channel ~2D  program ~D: ~A" channel (1+ one)
                      (third (find channel *parts* :key #'first))))
        (t "?"))))))

(defun log-line (text attribute)
  (push (cons text attribute) *log*)
  (when (> (length *log*) +log-lines+)
    (setf *log* (subseq *log* 0 +log-lines+))))

(defun draw-log ()
  (loop for (text . attribute) in (reverse *log*)
        for row from +log-top+
        do (text-put 2 row (format nil "~76A" text) attribute)))

(defvar *messages* 0 "Messages sent.")

(defun send-message (step bytes)
  "Send a MIDI message and put it in the log.  STEP is where in the phrase, or NIL."
  (dolist (byte bytes) (mpu-send byte))
  (incf *messages*)
  (log-line (format nil "~:[    ~;~:*~4D~]   ~{~2,'0X ~}~18T~A"
                    step bytes (describe-message bytes))
            (case (logand (first bytes) #xF0) (#x90 15) (#x80 8) (t 11))))

(defun draw-frame ()
  (text-fill 0 0 80 25 7)
  (text-fill 0 0 80 1 #x1F)
  (text-put 1 0 "MIDI out" #x1E)
  (text-put 11 0 (if *pretend*
                     "No interface at 330h: the bytes are shown, not sent"
                     "MPU-401 at 330h: reset acknowledged (FEh), UART mode")
            #x1F)
  (text-put 2 2 "Step   Bytes      What they say" 3)
  (text-put 2 24 "Esc: leave" #x70))

(defun draw-counts (step times)
  (text-put 2 23 (format nil "Bar ~D, step ~2D   time through ~D   ~6D messages  ~6D bytes~
                              ~:[~;  ~:*~D refused~]   "
                         (1+ (floor step 16)) (mod step 16) (1+ times) *messages* *bytes*
                         (and (plusp *refused*) *refused*))
            7))

;;; ---- playing
(defun play ()
  "Send the phrase round and round until Esc.  Returns the times through."
  (let ((sounding '())                  ; (channel-byte key) of the notes that are on
        (events (phrase-events))
        (start (ticks)) (times 0))
    (unwind-protect
         (progn
           ;; each channel's instrument and volume
           (loop for (channel program) in *parts*
                 do (when program
                      (send-message nil (list (+ #xC0 (1- channel)) (1- program))))
                    (send-message nil (list (+ #xB0 (1- channel)) 7 100)))
           (draw-log)
           (loop
             (let ((step (floor (- (ticks) start) +step-ticks+)) (sent nil))
               (loop while (and events (<= (first (first events)) step))
                     do (destructuring-bind (at status key velocity) (pop events)
                          (send-message at (list status key velocity))
                          (if (= (logand status #xF0) #x90)
                              (push (list (- status #x10) key) sounding)
                              (setf sounding (delete (list status key) sounding
                                                     :test #'equal :count 1)))
                          (setf sent t)))
               (when sent (draw-log))
               (draw-counts (min step (1- +steps+)) times)
               (when (and (null events) (>= step +steps+))      ; round again
                 (setf events (phrase-events))
                 (incf start (* +steps+ +step-ticks+))
                 (incf times)))
             (when (key-pressed-p)
               (when (member (get-key) '(#\Escape #\q #\Q))
                 (return)))
             (wait-tick)))
      ;; however it ends: nothing is left sounding
      (loop for (status key) in sounding
            do (send-message nil (list status key 64)))
      (loop for (channel) in *parts*
            do (send-message nil (list (+ #xB0 (1- channel)) 123 0))))
    times))

(defun main (&optional show)
  "Send a phrase to a MIDI synthesizer through the MPU-401 at 330h; with
SHOW (\"show\"), show the bytes even when there is no interface."
  (let ((present (mpu-present-p))
        (show (and show (string-equal (string show) "show"))))
    (unless (or present show)
      (format t "~&No MPU-401 at 330h: a reset got no FEh back.  DEMO MIDI SHOW shows the bytes.~%")
      (return-from main (values)))
    (setf *pretend* (not present) *bytes* 0 *refused* 0 *messages* 0 *log* '())
    (let ((times 0) (left nil))
      (unwind-protect
           (progn (hide-text-cursor)
                  (draw-frame)
                  (setf times (play)))
        ;; the reset command ends UART mode; in that mode it is not acknowledged
        (unless *pretend*
          (when (mpu-ready-p) (outb +status+ #xFF) (setf left t)))
        (loop while (key-pressed-p) do (wait-key))
        (text-fill 0 0 80 25 7) (text-cursor 0 0) (show-text-cursor))
      (format t "~&~D MIDI messages, ~D bytes~:[ sent~; shown (no interface: none sent)~], ~
                 ~D time~:P through.~%"
              *messages* *bytes* *pretend* times)
      (format t "Ended with all-notes-off on channels ~{~D~^, ~}~:[~;; the interface is reset~].~%"
              (mapcar #'first *parts*) left)
      (when (plusp *refused*)
        (format t "~D bytes were not sent: the interface was never ready for them.~%"
                *refused*))))
  (values))

(doslib-examples:define-example "midi" 'main
  :category :sound
  :description "MIDI out through an MPU-401: a phrase sent byte by byte, and shown.")
