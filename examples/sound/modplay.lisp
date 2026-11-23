;;;; A MOD player: four channels of samples mixed in Lisp, as they play,
;;;; into the Sound Blaster's DMA buffer.
;;;;
;;;;   (doslib-examples:run "modplay")              a small song, made here
;;;;   (doslib-examples:run "modplay" "FILE.MOD")   a ProTracker module
;;;;   (doslib-examples:run "modplay" "FILE.MOD" 11025)   at another rate
;;;;   (doslib-examples:run "modplay" "FILE.MOD" "info")  what is in it, not played
;;;;   (doslib-examples.modplay:write-song "MINE.MOD")    the song made here, as a file
;;;; From DOS: DEMO MODPLAY SONG.MOD, DEMO MODPLAY SONG.MOD INFO.  A file that
;;;; is not where the name says is looked for among the examples' data, where
;;;; SONG.MOD is.  Esc (or any key) ends.  Without a card it says so and returns.
;;;;
;;;; A module is a piece of music as a tracker stores it: up to 31 samples
;;;; (short recordings, 8 bits, signed), and patterns of 64 rows by four
;;;; channels.  A cell of a pattern says "play this sample at this pitch
;;;; now", perhaps with an effect; a table gives the order the patterns are
;;;; played in.  The pitch is an Amiga "period": the sample is played at
;;;; 3546895 / period samples a second.  A row lasts SPEED ticks (6 unless
;;;; the song says otherwise) and a tick 2.5 / TEMPO seconds (tempo 125: a
;;;; fiftieth of a second); notes start on a row's first tick and effects
;;;; that slide work on the others.
;;;;
;;;; Playing it is the "blaster" example with a mixer where the tune was:
;;;; each time the card interrupts, Lisp makes the next block of the song.
;;;; For every output sample each channel adds sample * volume and steps
;;;; along its sample by period-constant / period, a fraction held in fixed
;;;; point (16 bits below the point), wrapping at the loop's end.  That
;;;; inner loop is MIX-CHANNEL: fixnums, declared, nothing allocated, so the
;;;; garbage collector has no reason to run while the music plays.  The
;;;; screen is written straight into video memory for the same reason.  (At
;;;; the end it says how many bytes were allocated while it played: a few
;;;; hundred a second, for the pointers and the keyboard calls.)
;;;;
;;;; The file (PARSE-MOD reads it; examples/support/make-mod.py has the
;;;; layout byte by byte): a title of 20 bytes; 31 sample headers of 30 --
;;;; name, length in words with the big end first, as the Amiga had it,
;;;; finetune, volume, where the loop starts and how long it is; the song's
;;;; length and 128 orders; the letters "M.K." (or "M!K!", "FLT4", "4CHN":
;;;; other trackers, same layout) at byte 1080; the patterns, as many as the
;;;; highest number among the orders says, 1024 bytes each; and then the
;;;; samples' data, one after another.
;;;;
;;;; Effects done: 0 arpeggio, 1 and 2 slide up and down, 3 slide to note,
;;;; 4 vibrato, 5 and 6 (3 and 4 with a volume slide), 9 sample offset,
;;;; A volume slide, B position jump, C volume, D pattern break, F speed
;;;; (below 32) and tempo.  Sample loops, and finetune: a sample's finetune
;;;; of n eighths of a semitone multiplies its periods by 2^(-n/96), where
;;;; ProTracker has a table of periods for each setting -- the same to
;;;; within a count of the period.
;;;; Not done, and ignored when met: 7 (tremolo), 8, and all the E effects
;;;; (fine slides, pattern loop, retrigger, note cut and delay, the filter).
;;;; Slides stop at the periods of C-1 and B-3, as ProTracker's do.  Modules
;;;; with more than four channels, or of the older 15-sample kind (which
;;;; have no letters at 1080), are refused.
;;;;
;;;; Is Lisp fast enough?  Before it plays, the program mixes the song for a
;;;; fifth of a second by the BIOS's clock and counts the samples made: the
;;;; bottom line shows what share of the time 22050 a second then takes (a
;;;; few per cent under QEMU on a fast host), and if it were more than half,
;;;; the song would be played at 11025 instead.  "Late" counts the times
;;;; two interrupts had arrived when Lisp looked: a block not made in time.
;;;; The row shown is the one being mixed, which the card plays up to two
;;;; blocks (0.09 s) later.
;;;;
;;;; Without a file the song is made here: five samples computed, four
;;;; patterns written as lists, the whole put together as the bytes of a
;;;; real "M.K." module and then read by the same parser a file goes
;;;; through.  The module that comes with DOSLIB, examples/data/song.mod, is
;;;; made in the same spirit but somewhere else: by a Python program on the
;;;; machine DOSLIB is built on, examples/support/make-mod.py, which does
;;;; not share a line with this file.  So it is a check of the parser:
;;;; "info" (DESCRIBE-MOD) prints the title, the orders, each sample's
;;;; header and a checksum of its data, the first rows of the patterns and
;;;; a checksum of them all, and how long the song takes to play through
;;;; once; make-mod.py --describe prints the same from its own reading of
;;;; the file, and the two should agree to the letter.  When playing ends,
;;;; the row the mixer had reached is printed with the number of samples
;;;; made, and make-mod.py --at that number says where it should have been.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.modplay
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main #:write-song #:built-in-song #:parse-mod #:read-mod #:find-mod
           #:describe-mod))
(in-package #:doslib-examples.modplay)

;;; ---- a module in memory
(deftype sample-data () '(simple-array (signed-byte 8) (*)))
(deftype bytes () '(simple-array (unsigned-byte 8) (*)))

(defstruct (instrument (:conc-name ins-))
  (name "" :type simple-string)
  (data (make-array 0 :element-type '(signed-byte 8)) :type sample-data)
  (finetune 0 :type fixnum)             ; 0 to 15: eighths of a semitone, 8 to 15 downwards
  (volume 0 :type fixnum)               ; 0 to 64
  (loop-start 0 :type fixnum)
  (loop-length 0 :type fixnum))         ; 0: played once

(defstruct song
  (title "" :type simple-string)
  (instruments (vector) :type simple-vector)    ; 32 of them; number 0 is silence
  (orders (make-array 0 :element-type '(unsigned-byte 8)) :type bytes)
  (patterns (make-array 0 :element-type '(unsigned-byte 8)) :type bytes))   ; 4 bytes a cell

(defun text-of (bytes start length)
  "The text in a field of a module: up to the first zero, printable characters only."
  (let* ((end (or (position 0 bytes :start start :end (+ start length)) (+ start length)))
         (text (make-string (- end start))))
    (dotimes (i (length text) (string-right-trim " " text))
      (let ((code (aref bytes (+ start i))))
        (setf (char text i) (if (< 31 code 127) (code-char code) #\Space))))))

(defun parse-mod (bytes)
  "The SONG in BYTES, the contents of a 31-sample, four-channel module file."
  (declare (type bytes bytes))
  (flet ((word (i) (+ (ash (aref bytes i) 8) (aref bytes (1+ i)))))     ; big end first
    (unless (and (>= (length bytes) 1084)
                 (member (text-of bytes 1080 4) '("M.K." "M!K!" "FLT4" "4CHN") :test #'string=))
      (error "not a four-channel ProTracker module (no M.K. at byte 1080)"))
    (let* ((length (max 1 (min 128 (aref bytes 950))))
           (patterns (1+ (reduce #'max bytes :start 952 :end 1080)))
           (at (min (length bytes) (+ 1084 (* patterns 1024))))
           (instruments (make-array 32)))
      (setf (aref instruments 0) (make-instrument))
      (loop for number from 1 to 31
            for header = (+ 20 (* 30 (1- number)))
            do (let* ((size (min (* 2 (word (+ header 22))) (- (length bytes) at)))
                      (data (make-array size :element-type '(signed-byte 8)))
                      (loop-start (* 2 (word (+ header 26))))
                      (loop-length (* 2 (word (+ header 28)))))
                 (dotimes (i size)
                   (let ((byte (aref bytes (+ at i))))
                     (setf (aref data i) (if (> byte 127) (- byte 256) byte))))
                 (incf at size)
                 (when (or (<= loop-length 2) (>= loop-start size)) (setf loop-length 0))
                 (setf (aref instruments number)
                       (make-instrument :name (text-of bytes header 22) :data data
                                        :finetune (logand (aref bytes (+ header 24)) 15)
                                        :volume (min 64 (aref bytes (+ header 25)))
                                        :loop-start loop-start
                                        :loop-length (min loop-length (- size loop-start))))))
      (make-song :title (text-of bytes 0 20)
                 :instruments instruments
                 :orders (subseq bytes 952 (+ 952 length))
                 :patterns (let ((cells (make-array (* patterns 1024)
                                                    :element-type '(unsigned-byte 8)
                                                    :initial-element 0)))
                             (replace cells bytes :start2 1084)
                             cells)))))

(defun find-mod (file)
  "FILE if there is such a file, or the file of that name among the examples'
data (SONG.MOD is there), or NIL."
  (let ((file (string file)))
    (cond ((probe-file file) file)
          ((ignore-errors (probe-file (doslib-examples:data-file file)))
           (doslib-examples:data-file file)))))

(defun read-mod (file)
  "The SONG in the module FILE."
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      (parse-mod bytes))))

;;; ---- pitch
(defparameter *periods*
  (coerce '(856 808 762 720 678 640 604 570 538 508 480 453     ; C-1 to B-1
            428 404 381 360 339 320 302 285 269 254 240 226     ; C-2 to B-2
            214 202 190 180 170 160 151 143 135 127 120 113)    ; C-3 to B-3
          '(simple-array fixnum (*)))
  "The period of each note of ProTracker's three octaves.")
(declaim (type (simple-array fixnum (*)) *periods* *semitones* *finetunes* *sine*))

(defparameter *semitones*
  (coerce (loop for n below 16 collect (round (* 65536 (expt 2 (/ n -12.0)))))
          '(simple-array fixnum (*)))
  "65536 times what a period is multiplied by to go up N semitones.")
(defparameter *finetunes*
  (coerce (loop for n below 16
                collect (round (* 65536 (expt 2 (/ (if (> n 7) (- n 16) n) -96.0)))))
          '(simple-array fixnum (*)))
  "65536 times what a period is multiplied by for each finetune setting.")
(defparameter *sine*
  (coerce (loop for n below 32 collect (round (* 255 (sin (/ (* pi n) 32)))))
          '(simple-array fixnum (*)))
  "Half a wave, for vibrato.")

;;; ---- the player: what each channel is doing, and where the song is
(defstruct (channel (:conc-name ch-))
  ;; what the mixer reads
  (data (make-array 0 :element-type '(signed-byte 8)) :type sample-data)
  (active nil)
  (position 0 :type fixnum)             ; where in DATA
  (fraction 0 :type fixnum)             ; and how far to the next sample, in 65536ths
  (step 0 :type fixnum)                 ; samples of DATA to an output sample, times 65536
  (end 0 :type fixnum)                  ; stop, or go back, here
  (loop-length 0 :type fixnum)
  (volume 0 :type fixnum)
  (level 0 :type fixnum)                ; the loudest it has been since the screen was drawn
  ;; what the song has told it
  (instrument 0 :type fixnum)
  (period 0 :type fixnum)
  (effect 0 :type fixnum)
  (parameter 0 :type fixnum)
  (target 0 :type fixnum)               ; effect 3: the period to slide to
  (slide 0 :type fixnum)                ; and how fast
  (vibrato 0 :type fixnum)              ; effect 4: where in the wave
  (vibrato-speed 0 :type fixnum)
  (vibrato-depth 0 :type fixnum)
  (offset 0 :type fixnum))              ; effect 9: the last one given

(defstruct (player (:conc-name pl-))
  (song (make-song) :type song)
  (channels (vector) :type simple-vector)
  (mix (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (rate 22050 :type fixnum)
  (pitch 0 :type fixnum)                ; 3546895 * 2^20 / rate: divide by a period for a step
  (speed 6 :type fixnum)                ; ticks to a row
  (tempo 125 :type fixnum)              ; a tick is 2.5 / tempo seconds
  (tick 0 :type fixnum)
  (row 0 :type fixnum)
  (order 0 :type fixnum)
  (next-order -1 :type fixnum)          ; effects B and D: where to go after this row
  (next-row 0 :type fixnum)
  (left 0 :type fixnum)                 ; output samples left of this tick
  (remainder 0 :type fixnum)            ; ticks are not whole numbers of samples
  (mixed 0 :type fixnum)                ; output samples made so far
  (rows 0 :type fixnum)                 ; rows begun so far
  (repeats 0 :type fixnum))             ; times the song has gone back to its start

(defun new-player (song rate)
  (make-player :song song :rate rate
               :pitch (round (* 3546895 (expt 2 20)) rate)
               :channels (coerce (loop repeat 4 collect (make-channel)) 'simple-vector)
               :mix (make-array 4096 :element-type 'fixnum :initial-element 0)))

(defun set-step (player channel period)
  "Have CHANNEL play at PERIOD from now on."
  (declare (type player player) (type channel channel) (fixnum period))
  (setf (ch-step channel)
        (if (plusp period)
            (min #xFFFFF (ash (truncate (pl-pitch player) period) -4))
            0)))

(defun start-row (player)
  "The first tick of a row: read its four cells; start notes; do the effects that happen once."
  (declare (type player player))
  (let* ((song (pl-song player))
         (cells (song-patterns song))
         (instruments (song-instruments song))
         (pattern (aref (song-orders song) (pl-order player)))
         (base (* 16 (+ (pl-row player) (* 64 pattern)))))
    (declare (fixnum base))
    (setf (pl-next-order player) -1 (pl-next-row player) 0)
    (dotimes (c 4)
      (let* ((channel (svref (pl-channels player) c))
             (at (+ base (* 4 c)))
             (b0 (aref cells at)) (b1 (aref cells (+ at 1)))
             (b2 (aref cells (+ at 2))) (b3 (aref cells (+ at 3)))
             (number (logior (logand b0 #xF0) (ash b2 -4)))
             (period (logior (ash (logand b0 #x0F) 8) b1))
             (effect (logand b2 #x0F))
             (started nil))
        (declare (type channel channel) (fixnum at number period effect))
        (setf (ch-effect channel) effect (ch-parameter channel) b3)
        (when (<= 1 number 31)
          (setf (ch-instrument channel) number
                (ch-volume channel) (ins-volume (svref instruments number))))
        (when (plusp period)
          (let ((instrument (svref instruments (ch-instrument channel))))
            (declare (type instrument instrument))
            (setf period (ash (* period (aref *finetunes* (ins-finetune instrument))) -16))
            (cond ((or (= effect 3) (= effect 5))       ; slide to this note: no new start
                   (setf (ch-target channel) period))
                  (t
                   (let* ((loop-length (ins-loop-length instrument))
                          (end (if (plusp loop-length)
                                   (+ (ins-loop-start instrument) loop-length)
                                   (length (ins-data instrument)))))
                     (setf (ch-data channel) (ins-data instrument)
                           (ch-position channel) 0 (ch-fraction channel) 0
                           (ch-end channel) end (ch-loop-length channel) loop-length
                           (ch-active channel) (plusp end)
                           (ch-period channel) period (ch-vibrato channel) 0
                           started t))))))
        (case effect
          (3 (when (plusp b3) (setf (ch-slide channel) b3)))
          (4 (when (plusp (ash b3 -4)) (setf (ch-vibrato-speed channel) (ash b3 -4)))
           (when (plusp (logand b3 15)) (setf (ch-vibrato-depth channel) (logand b3 15))))
          (9 (when (plusp b3) (setf (ch-offset channel) b3))
           (when started
             (let ((position (* 256 (ch-offset channel))))
               (if (< position (ch-end channel))
                   (setf (ch-position channel) position)
                   (setf (ch-active channel) nil)))))
          (11 (setf (pl-next-order player) b3 (pl-next-row player) 0))
          (12 (setf (ch-volume channel) (min 64 b3)))
          (13 (let ((row (+ (* 10 (ash b3 -4)) (logand b3 15))))
                (when (minusp (pl-next-order player))
                  (setf (pl-next-order player) (1+ (pl-order player))))
                (setf (pl-next-row player) (if (< row 64) row 0))))
          (15 (cond ((zerop b3))
                    ((< b3 32) (setf (pl-speed player) b3))
                    (t (setf (pl-tempo player) b3)))))
        (set-step player channel (ch-period channel))))))

(defun continue-row (player)
  "The other ticks of a row: the effects that slide."
  (declare (type player player))
  (dotimes (c 4)
    (let* ((channel (svref (pl-channels player) c))
           (effect (ch-effect channel))
           (parameter (ch-parameter channel))
           (period (ch-period channel))
           (heard period))              ; arpeggio and vibrato do not change the note itself
      (declare (type channel channel) (fixnum effect parameter period heard))
      (case effect
        (0 (when (plusp parameter)
             (let ((up (case (mod (pl-tick player) 3)
                         (0 0) (1 (ash parameter -4)) (t (logand parameter 15)))))
               (setf heard (ash (* period (aref *semitones* up)) -16)))))
        (1 (setf period (max 113 (- period parameter)) heard period))
        (2 (setf period (min 856 (+ period parameter)) heard period)))
      (when (and (or (= effect 3) (= effect 5)) (plusp (ch-target channel)) (plusp period))
        (let ((target (ch-target channel)))
          (setf period (if (< period target)
                           (min target (+ period (ch-slide channel)))
                           (max target (- period (ch-slide channel))))
                heard period)))
      (when (or (= effect 4) (= effect 6))
        (let* ((place (ch-vibrato channel))
               (delta (ash (* (aref *sine* (logand place 31)) (ch-vibrato-depth channel)) -7)))
          (setf heard (if (logbitp 5 place) (- period delta) (+ period delta))
                (ch-vibrato channel) (logand 63 (+ place (ch-vibrato-speed channel))))))
      (when (or (= effect 10) (= effect 5) (= effect 6))
        (setf (ch-volume channel)
              (if (plusp (ash parameter -4))
                  (min 64 (+ (ch-volume channel) (ash parameter -4)))
                  (max 0 (- (ch-volume channel) (logand parameter 15))))))
      (setf (ch-period channel) period)
      (set-step player channel heard))))

(defun next-tick (player)
  "Move the song on by a tick.  Returns the number of output samples the tick lasts."
  (declare (type player player))
  (when (>= (pl-tick player) (pl-speed player))         ; the row is over: which is next?
    (setf (pl-tick player) 0)
    (let ((order (pl-order player)) (row (1+ (pl-row player)))
          (length (length (song-orders (pl-song player)))))
      (declare (fixnum order row length))
      (cond ((>= (pl-next-order player) 0)
             (when (<= (pl-next-order player) order) (incf (pl-repeats player)))
             (setf order (pl-next-order player) row (pl-next-row player)))
            ((>= row 64) (setf row 0) (incf order)))
      (when (>= order length)
        (setf order 0) (incf (pl-repeats player)))
      (setf (pl-order player) order (pl-row player) row)))
  (cond ((zerop (pl-tick player)) (incf (pl-rows player)) (start-row player))
        (t (continue-row player)))
  (incf (pl-tick player))
  ;; rate * 2.5 / tempo samples, the fraction carried to the next tick
  (let ((total (+ (pl-remainder player) (* 5 (pl-rate player))))
        (divisor (* 2 (pl-tempo player))))
    (declare (fixnum total divisor))
    (multiple-value-bind (samples remainder) (truncate total divisor)
      (setf (pl-remainder player) remainder)
      (max 1 samples))))

;;; ---- what is in a module, in words: a check of the parser and of the
;;; player's way through the song
(defun data-checksum (data)
  "A number that depends on every byte of DATA and on where it is."
  (let ((sum 0))
    (declare (fixnum sum))
    (dotimes (i (length data) sum)
      (setf sum (mod (+ sum (* (1+ i) (logand (aref data i) 255))) 65521)))))

(defun once-through (song)
  "Play SONG without making a sound until it goes back -- a jump to an order
that is not after the one it is in, or the end of the orders.  Returns the
rows and the ticks that took, and the time in seconds, a rational."
  (let ((player (new-player song 22050)) (rows 0) (ticks 0) (time 0))
    (loop (next-tick player)
          (when (plusp (pl-repeats player)) (return (values rows ticks time)))
          (setf rows (pl-rows player))
          (incf ticks)
          (incf time (/ 5 (* 2 (pl-tempo player)))))))

(defun describe-mod (song &optional (bytes 0) (stream *standard-output*))
  "Print what is in SONG (or the module file of that name; BYTES is the
file's size): title, orders, a checksum of the patterns, the samples with a
checksum of each one's data, the first four rows of the first three patterns
in numbers -- period, sample, effect -- and what playing it through once
comes to.  examples/support/make-mod.py
--describe prints the same of the same file."
  (when (or (stringp song) (pathnamep song))
    (let ((file (find-mod song)))
      (with-open-file (in file :element-type '(unsigned-byte 8))
        (setf bytes (file-length in)))
      (setf song (read-mod file))))
  (let* ((orders (song-orders song)) (cells (song-patterns song))
         (patterns (floor (length cells) 1024)))
    (format stream "~&Title ~S, ~D bytes~%" (song-title song) bytes)
    (format stream "~D orders: ~{~D~^ ~}~%" (length orders) (coerce orders 'list))
    (format stream "~D patterns, checksum ~D~%" patterns (data-checksum cells))
    (loop for number from 1 to 31
          for instrument = (svref (song-instruments song) number)
          when (plusp (length (ins-data instrument)))
            do (format stream "Sample ~2D ~S: ~D bytes, tune ~D, volume ~D, loop ~D+~D, ~
                               sum ~D~%"
                       number (ins-name instrument) (length (ins-data instrument))
                       (ins-finetune instrument) (ins-volume instrument)
                       (ins-loop-start instrument) (ins-loop-length instrument)
                       (data-checksum (ins-data instrument))))
    (dotimes (pattern (min 3 patterns))
      (dotimes (row 4)
        (format stream "Pattern ~D row ~D:" pattern row)
        (dotimes (c 4)
          (let* ((at (+ (* 1024 pattern) (* 16 row) (* 4 c)))
                 (b0 (aref cells at)) (b2 (aref cells (+ at 2))))
            (format stream "~:[ |~;~] ~3D ~2,'0D ~X~2,'0X" (zerop c)
                    (logior (ash (logand b0 15) 8) (aref cells (+ at 1)))
                    (logior (logand b0 #xF0) (ash b2 -4))
                    (logand b2 15) (aref cells (+ at 3)))))
        (terpri stream)))
    (multiple-value-bind (rows ticks time) (once-through song)
      (format stream "Once through: ~D rows, ~D ticks, ~D ms~%" rows ticks (floor (* time 1000)))))
  (values))

;;; ---- the mixer
(defun mix-channel (channel mix start count)
  "Add COUNT samples of CHANNEL to MIX from START on, and move CHANNEL along."
  (declare (type channel channel) (type (simple-array fixnum (*)) mix)
           (type (integer 0 65535) start count)
           (optimize speed (safety 0)))
  (when (and (ch-active channel) (plusp (ch-step channel)))
    (let ((data (ch-data channel))
          (position (ch-position channel))
          (fraction (ch-fraction channel))
          (step (ch-step channel))
          (volume (ch-volume channel))
          (end (ch-end channel))
          (loop-length (ch-loop-length channel))
          (peak (ch-level channel)))
      (declare (type (integer 0 #xFFFFF) position end loop-length step)
               (type (integer 0 #x1FFFFF) fraction)
               (type (integer 0 64) volume)
               (type (integer 0 #xFFFF) peak))
      (loop for i of-type fixnum from start below (+ start count)
            do (let ((sample (* (aref data position) volume)))
                 (declare (type (integer -8192 8192) sample))
                 (incf (aref mix i) sample)
                 (when (> (abs sample) peak) (setf peak (abs sample)))
                 (setf fraction (+ fraction step)
                       position (+ position (ash fraction -16))
                       fraction (logand fraction #xFFFF))
                 (when (>= position end)
                   (cond ((plusp loop-length)
                          (loop do (setf position (- position loop-length))
                                while (>= position end)))
                         (t (setf (ch-active channel) nil position 0)
                            (return))))))
      (setf (ch-position channel) position
            (ch-fraction channel) fraction
            (ch-level channel) peak)))
  nil)

(defun mix-block (player buffer count)
  "Make the next COUNT samples of the song and store them, as bytes, at BUFFER."
  (declare (type player player) (system-area-pointer buffer) (type (integer 0 4096) count)
           (optimize speed))
  (let ((mix (pl-mix player)) (done 0))
    (declare (type (integer 0 4096) done))
    (loop while (< done count)
          do (when (zerop (pl-left player))
               (setf (pl-left player) (next-tick player)))
             (let ((now (min (pl-left player) (- count done))))
               (declare (type (integer 0 4096) now))
               (dotimes (c 4)
                 (mix-channel (svref (pl-channels player) c) mix done now))
               (incf done now)
               (decf (pl-left player) now)))
    ;; four channels of up to 64 * 128 each: divide by 128 and clip what is
    ;; left over, which leaves two channels at full volume just unclipped
    (dotimes (i count)
      (let ((value (+ 128 (ash (aref mix i) -7))))
        (declare (fixnum value))
        (setf (sap-ref-8 buffer i) (cond ((< value 0) 0) ((> value 255) 255) (t value))
              (aref mix i) 0)))
    (incf (pl-mixed player) count)
    nil))

;;; ---- the song that is built in, as the bytes of a module file
(defun note-period (name)
  "The period of a note written like \"C-2\" or \"F#1\"."
  (aref *periods* (+ (* 12 (- (digit-char-p (char name 2)) 1))
                     (position (subseq name 0 2)
                               '("C-" "C#" "D-" "D#" "E-" "F-" "F#" "G-" "G#" "A-" "A#" "B-")
                               :test #'string=))))

(defun built-in-samples ()
  "Five samples, computed: (name volume loop-length data) each."
  (let ((seed 12345))
    (flet ((noise () (setf seed (logand (+ (* seed 1103515245) 12345) #x7FFFFFFF))
             (- (ldb (byte 8 16) seed) 128))
           (make (length function)
             (let ((data (make-array length :element-type '(signed-byte 8))))
               (dotimes (i length data)
                 (setf (aref data i) (max -128 (min 127 (round (funcall function i)))))))))
      (list
       ;; one wave each, 32 and 64 samples long, played round and round
       (list "square lead" 40 32 (make 32 (lambda (i) (if (< i 16) 100 -100))))
       (list "saw bass" 64 64 (make 64 (lambda (i) (- (* 4 i) 126))))
       ;; a sine falling in pitch and dying away
       (list "kick" 64 0
             (make 1600 (lambda (i) (* 125 (exp (/ i -400.0))
                                       (sin (* 60 (- 1 (exp (/ i -300.0)))))))))
       ;; noise dying away, fast and slower
       (list "noise hat" 40 0 (make 900 (lambda (i) (* (noise) (exp (/ i -150.0))))))
       (list "snare" 56 0
             (make 2400 (lambda (i) (* (exp (/ i -500.0))
                                       (+ (* 0.7 (noise)) (* 50 (sin (/ i 5.0))))))))))))

(defparameter *chords*
  ;; bass note, chord's note, arpeggio (semitones up, twice)
  '(("A-1" "A-2" #x37) ("F-1" "F-2" #x47) ("C-2" "C-3" #x47) ("G-1" "G-2" #x47)))

(defun built-in-patterns ()
  "The song's four patterns: a list for each of (row channel note sample effect parameter)."
  (let ((patterns (make-array 4 :initial-element '())))
    (flet ((cell (pattern row channel note sample &optional (effect 0) (parameter 0))
             (push (list row channel note sample effect parameter) (aref patterns pattern))))
      (dotimes (pattern 2)
        ;; channel 0: drums; the first row also sets the speed (and channel 1 the tempo)
        (loop for row from 0 below 64 by 8
              do (if (and (= pattern 1) (oddp (floor row 8)))
                     (cell pattern row 0 "C-2" 5)
                     (cell pattern row 0 "C-2" 3 (if (zerop row) 15 0) (if (zerop row) 6 0))))
        ;; channel 1: hats, loud off the beat, quiet and started part way in between
        (loop for row from 0 below 64 by 2
              do (cond ((zerop row) (cell pattern row 1 "C-3" 4 15 125))
                       ((= (mod row 8) 4) (cell pattern row 1 "C-3" 4 12 48))
                       (t (cell pattern row 1 "C-3" 4 9 1))))
        ;; channel 2: the bass, each note fading
        (loop for (bass) in *chords* for start from 0 by 16
              do (loop for (row up) in '((0 0) (4 0) (6 12) (8 0) (12 0) (14 7))
                       do (cell pattern (+ start row) 2
                                (let ((index (+ up (position (note-period bass) *periods*))))
                                  (format nil "~A~D"
                                          (nth (mod index 12) '("C-" "C#" "D-" "D#" "E-" "F-"
                                                                "F#" "G-" "G#" "A-" "A#" "B-"))
                                          (1+ (floor index 12))))
                                2)
                          (cell pattern (+ start row 1) 2 nil 0 10 #x06))))
      ;; channel 3 of pattern 0: a chord to each bar, as an arpeggio
      (loop for (nil note arpeggio) in *chords* for start from 0 by 16
            do (cell 0 start 3 note 1 0 arpeggio)
               (loop for row from 1 below 16
                     do (cell 0 (+ start row) 3 nil 0 0 arpeggio)))
      ;; channel 3 of pattern 1: a tune, with slides
      (loop for (row note effect parameter)
              in '((0 "E-3" 12 48) (4 "D-3") (6 "C-3") (8 "A-2") (10 nil 10 #x04) (11 nil 10 #x04)
                   (12 "C-3" 12 40) (14 nil 1 8) (15 nil 1 8)
                   (16 "C-3" 12 48) (20 "A-2") (22 "F-2") (24 "A-2") (26 "C-3" 3 12)
                   (27 nil 3 0) (28 nil 3 0) (29 nil 4 #x46) (30 nil 4 0) (31 nil 4 0)
                   (32 "G-3" 12 48) (36 "E-3") (38 "C-3") (40 "E-3") (42 nil 2 6) (43 nil 2 6)
                   (44 "D-3") (46 nil 10 #x08)
                   (48 "D-3" 12 48) (50 "B-2") (52 "G-2") (54 "B-2") (56 "D-3" 12 40)
                   (58 nil 10 #x03) (59 nil 10 #x03) (60 nil 10 #x03) (61 nil 10 #x03)
                   (62 nil 10 #x03) (63 nil 10 #x03))
            do (cell 1 row 3 note (if note 1 0) (or effect 0) (or parameter 0)))
      ;; pattern 2: drums and bass alone for 32 rows, then a pattern break
      (loop for row from 0 below 32 by 4
            do (cell 2 row 0 "C-2" (if (= (mod row 8) 4) 5 3))
               (cell 2 row 1 "C-3" 4 12 40)
               (cell 2 (+ row 2) 1 "C-3" 4 9 2)
               (cell 2 row 2 (if (< row 16) "A-1" "E-1") 2)
               (cell 2 (+ row 3) 2 (if (< row 16) "A-2" "E-2") 2 12 32))
      (cell 2 31 0 nil 0 13 0)
      ;; pattern 3: sixteen rows at twice the speed, a roll getting louder
      ;; and the bass sliding up, then a jump back to the beginning
      (dotimes (row 16)
        (cell 3 row 0 "C-2" 5 12 (+ 16 (* 3 row)))
        (if (zerop row)
            (cell 3 row 2 "A-1" 2 15 3)
            (cell 3 row 2 nil 0 1 12)))
      (cell 3 15 1 nil 0 11 0))
    patterns))

(defun built-in-song ()
  "A small song as the bytes of a ProTracker module, as they would be in a file."
  (let* ((samples (built-in-samples))
         (patterns (built-in-patterns))
         (orders '(0 1 0 1 2 3))
         (size (+ 1084 (* 1024 (length patterns))
                  (reduce #'+ samples :key (lambda (sample) (length (fourth sample))))))
         (bytes (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
         (at (+ 1084 (* 1024 (length patterns)))))
    (flet ((text (start string)
             (loop for c across string for i from start
                   do (setf (aref bytes i) (char-code c))))
           (word (i value)              ; big end first, and counted in pairs of bytes
             (setf (aref bytes i) (ldb (byte 8 9) value)
                   (aref bytes (1+ i)) (ldb (byte 8 1) value))))
      (text 0 "doslib song")
      (dotimes (number 31)              ; thirty bytes for each of 31 samples
        (let ((header (+ 20 (* 30 number))))
          (word (+ header 28) 2)        ; "no loop" is a loop of one word
          (when (< number (length samples))
            (destructuring-bind (name volume loop-length data) (nth number samples)
              (text header name)
              (word (+ header 22) (length data))
              (setf (aref bytes (+ header 25)) volume)
              (when (plusp loop-length) (word (+ header 28) loop-length))
              (dotimes (i (length data))
                (setf (aref bytes (+ at i)) (logand (aref data i) 255)))
              (incf at (length data))))))
      (setf (aref bytes 950) (length orders)
            (aref bytes 951) 127)
      (replace bytes orders :start1 952)
      (text 1080 "M.K.")
      (dotimes (pattern (length patterns))
        (loop for (row channel note sample effect parameter) in (aref patterns pattern)
              do (let ((cell (+ 1084 (* 1024 pattern) (* 16 row) (* 4 channel)))
                       (period (if note (note-period note) 0)))
                   (setf (aref bytes cell) (logior (logand sample #xF0) (ash period -8))
                         (aref bytes (+ cell 1)) (logand period 255)
                         (aref bytes (+ cell 2)) (logior (ash (logand sample 15) 4) effect)
                         (aref bytes (+ cell 3)) parameter)))))
    bytes))

(defun write-song (&optional (file "SONG.MOD"))
  "Write the built-in song to FILE, as a module any tracker or player can read."
  (let ((bytes (built-in-song)))
    (with-open-file (out file :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence bytes out))
    (format t "~&~A: ~:D bytes.~%" file (length bytes))
    file))

;;; ---- the screen: straight into text-mode memory, so that nothing is allocated
(defun put-char (x y code attribute)
  (declare (fixnum x y code attribute))
  (when (and (<= 0 x 79) (<= 0 y 24))
    (setf (sap-ref-16 (int-sap #xB8000) (* 2 (+ x (* 80 y)))) (logior code (ash attribute 8)))))
(defun put-string (x y string attribute)
  (declare (simple-string string) (fixnum x))
  (dotimes (i (length string))
    (put-char (+ x i) y (logand (char-code (char string i)) 255) attribute)))
(defun put-number (x y number width attribute &optional (radix 10) (pad 32))
  "NUMBER in WIDTH columns ending before X + WIDTH, padded on the left."
  (declare (fixnum x number width radix))
  (loop for column of-type fixnum from (+ x width -1) downto x
        for first = t then nil
        do (if (or first (plusp number))
               (multiple-value-bind (rest digit) (truncate number radix)
                 (put-char column y (char-code (char "0123456789ABCDEF" digit)) attribute)
                 (setf number rest))
               (put-char column y pad attribute))))

(defun put-cell (x y cells at attribute)
  "A cell of a pattern as a tracker shows it: note, sample, effect.  C-2 04 C30"
  (declare (type bytes cells) (fixnum x at))
  (let* ((b0 (aref cells at)) (b1 (aref cells (+ at 1)))
         (b2 (aref cells (+ at 2))) (b3 (aref cells (+ at 3)))
         (number (logior (logand b0 #xF0) (ash b2 -4)))
         (period (logior (ash (logand b0 #x0F) 8) b1))
         (effect (logand b2 #x0F)))
    (if (zerop period)
        (put-string x y "---" attribute)
        (let ((note (or (position period *periods* :test #'>=) 35)))
          (put-string x y (svref #("C-" "C#" "D-" "D#" "E-" "F-" "F#" "G-" "G#" "A-" "A#" "B-")
                                 (mod note 12))
                      attribute)
          (put-number (+ x 2) y (1+ (floor note 12)) 1 attribute)))
    (if (zerop number)
        (put-string (+ x 4) y ".." attribute)
        (put-number (+ x 4) y number 2 attribute 10 48))
    (if (and (zerop effect) (zerop b3))
        (put-string (+ x 7) y "..." attribute)
        (progn (put-number (+ x 7) y effect 1 attribute 16)
               (put-number (+ x 8) y b3 2 attribute 16 48)))))

(defconstant +rows-shown+ 13)

(defun draw-frame (song rate file)
  (text-fill 0 0 80 25 7)
  (text-fill 0 0 80 1 #x1F)
  (put-string 1 0 "MOD player" #x1E)
  (put-string 13 0 (if (plusp (length (song-title song))) (song-title song) "(no title)") #x1F)
  (when file (put-string 36 0 (coerce (string file) 'simple-string) #x17))
  (put-number 58 0 rate 5 #x1F)
  (put-string 64 0 "a second, mono" #x1F)
  (put-string 1 2 "Order" 7) (put-string 11 2 "of" 7) (put-string 20 2 "Pattern" 7)
  (put-string 34 2 "Row" 7) (put-string 43 2 "Speed" 7) (put-string 54 2 "Tempo" 7)
  (dotimes (c 4)
    (put-string (+ 6 (* 13 c)) 4 "Channel" 3)
    (put-number (+ 14 (* 13 c)) 4 (1+ c) 1 3)
    (put-number 1 (+ 20 c) (1+ c) 1 3))
  (put-string 60 4 "Samples" 3)
  (let ((instruments (song-instruments song)))
    (loop for number from 1 to 31
          for instrument = (svref instruments number)
          with line = 5
          when (and (plusp (length (ins-data instrument))) (< line 19))
            do (put-number 60 line number 2 7 10 48)
               (put-string 63 line (subseq (ins-name instrument)
                                           0 (min 16 (length (ins-name instrument))))
                           7)
               (incf line)))
  (put-string 1 24 "Esc ends" 8))

;;; A scope for each channel, in text: the wave the channel is playing, from
;;; where it has got to, nineteen samples across and three rows high.  A row
;;; of text shows two heights with the half-block characters, so there are
;;; six; the wave is scaled by the channel's volume, as the mixer scales it.
(defun draw-scopes (player)
  (declare (type player player))
  (dotimes (c 4)
    (let* ((channel (svref (pl-channels player) c))
           (data (ch-data channel))
           (limit (min (ch-end channel) (length data)))
           (stride (max 1 (ash (ch-step channel) -14)))         ; four output samples a column
           (top (+ 5 (* 3 c))))
      (declare (type channel channel) (fixnum limit stride top))
      (dotimes (i 19)
        (let* ((at (+ (ch-position channel) (* i stride)))
               (sample (if (and (ch-active channel) (< at limit)) (aref data at) 0))
               (level (- 5 (max 0 (min 5 (floor (+ (* sample (ch-volume channel)) 8192) 2731))))))
          (declare (fixnum at sample level))
          (dotimes (row 3)
            (put-char (+ 60 i) (+ top row)
                      (cond ((= level (* 2 row)) 223) ((= level (1+ (* 2 row))) 220) (t 32))
                      (if (evenp c) 11 10))))))))

(defun draw-state (player seconds late busy)
  "Where the song is, the rows around it, and how loud each channel has been."
  (declare (type player player) (fixnum seconds late busy))
  (let* ((song (pl-song player))
         (cells (song-patterns song))
         (order (pl-order player)) (row (pl-row player))
         (pattern (aref (song-orders song) order)))
    (put-number 7 2 order 3 15) (put-number 14 2 (length (song-orders song)) 3 15)
    (put-number 28 2 pattern 3 15) (put-number 38 2 row 2 15 10 48)
    (put-number 49 2 (pl-speed player) 2 15) (put-number 60 2 (pl-tempo player) 3 15)
    (dotimes (line +rows-shown+)
      (let* ((shown (+ row line (- (floor +rows-shown+ 2))))
             (attribute (cond ((= shown row) #x70) ((zerop (mod shown 4)) 7) (t 8)))
             (y (+ 5 line)))
        (dotimes (x 58) (put-char x y 32 attribute))
        (when (<= 0 shown 63)
          (put-number 1 y shown 2 attribute 10 48)
          (dotimes (c 4)
            (put-cell (+ 6 (* 13 c)) y cells (+ (* 4 c) (* 16 (+ shown (* 64 pattern))))
                      attribute)))))
    (dotimes (c 4)
      (let* ((channel (svref (pl-channels player) c))
             (bar (min 32 (ash (ch-level channel) -8)))
             (y (+ 20 c)))
        (declare (type channel channel))
        (dotimes (i 32)
          (put-char (+ 3 i) y (if (< i bar) 254 250) (cond ((>= i bar) 8) ((< i 24) 10) (t 14))))
        (put-string 37 y "sample" 8)
        (put-number 44 y (ch-instrument channel) 2 7 10 48)
        (put-string 48 y "volume" 8)
        (put-number 55 y (ch-volume channel) 2 7)
        (setf (ch-level channel) (ash (* 3 (ch-level channel)) -2))))     ; the bars fall back
    (draw-scopes player)
    ;; the time of the song made so far, and the clock's
    (let ((song-tenths (truncate (* 10 (pl-mixed player)) (pl-rate player))))
      (put-string 12 24 "song" 8)
      (put-number 17 24 (floor song-tenths 10) 4 7) (put-char 21 24 46 7)
      (put-number 22 24 (mod song-tenths 10) 1 7) (put-string 24 24 "s  clock" 8)
      (put-number 33 24 seconds 4 7) (put-string 38 24 "s  mixing takes" 8)
      (put-number 54 24 busy 3 7) (put-string 57 24 "% of the time" 8)
      (put-number 72 24 late 3 (if (plusp late) 12 7)) (put-string 76 24 "late" 8))))

;;; ---- the card is the library's (src/blaster.lisp): where it is, commands to its
;;; processor (BLASTER-WRITE), and its interrupt, counted (WITH-BLASTER-INTERRUPT,
;;; BLASTER-INTERRUPTS).  So is the DMA controller: DMA-START, DMA-STOP, DMA-COUNT.

;;; ---- playing
(defconstant +block+ 1024 "Samples between interrupts.")

(defun measure-mixer (song rate buffer)
  "The number of samples of SONG the mixer makes in a second: it is run for four ticks."
  (let ((player (new-player song rate)) (samples 0))
    (wait-tick)
    (loop with start = (ticks)
          while (< (- (ticks) start) 4)
          do (mix-block player buffer +block+)
             (incf samples +block+))
    (round (* samples 18.2) 4)))

(defun play (song rate &optional file)
  "Play SONG until a key is pressed.  Returns interrupts, late blocks, seconds,
bytes allocated meanwhile, seconds of song made, times it began again, the
rate used, the samples a second the mixer can make, and the player (which
knows the samples made, the rows begun and where it stopped); or NIL if
there is no card that can."
  (unless (blaster-present-p)
    (format t "~&No Sound Blaster answered at port ~Xh.  (QEMU: -device sb16; ~
               SET BLASTER=A220 I5 D1 says where a card is.)~%" *blaster-port*)
    (return-from play nil))
  (let ((major (blaster-version)))
    (when (< major 2)
      (format t "~&The card is too old for this: auto-initialized DMA came with DSP version 2.~%")
      (return-from play nil))
    (with-dos-memory (segment (* 4 +block+))
      ;; A DMA transfer cannot cross a 64K boundary of memory: there is room
      ;; here for two blocks on one side or the other of any boundary.
      (let* ((start (dma-fit (linear segment) (* 2 +block+)))
             (capacity (measure-mixer song rate (int-sap start)))
             (rate (if (and (> rate 11025) (< capacity (* 2 rate))) 11025 rate))
             (busy (round (* 100 rate) (max 1 capacity)))
             (player (new-player song rate))
             (handled 0) (late 0) (tick 0)
             (began (ticks)) (allocated 0))
        (declare (fixnum handled late busy tick))
        (with-blaster-interrupt ()
          (unwind-protect
               (progn
                 (hide-text-cursor)
                 (draw-frame song rate file)
                 (mix-block player (int-sap start) +block+)
                 (mix-block player (int-sap (+ start +block+)) +block+)
                 (draw-state player 0 0 busy)
                 (dma-start *blaster-dma* start (* 2 +block+))
                 (blaster-write #xD1)                               ; speaker on
                 (cond ((>= major 4)                                ; the rate itself
                        (blaster-write #x41) (blaster-write (hi rate)) (blaster-write (lo rate)))
                       (t                                           ; or as near as 256 steps go
                        (blaster-write #x40) (blaster-write (- 256 (round 1000000 rate)))))
                 (blaster-write #x48)
                 (blaster-write (lo (1- +block+))) (blaster-write (hi (1- +block+)))
                 (blaster-write #x1C)                               ; go: 8 bits, auto-initialize
                 (setf began (ticks) tick began
                       allocated (get-bytes-consed))
                 ;; An interrupt means a block has been played and is free for
                 ;; new samples.  Which block?  They are played 0, 1, 0, 1 ...,
                 ;; but rather than count on never missing an interrupt, ask
                 ;; the DMA controller where the card is now: the count
                 ;; register goes down from two blocks, less one, to nothing.
                 (loop
                   (let ((raised (blaster-interrupts)))
                     (declare (fixnum raised))
                     (when (/= handled raised)
                       (when (> (- raised handled) 1)   ; two since we looked: too slow
                         (incf late))
                       (setf handled raised)
                       (mix-block player
                                  (int-sap (+ start (if (>= (dma-count *blaster-dma*) +block+)
                                                        +block+
                                                        0)))
                                  +block+)
                       (draw-state player (truncate (* 10 (- (ticks) began)) 182) late busy)))
                   (let ((now (ticks)))                 ; look for a key 18 times a second
                     (declare (fixnum now))
                     (when (/= now tick)
                       (setf tick now)
                       (when (key-pressed-p) (wait-key) (return)))))
                 (setf allocated (- (get-bytes-consed) allocated)))
            ;; pause, end, speaker off
            (blaster-write #xD0) (blaster-write #xDA) (blaster-write #xD3)
            (dma-stop *blaster-dma*)
            (text-fill 0 0 80 25 7) (text-cursor 0 0) (show-text-cursor)))
        (values handled late (/ (- (ticks) began) 18.2) allocated
                (/ (pl-mixed player) (float rate)) (pl-repeats player) rate capacity
                player)))))

(defun main (&optional file (rate 22050))
  "Play a ProTracker module, or the song built in; or, with \"info\" where
the rate goes, say what is in it."
  (let* ((info (and (stringp rate) (string-equal rate "info")))
         (rate (max 8000 (min 22050 (if (integerp rate) rate 22050))))
         (found (and file (find-mod file)))
         (bytes 0)
         (song (handler-case
                   (cond ((null file)
                          (let ((built (built-in-song)))
                            (setf bytes (length built))
                            (parse-mod built)))
                         (found
                          (with-open-file (in found :element-type '(unsigned-byte 8))
                            (setf bytes (file-length in)))
                          (read-mod found))
                         (t (error "there is no such file.")))
                 (error (condition)
                   (format t "~&Cannot play ~A: ~A~%" (or file "the song") condition)
                   (return-from main (values))))))
    (when info
      (describe-mod song bytes)
      (return-from main (values)))
    (multiple-value-bind (interrupts late seconds allocated song-seconds repeats rate capacity
                          player)
        (play song rate file)
      (when interrupts
        (format t "~&~S: ~,1F s of song mixed in ~,1F s at ~D samples a second~
                   ~[~:;, the song begun again ~:*~D time~:P~].~%~
                   ~D interrupts (~,1F a second; ~,1F expected), ~D block~:P late; ~
                   ~:D bytes allocated.~%~
                   ~D samples made, ~D rows begun: the mixer was at order ~D, row ~D.~%~
                   The mixer can make ~:D samples a second: ~,1F times as many.~%"
                (song-title song) song-seconds seconds rate repeats
                interrupts (/ interrupts (max seconds 0.1)) (/ rate (float +block+))
                late allocated
                (pl-mixed player) (pl-rows player) (pl-order player) (pl-row player)
                capacity (/ capacity rate)))))
  (values))

(doslib-examples:define-example "modplay" 'main
  :category :sound
  :description "A MOD player: four channels mixed in Lisp into the Sound Blaster's DMA buffer.")
