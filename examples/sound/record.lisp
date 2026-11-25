;;;; Recording with a Sound Blaster: DMA in the other direction.
;;;;
;;;;   (doslib-examples:run "record")       record three seconds, play them back
;;;;   (doslib-examples:run "record" 10)    or ten
;;;; Esc stops the recording early, and the playing.  Without a card, or with
;;;; one that hands over no samples, it says so and returns.
;;;;
;;;; Playing (see the "blaster" example) has the DMA controller take bytes
;;;; from memory and give them to the card.  Recording is the same machinery
;;;; turned round: the card measures its input 11025 times a second, and
;;;; the DMA controller takes each measurement and stores it in memory.  So
;;;; the channel's mode is 54h + channel ("write" -- to memory -- and
;;;; auto-initialize) where playing has 58h + channel, and the card is given
;;;; a record command where playing gives a play command:
;;;;   DSP version 4 (Sound Blaster 16): 42h, rate high, rate low; then
;;;;       CEh (8 bits, input, auto-initialize), 00h (mono, unsigned),
;;;;       block length - 1 low, high;
;;;;   DSP versions 2 and 3: 40h, time constant; 48h, block length - 1 low,
;;;;       high; 2Ch (8 bits, input, auto-initialize).
;;;; The buffer, the two blocks, the interrupt at the end of each block and
;;;; the handler are the same; Lisp now copies each block out when it is
;;;; full rather than filling it when it is empty.  Which block is full it
;;;; asks the DMA controller: the one its count register is not in.
;;;;
;;;; What is recorded is whatever the card's mixer has switched to its
;;;; input: the microphone, unless something has changed it.  The screen
;;;; shows the last block as a waveform and its loudest sample as a bar;
;;;; then the whole recording, with a marker, while it is played back.
;;;;
;;;; QEMU's sb16 does not record: it accepts the commands and then does
;;;; nothing (no byte is transferred, no interrupt arrives), so there this
;;;; program reports that after a second and a half and returns.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.record
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.record)

;;; The card is the library's (src/blaster.lisp): where it is, commands to its processor
;;; (BLASTER-WRITE), and its interrupt, counted (WITH-BLASTER-INTERRUPT, BLASTER-INTERRUPTS).
;;; So is the DMA controller (src/dma.lisp): DMA-START with :DIRECTION :TO-MEMORY has the
;;; card's bytes written to memory.

(defun call-with-card-interrupt (function)
  "Call FUNCTION with the card's interrupt counted, and afterwards stop the
card and the DMA channel and put the vector and the mask back."
  (with-blaster-interrupt ()
    (unwind-protect (funcall function)
      (blaster-write #xD0) (blaster-write #xDA) (blaster-write #xD3)    ; pause, end, speaker off
      (dma-stop *blaster-dma*))))

;;; ---- drawing: mode 13h; the letters are the library's (GRAPHICS-TEXT)
(defun draw-line-of-text (y colour control &rest arguments)
  "A whole line of the screen: the text, and blanks to the right-hand edge."
  (let ((text (apply #'format nil control arguments)))
    (graphics-text 0 y (format nil "~40A" (subseq text 0 (min 40 (length text)))) colour)))

(defconstant +top+ 20 "The first line of the waveform.")
(defconstant +height+ 96)

(defun draw-wave (sample-at count marker)
  "COUNT samples, (SAMPLE-AT i) each, as 320 columns; column MARKER (if any) white."
  (declare (function sample-at) (fixnum count))
  (let ((screen (int-sap #xA0000)))
    (dotimes (x 320)
      (let ((low 255) (high 0))
        (loop for i from (floor (* x count) 320) to (max (floor (* x count) 320)
                                                         (1- (floor (* (1+ x) count) 320)))
              do (let ((sample (funcall sample-at (min i (1- count)))))
                   (setf low (min low sample) high (max high sample))))
        (let ((y-top (floor (* (- 255 high) +height+) 256))
              (y-bottom (floor (* (- 255 low) +height+) 256)))
          (dotimes (y +height+)
            (setf (sap-ref-8 screen (+ x (* 320 (+ +top+ y))))
                  (cond ((eql x marker) 15)
                        ((<= y-top y y-bottom) 10)
                        ((= y (floor +height+ 2)) 8)
                        (t 1)))))))))

(defun draw-bar (y part whole colour)
  "A bar eight lines high, PART / WHOLE of the screen's width."
  (let ((width (if (plusp whole) (min 320 (floor (* 320 part) whole)) 0)))
    (dotimes (line 8)
      (dotimes (x 320)
        (plot x (+ y line) (if (< x width) colour 8))))))

;;; ---- recording and playing
(defconstant +block+ 1024 "Samples between interrupts.")

(defun buffer-address (segment)
  "Where in the DOS memory at SEGMENT (four blocks long) two blocks lie
without a 64K boundary between them: a DMA transfer cannot cross one."
  (dma-fit (linear segment) (* 2 +block+)))

(defun capture (recording rate major start)
  "Fill RECORDING, a vector of bytes, from the card's input.  Returns the
number of samples recorded and the number of interrupts; or NIL and the DMA
count as it stayed, if nothing arrived."
  (let ((buffer (int-sap start)) (total (length recording))
        (recorded 0) (handled 0) (began (ticks)) (first-count 0))
    (dotimes (i (* 2 +block+)) (setf (sap-ref-8 buffer i) 128))
    (call-with-card-interrupt
     (lambda ()
       (dma-start *blaster-dma* start (* 2 +block+) :direction :to-memory)
       (blaster-write #xD3)                             ; speaker off: the old cards need it
       (cond ((>= major 4)
              (blaster-write #x42) (blaster-write (hi rate)) (blaster-write (lo rate))
              (blaster-write #xCE) (blaster-write #x00)
              (blaster-write (lo (1- +block+))) (blaster-write (hi (1- +block+))))
             (t
              (blaster-write #x40) (blaster-write (- 256 (round 1000000 rate)))
              (blaster-write #x48)
              (blaster-write (lo (1- +block+))) (blaster-write (hi (1- +block+)))
              (blaster-write #x2C)))
       (setf began (ticks) first-count (dma-count *blaster-dma*))
       (loop
         (when (/= handled (blaster-interrupts))
           (setf handled (blaster-interrupts))
           ;; the count goes down from two blocks, less one: while it is in
           ;; its upper half the card is filling block 0, so block 1 is full
           (let* ((full (if (>= (dma-count *blaster-dma*) +block+) 1 0))
                  (from (* full +block+))
                  (count (min +block+ (- total recorded)))
                  (peak 0))
             (dotimes (i count)
               (let ((sample (sap-ref-8 buffer (+ from i))))
                 (setf (aref recording (+ recorded i)) sample
                       peak (max peak (abs (- sample 128))))))
             (incf recorded count)
             (draw-wave (lambda (i) (sap-ref-8 buffer (+ from i))) +block+ nil)
             (draw-bar 120 peak 128 12)
             (draw-bar 130 recorded total 9)
             (draw-line-of-text 142 14 "recording ~,1F of ~,1F s" (/ recorded rate) (/ total rate))
             (draw-line-of-text 152 14 "loudest ~3D of 128 in block ~D" peak handled)))
         (when (>= recorded total) (return))
         (when (and (zerop handled) (> (- (ticks) began) 27))   ; a second and a half
           (return))
         (when (and (key-pressed-p) (eql (get-key) :escape)) (return)))))
    (if (zerop handled)
        (values nil (list first-count (dma-count *blaster-dma*)))
        (values recorded handled))))

(defun play-back (recording count rate start)
  "Play the first COUNT bytes of RECORDING.  Returns the number of interrupts."
  (let ((buffer (int-sap start)) (position 0) (handled 0) (silent-blocks 0)
        (major (progn (blaster-reset) (blaster-version))))
    (flet ((fill-block (which)
             (dotimes (i +block+)
               (setf (sap-ref-8 buffer (+ (* which +block+) i))
                     (if (< position count) (aref recording position) 128))
               (incf position))
             (when (> position (+ count +block+)) (incf silent-blocks))))
      (draw-wave (lambda (i) (aref recording i)) count nil)
      (call-with-card-interrupt
       (lambda ()
         (fill-block 0) (fill-block 1)
         (dma-start *blaster-dma* start (* 2 +block+))
         (blaster-write #xD1)                           ; speaker on
         (cond ((>= major 4)
                (blaster-write #x41) (blaster-write (hi rate)) (blaster-write (lo rate)))
               (t (blaster-write #x40) (blaster-write (- 256 (round 1000000 rate)))))
         (blaster-write #x48) (blaster-write (lo (1- +block+))) (blaster-write (hi (1- +block+)))
         (blaster-write #x1C)                               ; go: 8 bits, auto-initialize
         (loop until (>= silent-blocks 2)
               do (when (/= handled (blaster-interrupts))
                    (setf handled (blaster-interrupts))
                    ;; the block the card is not in has been played: refill it
                    (fill-block (if (>= (dma-count *blaster-dma*) +block+) 1 0))
                    (let ((played (min count (* handled +block+))))
                      (draw-wave (lambda (i) (aref recording i)) count
                                 (min 319 (floor (* 320 played) (max 1 count))))
                      (draw-bar 130 played count 10)
                      (draw-line-of-text 142 14 "playing ~,1F of ~,1F s"
                                         (/ played rate) (/ count rate))))
                  (when (and (key-pressed-p) (eql (get-key) :escape)) (return))))))
    handled))

(defun main (&optional (seconds 3))
  "Record from the Sound Blaster's input, showing it, then play the recording back."
  (unless (blaster-present-p)
    (format t "~&No Sound Blaster answered at port ~Xh.  (QEMU: -device sb16; ~
               SET BLASTER=A220 I5 D1 says where a card is.)~%" *blaster-port*)
    (return-from main (values)))
  (multiple-value-bind (major minor) (blaster-version)
    (when (< major 2)
      (format t "~&The card's DSP is version ~D.~2,'0D: auto-initialized DMA came with version 2.~%"
              major minor)
      (return-from main (values)))
    (let* ((rate 11025)
           (seconds (max 1 (min 60 (if (realp seconds) seconds 3))))
           (recording (make-array (* +block+ (ceiling (* seconds rate) +block+))
                                  :element-type '(unsigned-byte 8) :initial-element 128))
           (recorded nil) (detail nil) (played nil))
      (with-dos-memory (segment (* 4 +block+))
        (let ((start (buffer-address segment)))
          (with-video-mode (#x13)
            (graphics-text 0 0 (format nil "Recording: ~Xh IRQ ~D DMA ~D, DSP ~D.~2,'0D"
                                       *blaster-port* *blaster-irq* *blaster-dma* major minor)
                           7)
            (graphics-text 0 192 "Esc stops" 7)
            (draw-line-of-text 142 14 "waiting for the card's first block")
            (setf (values recorded detail) (capture recording rate major start))
            (when (and recorded (plusp recorded))
              (draw-bar 120 0 1 12)
              (draw-line-of-text 152 14 "")
              (setf played (play-back recording recorded rate start))))))
      (cond ((null recorded)
             (format t "~&The card (DSP ~D.~2,'0D) took the record command but handed over nothing ~
                        in a second and a~%half: no interrupt, and the DMA count went from ~
                        ~4,'0Xh to ~4,'0Xh.~%(QEMU's sb16 plays but does not record.)~%"
                     major minor (first detail) (second detail)))
            ((zerop recorded)
             (format t "~&Stopped before the first block was full: nothing recorded.~%"))
            (t
             (let* ((low (reduce #'min recording :end recorded))
                    (high (reduce #'max recording :end recorded))
                    (loudest (max (abs (- low 128)) (abs (- high 128)))))
               (format t "~&Recorded ~,1F s: ~:D samples at ~D a second in ~D blocks, from ~D to ~D ~
                          (silence is 128):~%~A~%Played it back: ~D interrupts.~%"
                       (/ recorded rate) recorded rate detail low high
                       (cond ((<= (- high low) 2) "the input was silent.")
                             ((< loudest 16) "the input was very quiet.")
                             ((or (= low 0) (= high 255)) "the input was loud enough to clip.")
                             (t (format nil "the loudest sample was ~D from silence, of 128."
                                        loudest)))
                       played))))))
  (values))

(doslib-examples:define-example "record" 'main
  :category :sound
  :description "Recording with a Sound Blaster: DMA from the card to memory, then played back.")
