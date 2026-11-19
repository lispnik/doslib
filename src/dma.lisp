;;;; The DMA controllers: memory handed to a device, or filled by one,
;;;; without the processor taking part.
;;;;
;;;; A device with a DMA channel -- a sound card, the floppy controller --
;;;; asks for each byte when it wants it, and the DMA controller answers:
;;;; it has been told an address and a length, and it moves the next byte
;;;; between memory and the device and counts.  The program meanwhile does
;;;; something else.  Told to repeat, the controller starts again at the
;;;; beginning when it reaches the end, for ever: which is how sound is
;;;; played without a gap (see examples/sound/blaster.lisp).
;;;;
;;;; Channels 0 to 3 move bytes.  Channels 5 to 7, on a second controller,
;;;; move 16-bit words, and their address and count registers hold words.
;;;; The controllers know nothing of protected mode: an address is a
;;;; physical one, of 24 bits, and the memory must be below 16MB -- DOS
;;;; memory, in practice (DOS-ALLOC) -- and must not cross a 64K boundary,
;;;; because the top bits sit in a separate "page" register that does not
;;;; count.  DMA-FIT and WITH-DMA-MEMORY see to that.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defun dma-page-port (channel)
  (aref #(#x87 #x83 #x81 #x82 #x8F #x8B #x89 #x8A) channel))

(defun dma-start (channel address length &key (direction :to-device) (repeat t))
  "Have DMA channel CHANNEL move LENGTH bytes at ADDRESS: to its device
(DIRECTION :TO-DEVICE, playing) or from it into memory (:TO-MEMORY,
recording, reading a disk).  With REPEAT it starts again at ADDRESS each
time it has moved them all; without, it stops there."
  (let ((mode (+ #x40                           ; one byte for each request
                 (if repeat #x10 0)
                 (ecase direction (:to-device #x08) (:to-memory #x04)))))
    (if (< channel 4)
        (progn
          (outb #x0A (+ 4 channel))             ; mask the channel while it is set up
          (outb #x0C 0)                         ; low byte first, in what follows
          (outb #x0B (+ mode channel))
          (outb (* 2 channel) (lo address))
          (outb (* 2 channel) (hi address))
          (outb (dma-page-port channel) (ldb (byte 8 16) address))      ; the top bits
          (outb (1+ (* 2 channel)) (lo (1- length)))
          (outb (1+ (* 2 channel)) (hi (1- length)))
          (outb #x0A channel))                  ; and let it go
        (let ((n (- channel 4)) (words (ash address -1)))
          (outb #xD4 (+ 4 n))
          (outb #xD8 0)
          (outb #xD6 (+ mode n))
          (outb (+ #xC0 (* 4 n)) (lo words))
          (outb (+ #xC0 (* 4 n)) (hi words))
          (outb (dma-page-port channel) (ldb (byte 8 16) address))
          (outb (+ #xC2 (* 4 n)) (lo (1- (ash length -1))))
          (outb (+ #xC2 (* 4 n)) (hi (1- (ash length -1))))
          (outb #xD4 n))))
  (values))

(defun dma-stop (channel)
  "Mask DMA channel CHANNEL: it answers its device no more."
  (if (< channel 4) (outb #x0A (+ 4 channel)) (outb #xD4 channel))
  (values))

(defun dma-register (channel which)
  ;; A register is read as two bytes, and the channel may count between
  ;; them: so it is read until two readings agree in their high byte.
  (let ((port (if (< channel 4)
                  (+ (* 2 channel) which)
                  (+ #xC0 (* 4 (- channel 4)) (* 2 which))))
        (flip-flop (if (< channel 4) #x0C #xD8)))
    (flet ((once ()
             (outb flip-flop 0)                 ; the next byte read is the low one
             (let ((low (inb port))) (+ low (ash (inb port) 8)))))
      (loop for first = (once) for second = (once)
            repeat 8
            when (= (hi first) (hi second)) return second
            finally (return second)))))
(defun dma-address (channel)
  "The channel's address register as it is now: the low 16 bits of where it
will move the next byte (for channels 5 to 7, the next word, counted in words)."
  (dma-register channel 0))
(defun dma-count (channel)
  "The channel's count register as it is now: the bytes (for channels 5 to 7,
the words) it has still to move, less one.  FFFFh: it has moved them all."
  (dma-register channel 1))

(defun dma-fit (address length)
  "The first address at or after ADDRESS where LENGTH bytes (65536 at most)
lie without a 64K boundary among them.  Memory of twice LENGTH has room."
  (if (> (+ (logand address #xFFFF) length) #x10000)
      (logandc2 (+ address #xFFFF) #xFFFF)
      address))
(defmacro with-dma-memory ((address length) &body body)
  "Run BODY with ADDRESS bound to the address of LENGTH bytes of DOS memory
that a DMA channel can be pointed at, freed afterwards."
  (let ((segment (gensym "SEGMENT")) (bytes (gensym "BYTES")))
    `(let ((,bytes ,length))
       (with-dos-memory (,segment (* 2 ,bytes))
         (let ((,address (dma-fit (linear ,segment) ,bytes)))
           ,@body)))))
