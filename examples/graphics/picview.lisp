;;;; A picture viewer for the two formats of the DOS years that "bmpview" does
;;;; not read: PCX and GIF, both decoded here in Lisp and shown in mode 13h
;;;; (320 by 200 in 256 colours).
;;;;
;;;; PCX is PC Paintbrush's format.  A header of 128 bytes gives the size;
;;;; then come the rows, top first, each run-length encoded: a byte whose top
;;;; two bits are set is a count (its low six bits, 1 to 63) of the byte that
;;;; follows, and any other byte is one pixel -- so a pixel of colour 192 or
;;;; more has to be written as a run, if only a run of one.  The palette was
;;;; an afterthought (the format is older than the VGA): the last 769 bytes
;;;; of the file, a 12 and then red, green and blue for each of 256 colours.
;;;;
;;;; GIF is CompuServe's (versions 87a and 89a).  After the size comes the
;;;; palette, then blocks: extensions, which are skipped here, and images, of
;;;; which the first is shown.  The pixels are compressed by LZW: the file
;;;; holds codes of 9 bits at first, growing to 12, each standing for a string
;;;; of pixels, and the table that says which is not in the file at all: the
;;;; reader builds it from the codes as they come, exactly as the writer did
;;;; -- every code adds a string to it, the one before plus the first pixel
;;;; of this one.  The codes are packed low bit first into blocks of up to
;;;; 255 bytes.  An interlaced image holds rows 0, 8, 16 ..., then 4, 12 ...,
;;;; then 2, 6 ..., then the odd ones, so that a picture arriving at 2400 baud
;;;; could be made out before a quarter of it had come.
;;;;
;;;;   DEMO PICVIEW                       the two sample pictures
;;;;   DEMO PICVIEW file                  a PCX or GIF file of your own
;;;;   (doslib-examples:run "picview" "C:\\PICS\\HOUSE.GIF")
;;;; Any key goes on to the next picture and Esc leaves; then it says, for
;;;; each picture, how big the file was for its pixels and how long decoding
;;;; took.  A picture bigger than the screen is shown every second (or third
;;;; ...) pixel, a smaller one in the middle.  PCX files must be the
;;;; 256-colour kind (8 bits, one plane); animated GIFs show their first frame.
;;;;
;;;; The samples, examples/data/sample.pcx and sample.gif (interlaced), were
;;;; drawn by examples/support/make-pictures.py.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.picview
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.picview)

(deftype bytes () '(simple-array (unsigned-byte 8) (*)))
(defstruct picture
  kind                                  ; what the file is, in words
  (width 0 :type fixnum) (height 0 :type fixnum)
  (pixels nil :type bytes)              ; a byte a pixel, the top row first
  (palette nil :type bytes))            ; red, green, blue for each colour, 0 to 255

(defun u16 (bytes offset) (+ (aref bytes offset) (ash (aref bytes (1+ offset)) 8)))
(defun make-bytes (count) (make-array count :element-type '(unsigned-byte 8) :initial-element 0))
(defun file-bytes (filename)
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (let ((data (make-bytes (file-length in))))
      (read-sequence data in)
      data)))

;;; ---- PCX
(defun unpack-runs (bytes start end row-bytes width height)
  "The pixels of a PCX file's rows, which are BYTES from START to END.  A row is ROW-BYTES
long in the file (an even number), of which the first WIDTH are pixels."
  (declare (optimize speed) (type bytes bytes) (fixnum start end row-bytes width height))
  (let ((pixels (make-bytes (* width height))) (at start) (x 0) (y 0))
    (declare (fixnum at x y))
    (loop while (and (< y height) (< at end))
          do (let ((byte (aref bytes at)) (count 1))
               (declare (fixnum count))
               (incf at)
               (when (>= byte #xC0)     ; a count: the byte after it, that many times
                 (setf count (logand byte #x3F)
                       byte (if (< at end) (aref bytes at) 0))
                 (incf at))
               (dotimes (i count)
                 (when (and (< x width) (< y height))
                   (setf (aref pixels (+ x (the fixnum (* width y)))) byte))
                 (when (= (incf x) row-bytes) (setf x 0) (incf y)))))
    pixels))
(defun decode-pcx (bytes)
  (let ((width (- (u16 bytes 8) (u16 bytes 4) -1)) (height (- (u16 bytes 10) (u16 bytes 6) -1))
        (palette (- (length bytes) 769)))
    (unless (and (= (aref bytes 2) 1) (= (aref bytes 3) 8) (= (aref bytes 65) 1)
                 (> palette 128) (= (aref bytes palette) 12))
      (error "is a PCX file, but not a 256-colour one"))
    (make-picture :kind (format nil "PCX version ~D" (aref bytes 1))
                  :width width :height height
                  :pixels (unpack-runs bytes 128 palette (u16 bytes 66) width height)
                  :palette (subseq bytes (1+ palette)))))

;;; ---- GIF
(defun gather-blocks (bytes at)
  "The data of the blocks that start at AT in BYTES, each a count and then that many bytes,
up to a count of 0: in one vector."
  (let ((total (loop for i = at then (+ i 1 (aref bytes i))
                     until (zerop (aref bytes i)) sum (aref bytes i))))
    (loop with data = (make-bytes total)
          for to = 0 then (+ to count)
          for count = (aref bytes at)
          until (zerop count)
          do (replace data bytes :start1 to :start2 (1+ at) :end2 (+ at 1 count))
             (incf at (1+ count))
          finally (return data))))
(defun unpack-lzw (data minimum count)
  "COUNT pixels from the LZW codes in DATA.  MINIMUM is the bits in a pixel: there are two
more codes than pixel values -- one to say that the table starts again, and one for the end
-- so the codes begin a bit wider than that."
  (declare (optimize speed) (type bytes data) (type (integer 2 8) minimum) (fixnum count))
  (let* ((pixels (make-bytes count))
         ;; the table: code N is the string of code PREFIX[N] with the pixel SUFFIX[N] after it
         (prefix (make-array 4096 :element-type '(unsigned-byte 16) :initial-element 0))
         (suffix (make-bytes 4096))
         (stack (make-bytes 4097))      ; a string comes out of the table last pixel first
         (clear (ash 1 minimum)) (end (1+ clear))
         (next (+ clear 2))             ; the code the next string will have
         (size (1+ minimum))            ; bits in a code now
         (old -1) (first 0) (top 0) (out 0)
         (at 0) (bits 0) (accumulator 0))
    (declare (fixnum clear end next old first top out at)
             (type (integer 2 12) size) (type (integer 0 24) bits)
             (type (unsigned-byte 24) accumulator))
    (loop
      (loop while (< bits size)         ; codes are packed low bit first
            do (when (>= at (length data)) (return-from unpack-lzw pixels))
               (setf accumulator (logior accumulator (ash (aref data at) bits)))
               (incf at)
               (incf bits 8))
      (let ((code (logand accumulator (1- (ash 1 size)))))
        (declare (fixnum code))
        (setf accumulator (ash accumulator (- size)))
        (decf bits size)
        (cond ((= code clear) (setf size (1+ minimum) next (+ clear 2) old -1))
              ((= code end) (return))
              ((or (> code next) (and (< old 0) (> code clear)))
               (error "is a GIF file with damaged data"))
              ((< old 0)                ; the first code after a clear is a pixel
               (when (< out count) (setf (aref pixels out) code) (incf out))
               (setf first code old code))
              (t
               (let ((this code))
                 (when (= code next)    ; the string being added now: the old one, and ...
                   (setf (aref stack top) first)        ; ... its own first pixel again
                   (incf top)
                   (setf code old))
                 (loop while (> code clear)
                       do (setf (aref stack top) (aref suffix code))
                          (incf top)
                          (setf code (aref prefix code)))
                 (setf first code)
                 (setf (aref stack top) first)
                 (incf top)
                 (when (< next 4096)    ; add: the string before, and this one's first pixel
                   (setf (aref prefix next) old (aref suffix next) first)
                   (incf next)
                   (when (and (= next (ash 1 size)) (< size 12)) (incf size)))
                 (setf old this)
                 (loop while (plusp top)
                       do (decf top)
                          (when (< out count)
                            (setf (aref pixels out) (aref stack top))
                            (incf out))))))))
    pixels))
(defun interlaced-rows (height)
  "The rows of an interlaced GIF image, in the order the file has them."
  (loop for (start step) in '((0 8) (4 8) (2 4) (1 2))
        nconc (loop for y from start below height by step collect y)))
(defun decode-gif (bytes)
  (let ((at 13) (palette (make-bytes 768)))
    (flet ((colours (flags)             ; a palette, if FLAGS says one follows
             (when (logbitp 7 flags)
               (let ((count (* 3 (ash 2 (logand flags 7)))))
                 (replace palette bytes :start2 at :end2 (+ at count))
                 (incf at count)))))
      (colours (aref bytes 10))         ; the palette for the whole file
      (loop
        (case (aref bytes at)
          (#x21                         ; an extension: its kind, then blocks
           (incf at 2)
           (loop for count = (aref bytes at)
                 do (incf at (1+ count))
                 until (zerop count)))
          (#x2C                         ; an image: where, how big, and how
           (let* ((width (u16 bytes (+ at 5))) (height (u16 bytes (+ at 7)))
                  (flags (aref bytes (+ at 9)))
                  (interlaced (logbitp 6 flags)))
             (incf at 10)
             (colours flags)            ; (a palette of its own replaces the file's)
             (unless (<= 2 (aref bytes at) 8) (error "is a GIF file with damaged data"))
             (let ((pixels (unpack-lzw (gather-blocks bytes (1+ at)) (aref bytes at)
                                       (* width height))))
               (when interlaced
                 (loop with straight = (make-bytes (* width height))
                       for y in (interlaced-rows height)
                       for from from 0 by width
                       do (replace straight pixels :start1 (* width y) :end1 (* width (1+ y))
                                                   :start2 from)
                       finally (setf pixels straight)))
               (return (make-picture
                        :kind (format nil "GIF ~A~:[~;, interlaced~]"
                                      (map 'string #'code-char (subseq bytes 3 6)) interlaced)
                        :width width :height height :pixels pixels :palette palette)))))
          (t (error "is a GIF file with no picture in it")))))))

(defun read-picture (filename)
  "The picture in the PCX or GIF file FILENAME, and the bytes in the file."
  (unless (probe-file filename) (error "is not there"))
  (let ((bytes (file-bytes filename)))
    (values (cond ((and (> (length bytes) 13) (equalp (subseq bytes 0 4) #(71 73 70 56)))  ; GIF8
                   (decode-gif bytes))
                  ((and (> (length bytes) 128) (= (aref bytes 0) 10)) (decode-pcx bytes))
                  (t (error "is neither a PCX nor a GIF file")))
            (length bytes))))

;;; ---- the screen
(defun show (picture)
  "Set mode 13h and show PICTURE: in the middle, and if it is too big for the screen, only
every second pixel of every second row of it, or every third, or whatever makes it fit."
  (declare (optimize speed))
  (let* ((width (picture-width picture)) (height (picture-height picture))
         (pixels (picture-pixels picture)) (palette (picture-palette picture))
         (step (max 1 (ceiling width 320) (ceiling height 200)))
         (columns (floor width step)) (rows (floor height step))
         (left (floor (- 320 columns) 2)) (top (floor (- 200 rows) 2))
         (vga *vga*)
         ;; the darkest colour the picture has is the one to put round it
         (dark (loop with best = 0
                     for i from 0 below 256
                     when (< (+ (aref palette (* 3 i)) (aref palette (+ (* 3 i) 1))
                                (aref palette (+ (* 3 i) 2)))
                             (+ (aref palette (* 3 best)) (aref palette (+ (* 3 best) 1))
                                (aref palette (+ (* 3 best) 2))))
                       do (setf best i)
                     finally (return best))))
    (declare (fixnum width height step columns rows left top dark)
             (type system-area-pointer vga))
    (video-mode #x13)
    (dotimes (i 256)                    ; the VGA wants 6 bits of each
      (set-palette i (ash (aref palette (* 3 i)) -2) (ash (aref palette (+ (* 3 i) 1)) -2)
                   (ash (aref palette (+ (* 3 i) 2)) -2)))
    (dotimes (i 16000) (setf (sap-ref-32 vga (* 4 i)) (* dark #x01010101)))
    (dotimes (y rows)
      (let ((from (* width (the fixnum (* y step)))) (to (+ left (* 320 (+ top y)))))
        (declare (fixnum from to))
        (dotimes (x columns)
          (setf (sap-ref-8 vga (+ to x)) (aref pixels from))
          (incf from step))))))

(defun view (files)
  (let ((notes '()))
    (unwind-protect
         (dolist (file files)
           (let ((began (timer-count)))
             (multiple-value-bind (picture size)
                 (handler-case (read-picture file)
                   (simple-error (e) (push (format nil "~A ~A." file e) notes) nil)
                   (error () (push (format nil "~A cannot be read as a picture." file) notes)
                     nil))
               (when picture
                 (let ((pixels (* (picture-width picture) (picture-height picture))))
                   (push (format nil "~A: ~A, ~D by ~D.~%  ~:D bytes of file for ~:D pixels ~
                                      (~D%), read and decoded in ~D ms."
                                 file (picture-kind picture)
                                 (picture-width picture) (picture-height picture)
                                 size pixels (round (* 100 size) (max 1 pixels))
                                 (round (* 1000 (- (timer-count) began)) +timer-hz+))
                         notes))
                 (show picture)
                 (when (eql (get-key) #\Escape) (return))))))
      (unless (= (current-video-mode) 3) (video-mode 3)))
    (dolist (note (reverse notes)) (format t "~&~A~%" note))))

(defun main (&optional file)
  "A viewer for PCX and GIF pictures, decoded in Lisp."
  (view (if file
            (list (princ-to-string file))
            (list (doslib-examples:data-file "sample.pcx")
                  (doslib-examples:data-file "sample.gif"))))
  (values))

(doslib-examples:define-example "picview" 'main
  :category :graphics
  :description "A viewer for PCX and GIF pictures, decoded in Lisp.")
