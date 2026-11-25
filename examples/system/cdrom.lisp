;;;; The CD-ROM: MSCDEX, raw sectors of ISO 9660, and the audio tracks
;;;;
;;;; DOS knows nothing of compact discs.  A device driver (here UDVD2.SYS)
;;;; talks to the drive, and a resident program -- Microsoft's MSCDEX, or
;;;; FreeDOS's SHSUCDX -- makes the disc a drive letter by posing as a
;;;; network redirector.  It also answers INT 2Fh, function 15h, for
;;;; programs that want more than files:
;;;;
;;;;   1500h  how many CD-ROM drives, and the first one's letter
;;;;   150Ch  the version of the extensions
;;;;   1508h  read sectors, 2,048 bytes each, by number
;;;;   1510h  pass a request to the device driver: the way to the audio
;;;;          commands (disc and track information, play, stop)
;;;;
;;;; This reads sector 16, where every ISO 9660 disc keeps its "primary
;;;; volume descriptor" (the label, the size, and where the root directory
;;;; is), lists the root directory from its own sector without any help
;;;; from DOS, and asks the driver for the table of tracks.  Given a track
;;;; number -- DEMO CDROM 2 -- it plays that audio track until a key: the
;;;; drive plays by itself through its own cable to the sound card, which
;;;; is how games had CD music with no work for the processor.
;;;;
;;;; The launcher's machine has no CD-ROM drive loaded.  To try it under
;;;; QEMU add -cdrom some.iso, and at the DOS prompt:
;;;;     devload c:\freedos\bin\udvd2.sys /D:CD0
;;;;     shsucdx /D:CD0
;;;; QEMU's drive has no audio, so playing a track is untested there.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.cdrom
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.cdrom)

;;; One block of DOS memory: a device request at 0, its control block at 64,
;;; a sector at 2048.
(defconstant +control+ 64)
(defconstant +sector+ 2048)

(defun extensions ()
  "The number of CD-ROM drives and the first one's number (0 is A:), or NIL."
  (multiple-value-bind (ax count first) (real-int #x2F :ax #x1500 :bx 0)
    (declare (ignore ax))
    (and (plusp count) (values count first))))

(defun read-sector (segment drive number)
  "Read sector NUMBER of DRIVE to offset +SECTOR+ of SEGMENT.  True if it worked."
  (multiple-value-bind (ax bx cx dx es ds flags)
      (real-int #x2F :ax #x1508 :es segment :bx +sector+ :cx drive
                     :si (ldb (byte 16 16) number) :di (ldb (byte 16 0) number) :dx 1)
    (declare (ignore ax bx cx dx es ds))
    (not (logbitp 0 flags))))

(defun device-request (segment drive command length &rest bytes)
  "Send the device driver a request: LENGTH bytes, COMMAND, and BYTES from
offset 13.  Returns true unless the driver reports an error."
  (let ((request (int-sap (linear segment))))
    (dotimes (i 32) (setf (sap-ref-8 request i) 0))
    (setf (sap-ref-8 request 0) length (sap-ref-8 request 2) command)
    (loop for byte in bytes for i from 13 do (setf (sap-ref-8 request i) byte))
    (real-int #x2F :ax #x1510 :es segment :bx 0 :cx drive)
    (not (logbitp 15 (sap-ref-16 request 3)))))

(defun ioctl-input (segment drive code size)
  "Ask the driver for information: control block CODE, SIZE bytes long, at +CONTROL+."
  (let ((block (int-sap (linear segment +control+))))
    (dotimes (i size) (setf (sap-ref-8 block i) 0))
    (setf (sap-ref-8 block 0) code)
    (device-request segment drive 3 26
                    0 (lo +control+) (hi +control+) (lo segment) (hi segment) (lo size) (hi size))))

(defun frames (block offset)
  "A Red Book address (frame, second, minute) at OFFSET as a sector number."
  (+ (sap-ref-8 block offset)
     (* 75 (+ (sap-ref-8 block (+ offset 1)) (* 60 (sap-ref-8 block (+ offset 2)))))
     -150))

(defun text-at (sap offset length)
  (string-right-trim (list #\Space (code-char 0)) (map 'string #'code-char
                              (loop for i below length collect (sap-ref-8 sap (+ offset i))))))

(defun volume (segment drive)
  "The label, size and root directory of the disc, from sector 16."
  (let ((sector (int-sap (linear segment +sector+))))
    (cond ((not (read-sector segment drive 16))
           (format t "  no disc in the drive, or not one with data on it~%"))
          ((not (and (= 1 (sap-ref-8 sector 0)) (string= "CD001" (text-at sector 1 5))))
           (format t "  sector 16 is not an ISO 9660 volume descriptor~%"))
          (t
           (let ((blocks (sap-ref-32 sector 80))
                 (root (sap-ref-32 sector (+ 156 2))) (root-bytes (sap-ref-32 sector (+ 156 10))))
             (format t "  volume ~S: ~:D sectors of 2,048 bytes, ~,1F MB~%"
                     (text-at sector 40 32) blocks (/ (* blocks 2048) 1048576.0))
             (format t "  root directory at sector ~D, ~:D bytes:~%" root root-bytes)
             (when (read-sector segment drive root)
               (loop with at = 0 with shown = 0
                     for length = (sap-ref-8 sector at)
                     until (or (zerop length) (>= (+ at length) 2048) (>= shown 12))
                     do (let ((name (text-at sector (+ at 33) (sap-ref-8 sector (+ at 32)))))
                          (unless (member name (list (string (code-char 0)) (string (code-char 1)))
                                          :test #'string=)
                            (incf shown)
                            (format t "    ~20A ~:[~10:D bytes~;~*     <DIR>~]  at sector ~D~%"
                                    (subseq name 0 (or (position #\; name) (length name)))
                                    (logbitp 1 (sap-ref-8 sector (+ at 25)))
                                    (sap-ref-32 sector (+ at 10)) (sap-ref-32 sector (+ at 2)))))
                        (incf at length))))))))

(defun track-information (segment drive track)
  "Track information (control block 0Bh) for TRACK."
  (let ((block (int-sap (linear segment +control+))))
    (dotimes (i 7) (setf (sap-ref-8 block i) 0))
    (setf (sap-ref-8 block 0) #x0B (sap-ref-8 block 1) track)
    (device-request segment drive 3 26
                    0 (lo +control+) (hi +control+) (lo segment) (hi segment) 7 0)))

(defun tracks (segment drive)
  "Print the table of tracks.  Returns a list of (track start-sector audio-p), the lead-out last."
  (let ((block (int-sap (linear segment +control+))))
    (unless (ioctl-input segment drive #x0A 7)
      (format t "  the driver gives no table of tracks~%")
      (return-from tracks nil))
    (let* ((low (sap-ref-8 block 1)) (high (sap-ref-8 block 2)) (end (frames block 3))
           (table (loop for track from low to high
                        when (track-information segment drive track)
                          collect (list track (frames block 2)
                                        (not (logbitp 6 (sap-ref-8 block 6)))))))
      (format t "  tracks ~D to ~D:~%" low high)
      (loop for ((track start audio) next) on table
            do (let ((sectors (- (if next (second next) end) start)))
                 (format t "    ~2D  ~:[data ~;audio~]  from sector ~7:D, ~D:~2,'0D long~%"
                         track audio start (floor sectors 4500) (mod (floor sectors 75) 60))))
      (append table (list (list nil end nil))))))

(defun play (segment drive table track)
  (let ((entry (assoc track table)))
    (cond ((null entry) (format t "There is no track ~D.~%" track))
          ((not (third entry)) (format t "Track ~D is data, not audio.~%" track))
          (t (let* ((start (second entry))
                    (sectors (- (second (nth (1+ (position entry table)) table)) start)))
               (unwind-protect
                    (if (device-request segment drive 132 22 0
                                        (ldb (byte 8 0) start) (ldb (byte 8 8) start)
                                        (ldb (byte 8 16) start) (ldb (byte 8 24) start)
                                        (ldb (byte 8 0) sectors) (ldb (byte 8 8) sectors)
                                        (ldb (byte 8 16) sectors) (ldb (byte 8 24) sectors))
                        (progn (format t "Playing track ~D.  Any key stops it.~%" track)
                               (wait-key))
                        (format t "The drive would not play track ~D.~%" track))
                 (device-request segment drive 133 13)))))))

(defun cdrom (&optional track)
  (multiple-value-bind (count first) (extensions)
    (unless count
      (format t "~&No CD-ROM: the CD-ROM extensions (MSCDEX, SHSUCDX) are not loaded.~%")
      (return-from cdrom nil))
    (multiple-value-bind (ax version) (real-int #x2F :ax #x150C :bx 0)
      (declare (ignore ax))
      (format t "~&CD-ROM extensions ~D.~2,'0D: ~D drive~:P, the first is ~C:~%"
              (hi version) (lo version) count (code-char (+ 65 first))))
    (with-dos-memory (segment 4096)
      (volume segment first)
      (let ((table (tracks segment first)))
        (when (and track table) (play segment first table track)))))
  (values))

(defun main (&optional track)
  "The CD-ROM: MSCDEX, raw sectors of ISO 9660, and the audio tracks."
  (cdrom track))

(doslib-examples:define-example "cdrom" 'main
  :category :system
  :description "The CD-ROM: MSCDEX, raw sectors of ISO 9660, and the audio tracks.")
