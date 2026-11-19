;;;; Reading disks a sector at a time: through DOS, and through the BIOS.
;;;;
;;;; DOS reads the sectors of a drive letter, numbered from the start of
;;;; its volume (DOS-READ-SECTORS); the BIOS reads the sectors of a whole
;;;; disk, partition table and all (BIOS-READ-SECTORS).  Both put the data
;;;; in memory below 1MB, since they run in real mode: the library's
;;;; scratch block (DOS-SCRATCH), from which it is copied into a vector.
;;;;
;;;; DOS has had three ways of reading sectors, and which one works depends
;;;; on the volume: the original (the sector number in DX; floppies and
;;;; volumes under 32MB), the same interrupt 25h with a packet (CX = FFFFh;
;;;; larger FAT16 volumes), and interrupt 21h function 7305h (FAT32, which
;;;; interrupt 25h refuses).  The first read of a drive finds out.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(define-condition disk-error (error)
  ((drive :initarg :drive :reader disk-error-drive)
   (sector :initarg :sector :reader disk-error-sector)
   (code :initarg :code :initform nil :reader disk-error-code)
   (reason :initarg :reason :initform nil :reader disk-error-reason))
  (:documentation "A sector could not be read: DOS-READ-SECTORS and BIOS-READ-SECTORS signal it.")
  (:report (lambda (c stream)
             (format stream "Cannot read sector ~D of ~A~@[: ~A~]~@[ (error ~4,'0Xh)~]"
                     (disk-error-sector c) (disk-error-drive c)
                     (disk-error-reason c) (disk-error-code c)))))

(defun current-drive ()
  "The number of DOS's current drive: 0 is A:, 2 is C:."
  (lo (int86 #x21 :ax #x1900)))
(defun drive-number (drive)
  "DRIVE as a number (0 is A:), given a number, a letter, a string such as
\"c:\", or NIL for the current drive."
  (etypecase drive
    (null (current-drive))
    (integer drive)
    (character (- (char-code (char-upcase drive)) 65))
    (string (if (string= drive "") (current-drive) (drive-number (char drive 0))))))
(defun drive-letter (drive)
  "The letter of DRIVE (as DRIVE-NUMBER takes it): a character."
  (code-char (+ 65 (drive-number drive))))

(defvar *drive-methods* (make-hash-table) "Drive number -> the way of reading it that works.")

(defun absolute-read (drive sector count method)
  "COUNT sectors of DRIVE into the scratch block, at offset 16, by METHOD.
Returns true if it worked, and DOS's error code if not."
  (let* ((segment (dos-scratch)) (packet (int-sap (linear segment))))
    ;; The packet: sector number, count, and the far address of the data.
    (setf (sap-ref-32 packet 0) sector (sap-ref-16 packet 4) count
          (sap-ref-16 packet 6) 16 (sap-ref-16 packet 8) segment)
    (multiple-value-bind (ax bx cx dx es ds flags)
        (ecase method
          (:small (real-int #x25 :ax drive :cx count :dx sector :ds segment :bx 16))
          (:large (real-int #x25 :ax drive :cx #xFFFF :ds segment :bx 0))
          (:fat32 (real-int #x21 :ax #x7305 :cx #xFFFF :dx (1+ drive) :si 0 :ds segment :bx 0)))
      (declare (ignore bx cx dx es ds))
      (values (not (logbitp 0 flags)) ax))))

(defun drive-read-method (drive)
  "Which of DOS's three ways reads DRIVE: :SMALL, :LARGE or :FAT32.  Signals
DISK-ERROR if there is no such drive, or none of them works."
  (let ((drive (drive-number drive)))
    (or (gethash drive *drive-methods*)
        ;; Function 36h (free space) says whether the drive exists and how
        ;; big it is: which matters, because a DOS that does not expect the
        ;; packet on a small volume would take CX = FFFFh as a count.
        (multiple-value-bind (ax bx cx dx) (int86 #x21 :ax #x3600 :dx (1+ drive))
          (declare (ignore bx cx))
          (when (= (word16 ax) #xFFFF)
            (error 'disk-error :drive (format nil "~C:" (drive-letter drive)) :sector 0
                               :reason (or (critical-error) "there is no such drive, or no disk in it")))
          (let ((method (find-if (lambda (method) (absolute-read drive 0 1 method))
                                 (if (< (* (word16 ax) (word16 dx)) 65536)
                                     '(:small :large :fat32)
                                     '(:large :fat32)))))
            (unless method
              (error 'disk-error :drive (format nil "~C:" (drive-letter drive)) :sector 0
                                 :reason (or (critical-error)
                                             "DOS cannot read its sectors (a network drive or a CD?)")))
            (setf (gethash drive *drive-methods*) method))))))

(defun dos-read-sectors (drive sector &key (count 1) (bytes-per-sector 512))
  "COUNT sectors of a DOS drive (a number, 0 for A:, or a letter, or NIL for
the current one), starting at SECTOR of its volume, as a vector of bytes."
  (let* ((drive (drive-number drive))
         (method (drive-read-method drive))
         (result (make-array (* count bytes-per-sector) :element-type '(unsigned-byte 8)))
         (most (floor *scratch-bytes* bytes-per-sector))
         (data (int-sap (linear (dos-scratch) 16))))
    (loop for done from 0 below count by most
          for n = (min most (- count done))
          do (multiple-value-bind (ok code) (absolute-read drive (+ sector done) n method)
               (unless ok
                 (error 'disk-error :drive (format nil "~C:" (drive-letter drive))
                                    :sector (+ sector done) :code code :reason (critical-error))))
             (dotimes (i (* n bytes-per-sector))
               (setf (aref result (+ (* done bytes-per-sector) i)) (sap-ref-8 data i))))
    result))

(defun bios-read-sectors (drive sector &optional (count 1))
  "COUNT 512-byte sectors of a whole disk, through the BIOS (interrupt 13h,
function 42h): DRIVE is the BIOS's number, #x80 for the first hard disk,
and SECTOR counts from the very start of the disk."
  (let* ((segment (dos-scratch)) (packet (int-sap (linear segment)))
         (result (make-array (* count 512) :element-type '(unsigned-byte 8)))
         (most (floor *scratch-bytes* 512))
         (data (int-sap (linear segment 16))))
    (loop for done from 0 below count by most
          for n = (min most (- count done))
          do (setf (sap-ref-16 packet 0) 16 (sap-ref-16 packet 2) n
                   (sap-ref-16 packet 4) 16 (sap-ref-16 packet 6) segment
                   (sap-ref-32 packet 8) (+ sector done) (sap-ref-32 packet 12) 0)
             (multiple-value-bind (ax bx cx dx es ds flags)
                 (real-int #x13 :ax #x4200 :dx drive :ds segment :si 0)
               (declare (ignore bx cx dx es ds))
               (when (logbitp 0 flags)
                 (error 'disk-error :drive (format nil "BIOS drive ~2,'0Xh" drive)
                                    :sector (+ sector done) :code (hi ax))))
             (dotimes (i (* n 512))
               (setf (aref result (+ (* done 512) i)) (sap-ref-8 data i))))
    result))
