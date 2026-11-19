;;;; A disk sector viewer and FAT explorer, full screen, in text mode.
;;;;
;;;;   (doslib-examples:run "fatview")            the current drive
;;;;   (doslib-examples:run "fatview" "C")        drive C:  ("A", "d:" ...)
;;;;   (doslib-examples:run "fatview" "DISK.IMG") a disk image in a file
;;;;
;;;; It only ever READS the disk.  Four views, changed with a letter or Tab:
;;;;
;;;;   B  the boot sector, decoded: who formatted the volume, its geometry,
;;;;      where the FATs, the root directory and the data begin, and whether
;;;;      it is FAT12, FAT16 or FAT32 (which depends only on how many
;;;;      clusters there are);
;;;;   D  the directories, from the root down: every entry with its
;;;;      attributes, size, first cluster and time stamp, and its long name
;;;;      put together from the hidden entries that precede it.  Enter goes
;;;;      into a directory (Backspace comes back), or shows a file: the chain
;;;;      of clusters the FAT gives for it, and its first bytes;
;;;;   S  any sector in hexadecimal: PgUp/PgDn step by sector, G asks for a
;;;;      sector number (or xHEX, or cN for the first sector of cluster N);
;;;;   F  the FAT summed up: free, used and bad clusters, and a map of
;;;;      where on the volume the used ones are.
;;;;
;;;; Esc or Q leaves and puts the screen back.
;;;;
;;;; Sectors come from DOS's absolute disk read, interrupt 25h, which counts
;;;; sectors from the start of the volume (so the boot sector is sector 0
;;;; whatever the partition table says).  It has three forms, for historical
;;;; reasons, and which one a volume needs depends on its size: the original
;;;; with a 16-bit sector number in DX; from DOS 3.31, CX = FFFFh and a packet
;;;; holding a 32-bit sector number, for volumes over 32MB; and for FAT32
;;;; volumes a different interrupt altogether, 21h function 7305h, with the
;;;; same packet.  The interrupt is run in real mode (DOSLIB:REAL-INT) with
;;;; its buffer in memory below 1MB (DOSLIB:DOS-ALLOC).
;;;;
;;;; Everything that decodes what was read -- the boot sector, directory
;;;; entries and long names, FAT entries of the three widths -- is a pure
;;;; function of a vector of bytes, so it works as well on a disk image read
;;;; from a file, on any machine; which is how it was tested.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.fatview
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.fatview)

;;; ------------------------------------------------------------------------
;;; Decoding: pure functions of byte vectors.

(defun u8 (bytes i) (aref bytes i))
(defun u16 (bytes i) (logior (aref bytes i) (ash (aref bytes (+ i 1)) 8)))
(defun u32 (bytes i) (logior (u16 bytes i) (ash (u16 bytes (+ i 2)) 16)))
(defun ascii (bytes start length)
  "The text in a fixed-width field: padded with spaces, perhaps with rubbish in it."
  (string-right-trim " " (map 'string (lambda (b) (if (<= 32 b 254) (code-char b) #\?))
                              (subseq bytes start (+ start length)))))

;;; The BIOS parameter block: the boot sector's description of the volume.
;;; The fields up to offset 36 are the same on every FAT volume; after that
;;; FAT32 has its own (a 32-bit FAT size, the root directory's cluster)
;;; before the serial number and label that FAT12/16 keep at offset 36.
(defstruct bpb
  oem bytes-per-sector sectors-per-cluster reserved fats root-entries total-sectors media
  fat-size sectors-per-track heads hidden serial label type-name root-cluster
  ;; and worked out from those:
  root-sector root-sectors data-sector clusters fat-type)

(defun parse-bpb (sector)
  "Decode a boot sector.  NIL if it does not describe a FAT volume."
  (let ((bytes-per-sector (u16 sector 11)) (sectors-per-cluster (u8 sector 13))
        (reserved (u16 sector 14)) (fats (u8 sector 16)))
    (when (and (member bytes-per-sector '(512 1024 2048 4096))
               (member sectors-per-cluster '(1 2 4 8 16 32 64 128))
               (plusp reserved) (<= 1 fats 4))
      (let* ((root-entries (u16 sector 17))
             (total (if (zerop (u16 sector 19)) (u32 sector 32) (u16 sector 19)))
             (fat-size (if (zerop (u16 sector 22)) (u32 sector 36) (u16 sector 22)))
             (root-sector (+ reserved (* fats fat-size)))
             (root-sectors (ceiling (* 32 root-entries) bytes-per-sector))
             (data-sector (+ root-sector root-sectors))
             (clusters (floor (- total data-sector) sectors-per-cluster))
             ;; The official rule: the type is decided by the number of
             ;; clusters and nothing else -- not by the name in the boot sector.
             (fat-type (cond ((< clusters 4085) 12) ((< clusters 65525) 16) (t 32)))
             (extended (if (= fat-type 32) 64 36))      ; drive number, signature, serial, label
             (signed (= (u8 sector (+ extended 2)) #x29)))
        (when (and (plusp fat-size) (plusp clusters))
          (make-bpb :oem (ascii sector 3 8) :bytes-per-sector bytes-per-sector
                    :sectors-per-cluster sectors-per-cluster :reserved reserved :fats fats
                    :root-entries root-entries :total-sectors total :media (u8 sector 21)
                    :fat-size fat-size :sectors-per-track (u16 sector 24) :heads (u16 sector 26)
                    :hidden (u32 sector 28)
                    :serial (and signed (u32 sector (+ extended 3)))
                    :label (if signed (ascii sector (+ extended 7) 11) "")
                    :type-name (if signed (ascii sector (+ extended 18) 8) "")
                    :root-cluster (if (= fat-type 32) (u32 sector 44) 0)
                    :root-sector root-sector :root-sectors root-sectors :data-sector data-sector
                    :clusters clusters :fat-type fat-type))))))

(defun partition-start (sector)
  "Given the first sector of a disk image: 0 if it is a boot sector itself,
or where the first partition starts if it is a master boot record."
  (cond ((and (member (u8 sector 0) '(#xEB #xE9)) (parse-bpb sector)) 0)
        ((and (= (u16 sector 510) #xAA55) (/= 0 (u8 sector 450))) (u32 sector 454))
        (t 0)))

(defun cluster-sector (bpb cluster)
  "The first sector of CLUSTER.  Clusters are numbered from 2."
  (+ (bpb-data-sector bpb) (* (- cluster 2) (bpb-sectors-per-cluster bpb))))

(defun sector-region (bpb sector)
  "What part of the volume SECTOR belongs to, in words."
  (let ((fat (- sector (bpb-reserved bpb))) (data (- sector (bpb-data-sector bpb))))
    (cond ((zerop sector) "the boot sector")
          ((< sector (bpb-reserved bpb)) "reserved sectors")
          ((< sector (bpb-root-sector bpb))
           (multiple-value-bind (copy offset) (floor fat (bpb-fat-size bpb))
             (format nil "FAT ~D, sector ~D of ~D" (1+ copy) (1+ offset) (bpb-fat-size bpb))))
          ((< sector (bpb-data-sector bpb)) "the root directory")
          ((< (floor data (bpb-sectors-per-cluster bpb)) (bpb-clusters bpb))
           (multiple-value-bind (cluster offset) (floor data (bpb-sectors-per-cluster bpb))
             (format nil "data: cluster ~D, sector ~D of ~D" (+ cluster 2) (1+ offset)
                     (bpb-sectors-per-cluster bpb))))
          (t "beyond the last cluster"))))

;;; The FAT is an array with one entry per cluster, 12, 16 or 32 bits wide,
;;; saying which cluster comes next in the file that owns this one.
(defun fat-entry (bytes fat-type cluster &optional (base 0))
  "Entry CLUSTER of a FAT.  BYTES holds the FAT from its byte BASE on."
  (ecase fat-type
    ;; Twelve bits: two entries share three bytes, so an entry starts at
    ;; byte 1.5 * cluster and is the low or the high 12 bits of the word there.
    (12 (let ((word (u16 bytes (- (+ cluster (floor cluster 2)) base))))
          (if (oddp cluster) (ash word -4) (logand word #xFFF))))
    (16 (u16 bytes (- (* 2 cluster) base)))
    (32 (logand (u32 bytes (- (* 4 cluster) base)) #x0FFFFFFF))))      ; the top 4 bits are reserved

(defun fat-entry-kind (fat-type value)
  "What a FAT entry says: :FREE, :BAD, :END (of a file), :RESERVED, or :NEXT
when it is the number of the next cluster."
  (let ((top (ecase fat-type (12 #xFFF) (16 #xFFFF) (32 #x0FFFFFFF))))
    (cond ((zerop value) :free)
          ((>= value (- top 7)) :end)           ; FF8..FFF
          ((= value (- top 8)) :bad)            ; FF7
          ((or (= value 1) (>= value (- top 15))) :reserved)
          (t :next))))

(defun chain-runs (chain)
  "A list of clusters as runs of consecutive ones: ((first . last) ...)."
  (let ((runs '()))
    (dolist (cluster chain (nreverse runs))
      (if (and runs (= cluster (1+ (cdar runs))))
          (setf (cdar runs) cluster)
          (push (cons cluster cluster) runs)))))

;;; Directories: 32-byte entries.  A long name is stored in extra entries
;;; BEFORE the ordinary one, last piece first, each marked with the
;;; otherwise impossible attributes 0Fh so that old programs skip them,
;;; holding 13 UTF-16 characters and a checksum of the short name they
;;; belong to (so that an old DOS which deleted or renamed the short entry
;;; without knowing about them leaves orphans that can be recognised).
(defstruct entry name long attributes size cluster date time)

(defun short-name (bytes e)
  "The 8.3 name in the entry at E, as DIR would show it."
  (let* ((flags (u8 bytes (+ e 12)))    ; Windows NT: bit 3 = base in lower case, 4 = extension
         (base (ascii bytes e 8)) (extension (ascii bytes (+ e 8) 3)))
    (when (= (u8 bytes e) 5)            ; a real first character E5h is stored as 05h
      (setf base (concatenate 'string (string (code-char #xE5)) (subseq base 1))))
    (when (logbitp 3 flags) (setf base (string-downcase base)))
    (when (logbitp 4 flags) (setf extension (string-downcase extension)))
    (if (string= extension "") base (format nil "~A.~A" base extension))))

(defun short-name-checksum (bytes e)
  (let ((sum 0))
    (dotimes (i 11 sum)                 ; rotate right one bit, add the next byte
      (setf sum (logand #xFF (+ (ash sum -1) (ash (logand sum 1) 7) (u8 bytes (+ e i))))))))

(defun long-name-piece (bytes e)
  "The (up to 13) characters in the long-name entry at E."
  (let ((characters '()))
    (dolist (offset '(1 3 5 7 9 14 16 18 20 22 24 28 30))
      (let ((unit (u16 bytes (+ e offset))))
        (when (zerop unit) (return))    ; the name ends with 0000, then FFFF padding
        (push (if (< unit char-code-limit) (code-char unit) #\?) characters)))
    (coerce (nreverse characters) 'string)))

(defun parse-directory (bytes &optional (fat-type 16))
  "Decode a directory: a list of ENTRY, in order, without the deleted ones."
  (let ((pieces '()) (entries '()))     ; pieces: (sequence-number checksum . text), last first
    (loop for e from 0 below (- (length bytes) 31) by 32
          for first = (u8 bytes e) and attributes = (u8 bytes (+ e 11))
          do (cond ((zerop first) (return))             ; never used: the directory ends here
                   ((= first #xE5) (setf pieces '()))   ; deleted
                   ((= (logand attributes #x3F) #x0F)
                    (when (logbitp 6 first) (setf pieces '()))  ; the last piece comes first
                    (push (list* (logand first #x3F) (u8 bytes (+ e 13)) (long-name-piece bytes e))
                          pieces))
                   (t (let ((checksum (short-name-checksum bytes e)))
                        (push (make-entry
                               :name (if (logbitp 3 attributes)
                                         (ascii bytes e 11)     ; a volume label: no dot
                                         (short-name bytes e))
                               ;; PIECES is now in ascending order: accept it only if it
                               ;; is 1, 2, ... n and every piece is for this short name.
                               :long (and pieces
                                          (loop for (number sum) in pieces and expected from 1
                                                always (and (= number expected) (= sum checksum)))
                                          (apply #'concatenate 'string (mapcar #'cddr pieces)))
                               :attributes attributes :size (u32 bytes (+ e 28))
                               :cluster (logior (u16 bytes (+ e 26))
                                                (if (= fat-type 32) (ash (u16 bytes (+ e 20)) 16) 0))
                               :date (u16 bytes (+ e 24)) :time (u16 bytes (+ e 22)))
                              entries)
                        (setf pieces '())))))
    (nreverse entries)))

(defun entry-directory-p (entry) (logbitp 4 (entry-attributes entry)))
(defun entry-label-p (entry) (logbitp 3 (entry-attributes entry)))
(defun attribute-letters (attributes)
  (map 'string (lambda (letter bit) (if (logbitp bit attributes) letter #\-)) "RHSDA" '(0 1 2 4 5)))
(defun date-text (date)                 ; 7 bits of years since 1980, 4 of month, 5 of day
  (if (zerop date)
      "          "
      (format nil "~4,'0D-~2,'0D-~2,'0D" (+ 1980 (ldb (byte 7 9) date)) (ldb (byte 4 5) date)
              (ldb (byte 5 0) date))))
(defun time-text (time)                 ; 5 bits of hours, 6 of minutes, 5 of seconds / 2
  (format nil "~2,'0D:~2,'0D:~2,'0D" (ldb (byte 5 11) time) (ldb (byte 6 5) time)
          (* 2 (ldb (byte 5 0) time))))

;;; ------------------------------------------------------------------------
;;; A volume: a BPB and a function that reads sectors, however it does it.

(defstruct volume
  name                                  ; "D:" or a file name, for the title
  reader                                ; (funcall reader sector count bytes-per-sector) -> bytes
  bpb
  (fat-cache (make-hash-table)))        ; pieces of the FAT already read

(defun read-sectors (volume sector &optional (count 1))
  (let* ((bpb (volume-bpb volume)) (count (min count (- (bpb-total-sectors bpb) sector))))
    (unless (and (>= sector 0) (plusp count))
      (error "Sector ~D is not on the volume" sector))
    (funcall (volume-reader volume) sector count (bpb-bytes-per-sector bpb))))

(defun open-volume (name reader)
  "Read the boot sector through READER and make a volume of it."
  (let ((bpb (parse-bpb (funcall reader 0 1 512))))
    (unless bpb (error "~A does not hold a FAT file system" name))
    (make-volume :name name :reader reader :bpb bpb)))

(defconstant +fat-piece+ 8192)          ; the FAT is read and kept in pieces of this many bytes

(defun volume-fat-entry (volume cluster)
  "The FAT's entry for CLUSTER, read from the first copy of the FAT."
  (let* ((bpb (volume-bpb volume)) (type (bpb-fat-type bpb))
         ;; A FAT12 entry can straddle two sectors, but a whole FAT12 FAT is
         ;; under 6144 bytes, so it never straddles two pieces.
         (piece (floor (ecase type (12 (floor (* 3 cluster) 2)) (16 (* 2 cluster)) (32 (* 4 cluster)))
                       +fat-piece+))
         (sectors (floor +fat-piece+ (bpb-bytes-per-sector bpb)))
         (bytes (or (gethash piece (volume-fat-cache volume))
                    (setf (gethash piece (volume-fat-cache volume))
                          (read-sectors volume (+ (bpb-reserved bpb) (* piece sectors))
                                        (min sectors (- (bpb-fat-size bpb) (* piece sectors))))))))
    (fat-entry bytes type cluster (* piece +fat-piece+))))

(defun cluster-chain (volume start &optional (limit (bpb-clusters (volume-bpb volume))))
  "The clusters of the file that starts at START, by following the FAT.
Second value: :END if the chain ends as it should, :EMPTY if START is 0 (a
file with nothing in it); otherwise what stopped it -- :FREE, :BAD,
:RESERVED (it runs into such an entry), :RANGE (a cluster number that does
not exist) or :LIMIT (longer than LIMIT: a loop, surely)."
  (let* ((chain '()) (bpb (volume-bpb volume)) (type (bpb-fat-type bpb)))
    (if (zerop start)
        (values '() :empty)
        (loop for cluster = start then (volume-fat-entry volume cluster)
              for count from 0
              for kind = (fat-entry-kind type cluster)
              do (cond ((not (eq kind :next)) (return (values (nreverse chain) kind)))
                       ((>= cluster (+ 2 (bpb-clusters bpb))) (return (values (nreverse chain) :range)))
                       ((>= count limit) (return (values (nreverse chain) :limit)))
                       (t (push cluster chain)))))))

(defun directory-bytes (volume cluster)
  "The contents of the directory that starts at CLUSTER; 0 is the root."
  (let ((bpb (volume-bpb volume)))
    (if (and (zerop cluster) (/= (bpb-fat-type bpb) 32))
        ;; Before FAT32 the root directory is not a file: it has a fixed
        ;; place and size between the FATs and the first cluster.
        (read-sectors volume (bpb-root-sector bpb) (bpb-root-sectors bpb))
        (let* ((chain (cluster-chain volume (if (zerop cluster) (bpb-root-cluster bpb) cluster) 4096))
               (size (* (bpb-sectors-per-cluster bpb) (bpb-bytes-per-sector bpb)))
               (bytes (make-array (* size (length chain)) :element-type '(unsigned-byte 8))))
          (loop for c in chain for at from 0 by size
                do (replace bytes (read-sectors volume (cluster-sector bpb c)
                                                (bpb-sectors-per-cluster bpb))
                            :start1 at))
          bytes))))

(defun read-directory (volume cluster)
  (parse-directory (directory-bytes volume cluster) (bpb-fat-type (volume-bpb volume))))

(defun fat-summary (volume cells)
  "Count the clusters.  Returns FREE USED BAD and a usage map: a vector of
at most CELLS elements, each the fraction in use of an equal share of the
volume (or :BAD if a bad cluster is among them)."
  (let* ((bpb (volume-bpb volume)) (clusters (bpb-clusters bpb)) (cells (min cells clusters))
         (used (make-array cells :initial-element 0)) (total (make-array cells :initial-element 0))
         (bad (make-array cells :initial-element nil))
         (free-count 0) (used-count 0) (bad-count 0))
    (dotimes (i clusters)
      (let ((cell (floor (* i cells) clusters)))
        (incf (aref total cell))
        (case (fat-entry-kind (bpb-fat-type bpb) (volume-fat-entry volume (+ i 2)))
          (:free (incf free-count))
          (:bad (incf bad-count) (setf (aref bad cell) t))
          (t (incf used-count) (incf (aref used cell))))))
    (values free-count used-count bad-count
            (map 'vector (lambda (u n b) (if b :bad (/ u n))) used total bad))))

;;; A disk image in a file: sectors by position.  If the image is of a
;;; whole disk, the volume is its first partition.
(defun image-reader (pathname)
  (flet ((read-at (position bytes)
           (with-open-file (stream pathname :element-type '(unsigned-byte 8))
             (let ((buffer (make-array bytes :element-type '(unsigned-byte 8) :initial-element 0)))
               (file-position stream position)
               (read-sequence buffer stream)
               buffer))))
    (let ((start (* 512 (partition-start (read-at 0 512)))))
      (lambda (sector count bytes-per-sector)
        (read-at (+ start (* sector bytes-per-sector)) (* count bytes-per-sector))))))

;;; ------------------------------------------------------------------------
;;; Reading a DOS drive.

(defun drive-reader (drive)
  "A reader for DOS drive DRIVE (0 = A:).  The library finds out which of
DOS's three ways of reading sectors works on it (src/disk.lisp)."
  (drive-read-method drive)             ; no such drive, or unreadable: an error now
  (lambda (sector count bytes-per-sector)
    (dos-read-sectors drive sector :count count :bytes-per-sector bytes-per-sector)))

(defun open-named (name)
  "NAME: NIL for the current drive, a drive letter (\"C\", \"c:\"), or a disk image's file name."
  (let ((name (and name (string name))))
    (cond ((and name (> (length name) 2)) (open-volume name (image-reader name)))
          (t (let ((drive (if (and name (plusp (length name)))
                              (- (char-code (char-upcase (char name 0))) 65)
                              (current-drive))))
               (unless (<= 0 drive 25) (error "~S is not a drive letter" name))
               (open-volume (format nil "~C:" (code-char (+ 65 drive))) (drive-reader drive)))))))

;;; ------------------------------------------------------------------------
;;; The screen.  Row 0 is a title, rows 1 to 23 the view, row 24 the keys.

(defvar *volume*)
(defvar *view*)                         ; :boot :directory :file :sector :fat
(defvar *message*)                      ; shown once on the bottom line, instead of the keys
;; the directory view
(defvar *path*)                         ; where we came through: ((name cluster selection) ...)
(defvar *cluster*)                      ; the directory shown; 0 = the root
(defvar *entries*)
(defvar *selection*)
(defvar *file*)                         ; the entry shown by the :file view
;; the sector view
(defvar *sector*)
(defvar *sector-row*)                   ; first row of the sector on the screen
;; the FAT view
(defvar *summary*)                      ; (free used bad map), once worked out

(defconstant +title+ #x1F)              ; white on blue
(defconstant +plain+ #x07)
(defconstant +label+ #x0B)              ; cyan
(defconstant +bright+ #x0F)
(defconstant +selected+ #x30)           ; black on cyan
(defconstant +keys+ #x70)               ; black on grey
(defconstant +alarm+ #x4F)              ; white on red

(defun screen-text (string)
  "STRING with anything the text screen cannot show replaced."
  (substitute-if #\? (lambda (c) (> (char-code c) 255)) string))
(defun put (x y attribute control &rest arguments)
  (text-put x y (screen-text (apply #'format nil control arguments)) attribute))
(defun put-line (y attribute control &rest arguments)
  "A whole row: the text, cut or padded to the width of the screen."
  (let ((text (screen-text (apply #'format nil control arguments))))
    (text-put 0 y (format nil "~80A" (subseq text 0 (min 80 (length text)))) attribute)))
(defun field (y label control &rest arguments)
  (put 2 y +label+ "~A" label)
  (apply #'put 26 y +bright+ control arguments))

(defun hex-rows (bytes start rows first-y)
  "ROWS rows of 16 bytes from offset START, in hexadecimal and as characters."
  (dotimes (row rows)
    (let ((offset (+ start (* 16 row))) (y (+ first-y row)))
      (when (< offset (length bytes))
        (let ((line (coerce (subseq bytes offset (min (length bytes) (+ offset 16))) 'list)))
          (put 2 y +label+ "~4,'0X" offset)
          (put 9 y +plain+ "~{~2,'0X~^ ~}" (subseq line 0 (min 8 (length line))))
          (put 34 y +plain+ "~{~2,'0X~^ ~}" (nthcdr 8 line))
          (put 60 y +bright+ "~A"
               (map 'string (lambda (b) (if (<= 32 b 126) (code-char b) #\.)) line)))))))

;;; The boot sector.
(defun draw-boot ()
  (let* ((bpb (volume-bpb *volume*)) (y 1)
         (bytes (* (bpb-total-sectors bpb) (bpb-bytes-per-sector bpb))))
    (flet ((line (label control &rest arguments)
             (apply #'field (incf y) label control arguments)))
      (put 2 1 +bright+ "Sector 0, the boot sector, says:")
      (line "OEM name" "~S" (bpb-oem bpb))
      (line "Bytes per sector" "~D" (bpb-bytes-per-sector bpb))
      (line "Sectors per cluster" "~D  (clusters of ~:D bytes)" (bpb-sectors-per-cluster bpb)
            (* (bpb-sectors-per-cluster bpb) (bpb-bytes-per-sector bpb)))
      (line "Reserved sectors" "~D" (bpb-reserved bpb))
      (line "FATs" "~D" (bpb-fats bpb))
      (line "Root directory entries" "~D~:[~;  (the root is an ordinary chain of clusters)~]"
            (bpb-root-entries bpb) (= (bpb-fat-type bpb) 32))
      (line "Total sectors" "~:D  (~:D bytes, ~D MB)" (bpb-total-sectors bpb) bytes
            (round bytes 1048576))
      (line "Sectors per FAT" "~:D" (bpb-fat-size bpb))
      (line "Media descriptor" "~2,'0Xh" (bpb-media bpb))
      (line "Geometry" "~D sectors per track, ~D heads, ~:D hidden sectors"
            (bpb-sectors-per-track bpb) (bpb-heads bpb) (bpb-hidden bpb))
      (line "Volume label" "~:[(none)~;~:*~S~]" (and (string/= (bpb-label bpb) "") (bpb-label bpb)))
      (if (bpb-serial bpb)
          (line "Serial number" "~4,'0X-~4,'0X" (ash (bpb-serial bpb) -16)
                (logand (bpb-serial bpb) #xFFFF))
          (line "Serial number" "(none)"))
      (line "File system name" "~S  (just text: the type is decided below)" (bpb-type-name bpb))
      (incf y)
      (put 2 (incf y) +bright+ "From which it follows:")
      (line "FAT type" "FAT~D: ~:D clusters  (< 4,085: 12; < 65,525: 16)"
            (bpb-fat-type bpb) (bpb-clusters bpb))
      (line "First FAT" "sector ~:D" (bpb-reserved bpb))
      (if (= (bpb-fat-type bpb) 32)
          (line "Root directory" "cluster ~:D, sector ~:D" (bpb-root-cluster bpb)
                (cluster-sector bpb (bpb-root-cluster bpb)))
          (line "Root directory" "sector ~:D, ~D sectors" (bpb-root-sector bpb) (bpb-root-sectors bpb)))
      (line "Data (cluster 2)" "sector ~:D" (bpb-data-sector bpb)))))

;;; Directories.
(defconstant +list-rows+ 19)

(defun path-text ()
  (format nil "~A\\~{~A~^\\~}" (volume-name *volume*) (reverse (mapcar #'first *path*))))

(defun enter-directory (cluster &optional (selection 0))
  (setf *entries* (read-directory *volume* cluster) *cluster* cluster *selection* selection))

(defun draw-directory ()
  (put 2 1 +bright+ "~A" (path-text))
  (put 50 1 +label+ "~28@A" (format nil "~D entr~:@P, ~:[cluster ~D~;the root~]" (length *entries*)
                                    (zerop *cluster*) *cluster*))
  (put-line 2 +keys+ "  Name          Attr         Size   Cluster  Date       Time   Long name")
  (let ((top (* +list-rows+ (floor *selection* +list-rows+))))        ; a page at a time
    (loop for entry in (nthcdr top *entries*) for row from 0 below +list-rows+
          do (put-line (+ 3 row) (if (= (+ top row) *selection*) +selected+ +plain+)
                       "  ~12A  ~A ~12@A  ~8D  ~A ~A  ~@[~A~]"
                       (entry-name entry) (attribute-letters (entry-attributes entry))
                       (cond ((entry-label-p entry) "<LABEL>")
                             ((entry-directory-p entry) "<DIR>")
                             (t (format nil "~:D" (entry-size entry))))
                       (entry-cluster entry) (date-text (entry-date entry))
                       (subseq (time-text (entry-time entry)) 0 5) (entry-long entry))))
  (let ((entry (nth *selection* *entries*)))
    (cond ((null entry) (put 2 4 +plain+ "(an empty directory)"))
          ((entry-long entry) (put 2 23 +label+ "Long name: ~A" (entry-long entry)))
          (t (put 2 23 +label+ "No long name is stored for ~A." (entry-name entry))))))

(defun leave-directory ()
  (when *path*
    (destructuring-bind (name cluster selection) (first *path*)
      (declare (ignore name))
      (enter-directory cluster selection)
      (pop *path*))))

(defun open-entry ()
  "Enter on the selected entry: go into a directory, or show a file."
  (let ((entry (nth *selection* *entries*)))
    (cond ((or (null entry) (entry-label-p entry) (string= (entry-name entry) ".")))
          ((string= (entry-name entry) "..") (leave-directory))
          ((entry-directory-p entry)
           (let ((here (list (entry-name entry) *cluster* *selection*)))
             (enter-directory (entry-cluster entry))    ; which may fail: then stay put
             (push here *path*)))
          (t (setf *file* entry *view* :file)))))

;;; One file: where the FAT says it is.
(defun draw-file ()
  (let* ((bpb (volume-bpb *volume*)) (entry *file*)
         (cluster-bytes (* (bpb-sectors-per-cluster bpb) (bpb-bytes-per-sector bpb)))
         (needed (ceiling (entry-size entry) cluster-bytes)))
    (multiple-value-bind (chain end) (cluster-chain *volume* (entry-cluster entry))
      (put 2 1 +bright+ "~A~:[~;\\~]~A" (path-text) *path* (entry-name entry))
      (field 2 "Long name" "~:[(none)~;~:*~A~]" (entry-long entry))
      (field 3 "Size" "~:D bytes, ~A, written ~A ~A" (entry-size entry)
             (attribute-letters (entry-attributes entry)) (date-text (entry-date entry))
             (time-text (entry-time entry)))
      (field 4 "Clusters" "~:D of ~:D bytes~:[  (but the size calls for ~:D!)~;~*~], in ~D run~:P"
             (length chain) cluster-bytes (= needed (length chain)) needed
             (length (chain-runs chain)))
      (field 5 "The chain ends" "~A"
             (ecase end
               (:end "with an end-of-file mark, as it should")
               (:empty "before it starts: the file has no clusters")
               (:free "in a FREE cluster: the file is damaged")
               (:bad "in a cluster marked BAD")
               (:reserved "in a reserved FAT value")
               (:range "in a cluster number that is not on the volume")
               (:limit "nowhere: it goes round in a loop")))
      ;; The chain, as runs of consecutive clusters, on up to four lines.
      (put 2 7 +label+ "Cluster chain")
      (let ((x 26) (y 7))
        (loop for ((first . last) . more) on (chain-runs chain)
              for text = (if (= first last) (format nil "~D" first) (format nil "~D-~D" first last))
              do (when (> (+ x (length text)) 76) (setf x 26) (incf y))
                 (when (and (= y 10) more (> (+ x (length text)) 60))
                   (put x y +plain+ "... to ~D" (car (last chain)))
                   (return))
                 (put x y +bright+ "~A" text)
                 (incf x (+ 2 (length text)))))
      (when chain
        (let ((sector (cluster-sector bpb (first chain))))
          (field 12 "First sector" "~:D  (Enter shows it in the sector view)" sector)
          (hex-rows (read-sectors *volume* sector) 0 10 14))))))

;;; Sectors.
(defconstant +hex-rows+ 20)

(defun show-sector (sector)
  (setf *sector* (max 0 (min sector (1- (bpb-total-sectors (volume-bpb *volume*)))))
        *sector-row* 0 *view* :sector))

(defun draw-sector ()
  (let ((bpb (volume-bpb *volume*)))
    (put 2 1 +bright+ "Sector ~:D of ~:D" *sector* (bpb-total-sectors bpb))
    (put 36 1 +label+ "~42@A" (sector-region bpb *sector*))
    (hex-rows (read-sectors *volume* *sector*) (* 16 *sector-row*) +hex-rows+ 3)))

(defun parse-sector-number (text bpb)
  "What G was told: a number, xHEX, or cN for cluster N.  NIL if it makes no sense."
  (let ((text (remove #\, (string-trim " " text))))
    (flet ((parse (start radix)
             (ignore-errors (parse-integer text :start start :radix radix))))
      (cond ((string= text "") nil)
            ((char-equal (char text 0) #\x) (parse 1 16))
            ((char-equal (char text 0) #\c)
             (let ((cluster (parse 1 10)))
               (and cluster (< 1 cluster (+ 2 (bpb-clusters bpb))) (cluster-sector bpb cluster))))
            (t (parse 0 10))))))

(defun go-to-sector ()
  (put-line 24 +keys+ " Go to sector (a number, xHEX, or cN for cluster N): ")
  (show-text-cursor)
  (let* ((text (text-input 54 24 20 :attribute +keys+))
         (sector (and text (parse-sector-number text (volume-bpb *volume*)))))
    (hide-text-cursor)
    (cond (sector (show-sector sector))
          ((and text (string/= text "")) (setf *message* (format nil "~S is not a sector number" text))))))

;;; The FAT, summed up.
(defconstant +map-columns+ 64)
(defconstant +map-rows+ 12)

(defun draw-fat ()
  (let* ((bpb (volume-bpb *volume*))
         (cluster-bytes (* (bpb-sectors-per-cluster bpb) (bpb-bytes-per-sector bpb))))
    (unless *summary*
      (put 2 1 +bright+ "Reading the FAT: ~:D entries..." (bpb-clusters bpb))
      (setf *summary* (multiple-value-list (fat-summary *volume* (* +map-columns+ +map-rows+))))
      (text-fill 0 1 80 1 +plain+))
    (destructuring-bind (free used bad map) *summary*
      (put 2 1 +bright+ "A ~D-bit FAT: ~:D clusters of ~:D bytes; ~D cop~:@P of ~:D sectors"
           (bpb-fat-type bpb) (bpb-clusters bpb) cluster-bytes (bpb-fats bpb) (bpb-fat-size bpb))
      (flet ((line (y label count)
               (field y label "~10:D cluster~:P ~16:D bytes  ~5,1F%" count (* count cluster-bytes)
                      (/ (* 100.0 count) (bpb-clusters bpb)))))
        (line 3 "Used" used)
        (line 4 "Free" free)
        (line 5 "Marked bad" bad))
      (put 2 7 +label+ "Where the used clusters are (each cell is ~:D cluster~:P):"
           (ceiling (bpb-clusters bpb) (length map)))
      (loop for cell across map for i from 0
            do (multiple-value-bind (row column) (floor i +map-columns+)
                 (text-put (+ 8 column) (+ 9 row)
                           (string (cond ((eq cell :bad) #\B)
                                         ((zerop cell) (code-char 250))         ; a small dot
                                         ((< cell 1/3) (code-char 176))         ; light shade
                                         ((< cell 2/3) (code-char 177))
                                         ((< cell 1) (code-char 178))
                                         (t (code-char 219))))                  ; solid
                           (cond ((eq cell :bad) +alarm+) ((zerop cell) 8) (t 3)))))
      (put 8 22 +plain+ "~C free   ~C ~C ~C partly used   ~C full   B has a bad cluster"
           (code-char 250) (code-char 176) (code-char 177) (code-char 178) (code-char 219)))))

;;; All together.
(defun draw ()
  (let ((bpb (volume-bpb *volume*)))
    (put-line 0 +title+ " FATVIEW   ~A  FAT~D~@[  ~A~]" (volume-name *volume*)
              (bpb-fat-type bpb) (and (string/= (bpb-label bpb) "") (bpb-label bpb)))
    (put 59 0 +title+ "~20@A" (ecase *view*
                                (:boot "Boot sector") (:directory "Directory") (:file "File")
                                (:sector "Sector") (:fat "FAT summary")))
    (text-fill 0 1 80 23 +plain+)
    ;; A view that cannot read what it needs says so, and the program goes on.
    (handler-case (ecase *view*
                    (:boot (draw-boot)) (:directory (draw-directory)) (:file (draw-file))
                    (:sector (draw-sector)) (:fat (draw-fat)))
      (error (e) (put 2 12 +alarm+ " ~A " e)))
    (if *message*
        (put-line 24 +alarm+ " ~A" (shiftf *message* nil))
        (put-line 24 +keys+ " B Boot  D Dir  S Sector  F FAT  Tab next   ~A~69TEsc/Q quit"
                  (ecase *view*
                    ((:boot :fat) "")
                    (:directory (format nil "~C~C Enter open  BkSp up" (code-char 24) (code-char 25)))
                    (:file "Enter sector  BkSp back")
                    (:sector "PgUp/PgDn  G go to"))))))

(defun handle-key (key)
  "Act on KEY.  Returns true when it is time to leave."
  (let* ((bpb (volume-bpb *volume*))
         (rows (floor (bpb-bytes-per-sector bpb) 16))
         (key (if (characterp key) (char-downcase key) key)))
    (flet ((select (n) (setf *selection* (max 0 (min n (1- (length *entries*)))))))
      (cond ((member key (list (code-char 27) #\q)) (return-from handle-key t))
            ((eql key #\b) (setf *view* :boot))
            ((eql key #\d) (setf *view* :directory))
            ((eql key #\s) (setf *view* :sector))
            ((eql key #\f) (setf *view* :fat))
            ((eql key #\Tab)
             (setf *view* (ecase *view* (:boot :directory) ((:directory :file) :sector)
                            (:sector :fat) (:fat :boot))))
            ((eq *view* :directory)
             (case key
               (:up (select (1- *selection*)))
               (:down (select (1+ *selection*)))
               (:page-up (select (- *selection* +list-rows+)))
               (:page-down (select (+ *selection* +list-rows+)))
               (:home (select 0))
               (:end (select (length *entries*)))
               ((#\Return :right) (open-entry))
               ((#\Backspace :left) (leave-directory))))
            ((eq *view* :file)
             (case key
               (#\Return (let ((chain (cluster-chain *volume* (entry-cluster *file*) 1)))
                           (when chain (show-sector (cluster-sector bpb (first chain))))))
               ((#\Backspace :left) (setf *view* :directory))))
            ((eq *view* :sector)
             (case key
               (:page-up (show-sector (1- *sector*)))
               (:page-down (show-sector (1+ *sector*)))
               (:home (show-sector 0))
               (:end (show-sector (1- (bpb-total-sectors bpb))))
               (:up (setf *sector-row* (max 0 (1- *sector-row*))))
               (:down (setf *sector-row* (max 0 (min (1+ *sector-row*) (- rows +hex-rows+)))))
               (#\g (go-to-sector)))))
      nil)))

(defun fatview (volume)
  (setf *volume* volume *view* :boot *message* nil *path* '() *file* nil
        *sector* 0 *sector-row* 0 *summary* nil)
  (enter-directory 0)
  (let ((saved (save-text-screen)))
    (hide-text-cursor)
    (unwind-protect
         (loop
           (draw)
           (when (handler-case (handle-key (get-key))
                   (error (e) (setf *message* (format nil "~A" e)) nil))
             (return)))
      (restore-text-screen saved))))

(defun main (&optional drive)
  "A disk sector viewer and FAT explorer."
  ;; With no disk in the drive DOS would ask "Abort, Retry, Fail?": have it fail instead.
  (catch-critical-errors)
  (unwind-protect
       (handler-case (fatview (open-named drive))
         (error (e) (format t "~&fatview: ~A~%" e)))
    (release-critical-errors))
  (values))

(doslib-examples:define-example "fatview" 'main
  :category :system
  :description "A disk sector viewer and FAT explorer.")
