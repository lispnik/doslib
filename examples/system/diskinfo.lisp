;;;; Reading a disk the hard way.  Sectors are read through
;;;; the BIOS (INT 13h, function 42h) into memory below 1MB, and Lisp decodes
;;;; what is in them: the partition table, the FAT boot sector, directories
;;;; (including the hidden entries that hold long file names) and the chain
;;;; of clusters that makes up one file.  DOS is not asked for anything.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.diskinfo
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.diskinfo)

(defvar *buffer* nil)                   ; DOS memory: a 16-byte request packet, then
                                        ; sector data.  Allocated when the example runs.
(defun read-sectors (drive lba &optional (count 1))
  (let ((packet (linear *buffer*)))
    (setf (sap-ref-16 (int-sap packet) 0) 16 (sap-ref-16 (int-sap packet) 2) count
          (sap-ref-16 (int-sap packet) 4) 512 (sap-ref-16 (int-sap packet) 6) *buffer*
          (sap-ref-32 (int-sap packet) 8) lba (sap-ref-32 (int-sap packet) 12) 0)
    (when (logbitp 0 (nth-value 6 (real-int #x13 :ax #x4200 :dx drive :ds *buffer* :si 0)))
      (error "BIOS could not read sector ~D of drive ~2,'0Xh" lba drive))
    (linear *buffer* 512)))
             ; where the data now is
(defun text (address length)
  (string-right-trim " " (map 'string #'code-char
                              (loop for i below length collect (peek (+ address i))))))
(defun entries (address count)
  "Decode COUNT 32-byte directory entries: list of (name long-name attributes cluster size)."
  (let ((long '()) (result '()))
    (dotimes (i count (nreverse result))
      (let* ((e (+ address (* 32 i))) (first (peek e)) (attributes (peek (+ e 11))))
        (cond ((zerop first) (return (nreverse result)))
              ((= first #xE5) (setf long '()))                  ; deleted
              ((= attributes #x0F)                              ; a piece of a long name
               (push (map 'string #'code-char
                          (remove-if (lambda (c) (or (zerop c) (= c #xFFFF)))
                                     (loop for o in '(1 3 5 7 9 14 16 18 20 22 24 28 30)
                                           collect (peek16 (+ e o)))))
                     long))
              (t (let ((base (text e 8)) (extension (text (+ e 8) 3)))
                   (push (list (if (string= extension "") base (format nil "~A.~A" base extension))
                               (and long (apply #'concatenate 'string long))
                               attributes (peek16 (+ e 26)) (peek32 (+ e 28)))
                         result)
                   (setf long '()))))))))
(defun diskinfo (&optional (drive #x81))
  (setf *buffer* (dos-alloc 8192))
  (let* ((mbr (read-sectors drive 0))
         (entry (+ mbr 446))
         (start (peek32 (+ entry 8))))
    (format t "~&Drive ~2,'0Xh sector 0, the master boot record (signature ~4,'0X):~%"
            drive (peek16 (+ mbr 510)))
    (format t "  partition 1: type ~2,'0Xh~:[~;, bootable~], starts at sector ~D, ~:D sectors (~D MB)~%"
            (peek (+ entry 4)) (= (peek entry) #x80) start (peek32 (+ entry 12))
            (floor (peek32 (+ entry 12)) 2048))
    (let* ((boot (read-sectors drive start))
           (cluster-sectors (peek (+ boot 13))) (reserved (peek16 (+ boot 14)))
           (fats (peek (+ boot 16))) (root-entries (peek16 (+ boot 17)))
           (fat-sectors (peek16 (+ boot 22)))
           (root (+ start reserved (* fats fat-sectors)))
           (data (+ root (ceiling (* 32 root-entries) 512))))
      (format t "Sector ~D, the boot sector: formatted by ~S, ~A, label ~S~%"
              start (text (+ boot 3) 8) (text (+ boot 54) 8) (text (+ boot 43) 11))
      (format t "  ~D-byte sectors, ~D per cluster, ~D FATs of ~D sectors, ~D root entries~%"
              (peek16 (+ boot 11)) cluster-sectors fats fat-sectors root-entries)
      (labels ((cluster-lba (cluster) (+ data (* (- cluster 2) cluster-sectors)))
               (next-cluster (cluster)                          ; look it up in the FAT
                 (let ((fat (read-sectors drive (+ start reserved (floor (* 2 cluster) 512)))))
                   (peek16 (+ fat (mod (* 2 cluster) 512)))))
               (show (list limit)
                 (loop for (name long attributes cluster size) in list repeat limit
                       do (format t "  ~12A ~:[~10:D~;~*     <DIR>~]  cluster ~5D~@[  long name ~S~]~%"
                                  name (logbitp 4 attributes) size cluster long))
                 (when (> (length list) limit)
                   (format t "  ... and ~D more~%" (- (length list) limit)))))
        (let* ((root-list (entries (read-sectors drive root 4) 64))
               (directory (find-if (lambda (e) (and (logbitp 4 (third e))
                                                    (char/= (char (first e) 0) #\.)))
                                   root-list))
               (list root-list))
          (format t "Sector ~D, the root directory:~%" root)
          (show root-list 5)
          (when directory
            (setf list (entries (read-sectors drive (cluster-lba (fourth directory)) cluster-sectors)
                                (* 16 cluster-sectors)))
            (format t "Cluster ~D (sector ~D), directory ~A:~%" (fourth directory)
                    (cluster-lba (fourth directory)) (first directory))
            (show list 6))
          (let ((file (find "SBCL.EXE" list :key #'first :test #'string=)))
            (when file
              (let* ((chain (loop for c = (fourth file) then (next-cluster c)
                                  while (< 1 c #xFFF0) collect c))
                     (contiguous (loop for (a b) on chain always (or (null b) (= b (1+ a))))))
                (format t "SBCL.EXE: ~:D bytes in ~D clusters, ~D -> ~D -> ... -> ~D, ~:[fragmented~;contiguous~]~%"
                        (fifth file) (length chain) (first chain) (second chain)
                        (car (last chain)) contiguous)
                (format t "  its first bytes on disk: ~S (a DOS program), sector ~D~%"
                        (text (read-sectors drive (cluster-lba (first chain))) 2)
                        (cluster-lba (first chain)))))))))))

(defun main ()
  "Reading a disk the hard way."
  (diskinfo))

(doslib-examples:define-example "diskinfo" 'main
  :category :system
  :description "Reading a disk the hard way.")
