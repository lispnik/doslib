;;;; Watching the garbage collector.  SBCL's collector divides
;;;; the heap into 4KB pages and keeps a table saying, for each one, which
;;;; generation its objects belong to and how full it is.  This reads that
;;;; table straight out of the runtime's memory and draws it: one small
;;;; square per page, coloured by generation, brighter when fuller.  A
;;;; workload then allocates garbage and keeps a little of it, so you can
;;;; see new pages fill (generation 0), survivors move up, and space come back.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.gcviz
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.gcviz)

(defun runtime-word (name) (sap-ref-32 (int-sap (find-foreign-symbol-address name)) 0))
(defvar *gcs* 0)
(defvar *gen-colours* '((63 16 8) (63 44 4) (56 60 8) (16 56 16)      ; 0 red 1 orange 2 yellow 3 green
                        (8 52 56) (20 28 63) (30 30 40)))
              ; 4 cyan 5 blue 6 "pseudo-static" grey
(defun set-colours ()
  (set-palette 1 5 5 7)                 ; a free page
  (set-palette 15 63 63 63)
  (loop for (r g b) in *gen-colours* for gen from 0
        do (dotimes (level 4)           ; four brightnesses per generation
             (flet ((v (c) (floor (* c (+ 4 (* 4 level))) 16)))
               (set-palette (+ 16 (* 4 gen) level) (v r) (v g) (v b))))))
(defun draw-heap ()
  (declare (optimize speed (safety 0)))
  (let* ((table (int-sap (runtime-word "page_table")))
         (pages (min (the fixnum (runtime-word "next_free_page")) (* 160 91)))
         (vga *vga*))
    (declare (fixnum pages))
    (dotimes (i (* 160 91))
      (let* ((flags (if (< i pages) (sap-ref-8 table (+ (* 8 i) 6)) 0))
             (colour (if (zerop flags) 1
                         (+ 16 (* 4 (min 6 (sap-ref-8 table (+ (* 8 i) 7))))
                            (min 3 (ash (sap-ref-16 table (+ (* 8 i) 4)) -9)))))
             (at (+ (* 2 (mod i 160)) (* 640 (floor i 160)))))
        (declare (fixnum flags colour at))
        (setf (sap-ref-8 vga at) colour (sap-ref-8 vga (+ at 320)) colour)
        (setf (sap-ref-8 vga (+ at 1)) (if (zerop flags) 0 colour))))
    pages))
(defun say (column row colour string)   ; BIOS teletype output works in graphics modes too
  (int86 #x10 :ax #x0200 :bx 0 :dx (+ (* 256 row) column))
  (loop for c across string do (int86 #x10 :ax (+ #x0E00 (char-code c)) :bx colour)))
(defun gcviz ()
  (push (lambda () (incf *gcs*)) sb-ext:*after-gc-hooks*)
  (setf (sb-ext:bytes-consed-between-gcs) (* 3 1024 1024))
  (video-mode #x13)
  (set-colours)
  (say 0 23 15 "generation:")
  (dotimes (gen 7) (say (+ 12 (* 3 gen)) 23 (+ 19 (* 4 gen)) (format nil "~D" gen)))
  (say 34 23 1 "free")
  (let ((keep (make-array 48 :initial-element nil)) (step 0))
    (loop until (key-pressed-p)
          do ;; the workload: lists and vectors, most dropped at once,
             ;; some kept for a while in a ring so that they age
             (dotimes (i 40)
               (let ((junk (loop repeat 600 collect (make-array 8 :initial-element i))))
                 (when (zerop (mod i 8))
                   (setf (aref keep (mod (incf step) 48)) junk))))
             (let ((pages (draw-heap)))
               ;; (40 columns in this mode, and writing the last one would scroll)
               (say 0 24 15 (format nil "~5D pages ~3D MB ~5D collections"
                                    pages (floor (sb-kernel:dynamic-usage) 1048576) *gcs*)))))
  (finish))

(defun main ()
  "Watching the garbage collector."
  (gcviz))

(doslib-examples:define-example "gcviz" 'main
  :category :graphics
  :description "Watching the garbage collector.")
