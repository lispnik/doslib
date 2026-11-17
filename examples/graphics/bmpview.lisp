;;;; An image viewer: reads a 256-colour Windows BMP file from
;;;; disk with ordinary Lisp streams, loads its palette into the VGA card and
;;;; copies the pixels to the screen (BMP files store the bottom row first).
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.bmpview
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.bmpview)

(defun u16 (bytes offset) (+ (aref bytes offset) (ash (aref bytes (1+ offset)) 8)))
(defun u32 (bytes offset) (+ (u16 bytes offset) (ash (u16 bytes (+ offset 2)) 16)))
(defun view (filename)
  (let ((bytes (with-open-file (in filename :element-type '(unsigned-byte 8))
                 (let ((data (make-array (file-length in) :element-type '(unsigned-byte 8))))
                   (read-sequence data in)
                   data))))
    (unless (and (= (aref bytes 0) 66) (= (aref bytes 1) 77) (= (u16 bytes 28) 8))
      (error "~A is not an 8-bit BMP file" filename))
    (let* ((pixels (u32 bytes 10)) (width (u32 bytes 18)) (height (u32 bytes 22))
           (palette (+ 14 (u32 bytes 14))) (row-bytes (* 4 (ceiling width 4))))
      (video-mode #x13)
      (dotimes (i (/ (- pixels palette) 4))             ; entries are B G R 0
        (set-palette i (ash (aref bytes (+ palette (* 4 i) 2)) -2)
                     (ash (aref bytes (+ palette (* 4 i) 1)) -2)
                     (ash (aref bytes (+ palette (* 4 i))) -2)))
      (dotimes (y (min height 200))
        (let ((row (+ pixels (* row-bytes (- height y 1)))))
          (dotimes (x (min width 320))
            (setf (sap-ref-8 *vga* (+ x (* 320 y))) (aref bytes (+ row x))))))
      (finish))))

(defun main ()
  "An image viewer."
  (view (doslib-examples:data-file "picture.bmp")))

(doslib-examples:define-example "bmpview" 'main
  :category :graphics
  :description "An image viewer.")
