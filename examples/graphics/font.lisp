;;;; Redefining text-mode characters.  The VGA keeps its font in
;;;; memory plane 2; a few register writes make that plane visible at A000:0,
;;;; where glyphs can be overwritten, 32 bytes per character.  Here some
;;;; characters become picture tiles, drawn as rows of text in this file.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.font
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.font)

(defvar *text* (int-sap #xB8000))
(defun call-with-font-memory (function)  ; (the library's writers of the VGA's registers)
  (vga-sequencer 2 4) (vga-sequencer 4 7)               ; write plane 2, no odd/even addressing
  (vga-graphics 4 2) (vga-graphics 5 0) (vga-graphics 6 4)      ; read plane 2, map at A0000
  (funcall function)
  (vga-sequencer 2 3) (vga-sequencer 4 3)               ; back to the text-mode arrangement
  (vga-graphics 4 0) (vga-graphics 5 #x10) (vga-graphics 6 #x0E))
(defun define-glyph (code rows)         ; ROWS: 16 strings of 8, # is ink
  (loop for row in rows for y from 0
        do (setf (sap-ref-8 *vga* (+ (* 32 code) y))
                 (loop for c across row for bit downfrom 7
                       sum (if (char= c #\#) (ash 1 bit) 0)))))
(defvar *lambda-picture*                ; 32x32, to be cut into 4x2 characters
  '("................................" "................................"
    "....######......................" "....#######....................."
    ".......#####...................." "........#####..................."
    ".........#####.................." ".........######................."
    "..........######................" "..........#######..............."
    ".........########..............." ".........#########.............."
    "........#####.#####............." "........#####..#####............"
    ".......#####...#####............" ".......#####....#####..........."
    "......#####.....#####..........." "......#####......#####.........."
    ".....#####.......#####.........." ".....#####........#####........."
    "....#####.........#####........." "....#####..........#####........"
    "...#####...........#####........" "...#####............#####......."
    "..#####.............######..##.." "..#####..............#########.."
    ".#####................#######..." ".#####.................#####...."
    "................................" "................................"
    "................................" "................................"))
(defvar *invader* '("........" "........" "..#..#.." "...##..." "..####.." ".##.##.#" ".######." "#.####.#"
                    "#.#..#.#" "...##..." "..#..#.." ".#....#." "........" "........" "........" "........"))
(defvar *heart* '("........" "........" ".##..##." "########" "########" "########" "########" ".######."
                  ".######." "..####.." "..####.." "...##..." "...##..." "........" "........" "........"))
(defun put (x y char attribute)
  (setf (sap-ref-8 *text* (* 2 (+ x (* 80 y)))) char
        (sap-ref-8 *text* (1+ (* 2 (+ x (* 80 y))))) attribute))
(defun say (x y attribute string)
  (loop for c across string for i from x do (put i y (char-code c) attribute)))
(defun font-demo ()
  (int86 #x10 :ax #x0100 :cx #x2000)    ; hide the cursor
  (dotimes (y 25) (dotimes (x 80) (put x y 32 7)))
  (call-with-font-memory
   (lambda ()
     (dotimes (ty 2)                    ; characters 128-135: the lambda
       (dotimes (tx 4)
         (define-glyph (+ 128 tx (* 4 ty))
                       (loop for y below 16
                             collect (subseq (nth (+ y (* 16 ty)) *lambda-picture*)
                                             (* 8 tx) (* 8 (1+ tx)))))))
     (define-glyph 136 *invader*)
     (define-glyph 137 *heart*)))
  (say 20 2 #x0F "The VGA text font, redefined from Lisp")
  (loop for (x y colour) in '((8 6 #x0E) (24 6 #x0B) (40 6 #x0D) (56 6 #x0A) (68 6 #x0C))
        do (dotimes (ty 2) (dotimes (tx 4) (put (+ x tx) (+ y ty) (+ 128 tx (* 4 ty)) colour))))
  (dotimes (row 3)
    (dotimes (i 26) (put (+ 14 (* 2 i)) (+ 11 (* 2 row)) 136 (+ 9 (mod (+ i row) 6)))))
  (dotimes (i 60) (put (+ 10 i) 18 137 (if (evenp i) #x0C #x04)))
  (say 12 21 #x07 "Characters 128-137 are now pictures; everything else is still text.")
  (finish))

(defun main ()
  "Redefining text-mode characters."
  (font-demo))

(doslib-examples:define-example "font" 'main
  :category :graphics
  :description "Redefining text-mode characters.")
