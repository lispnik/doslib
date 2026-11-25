;;;; Rotating solids with filled faces: a cube and a ring, flat shaded or
;;;; texture mapped, in mode 13h (320 by 200, a byte a pixel).  The example
;;;; "wire3d" draws the edges of its solids; this one fills them in.
;;;;
;;;;   (doslib-examples:run "solid3d")
;;;; Space: flat shading, or a texture on the faces.  Enter: the cube, or the
;;;; ring.  Esc leaves.
;;;;
;;;; A solid is a list of corners and a list of four-cornered faces.  Every
;;;; frame, the corners are turned about two axes and projected: screen x is
;;;; x / z times a constant, which is all perspective is.  Then, for each
;;;; face:
;;;;
;;;; Which faces to draw.  A face that looks away from the eye is hidden by
;;;; the rest of the solid, and is left out ("back-face culling"): each face
;;;; has a normal, an arrow standing straight out of it, turned with the
;;;; corners, and the face looks away when that arrow points away from the
;;;; eye.  For the cube nothing more is needed: the faces of a convex solid
;;;; that look at the eye never cover one another.
;;;;
;;;; In what order.  The ring is not convex: the near side of it covers part
;;;; of the far side, and both look at the eye.  The faces are sorted by
;;;; their distance and drawn furthest first, so that nearer ones are painted
;;;; over further ones: the painter's algorithm.  It sorts whole faces by one
;;;; distance each (that of the middle), so it is right only where faces do
;;;; not pass through each other or overlap in awkward ways.  They do not
;;;; here, and it is enough; doing it for every pixel takes a Z buffer.
;;;;
;;;; How bright.  The cosine of the angle between the normal and the
;;;; direction of the light (their dot product) says how squarely the light
;;;; falls on the face, and picks one of 64 shades of the face's colour from
;;;; a ramp in the palette: "flat shading", one shade to a face.
;;;;
;;;; Filling.  A face is drawn as two triangles.  The filler walks down the
;;;; triangle's three edges, noting for every line of the screen the leftmost
;;;; and rightmost x that an edge passes (an edge is a straight line: its x
;;;; moves by the same amount each line down, one addition), and then fills
;;;; each line between the two.  An edge shared by two triangles comes out
;;;; the same in both, and the pixel on the right-hand end of a line is left
;;;; for the neighbour, so there are no gaps and nothing is drawn twice.
;;;;
;;;; Texture.  Each corner of a face is also given a place on a texture, a
;;;; picture 64 by 64.  Across a flat triangle the texture co-ordinates u
;;;; and v change by the same amount for every pixel to the right, and by
;;;; another fixed amount for every line down: two divisions for a triangle
;;;; find those, and then every pixel is two additions and a look-up, as in
;;;; the example "rotozoom".  This is "affine" mapping.  It is exact for a
;;;; face seen square on; for one seen at a slant it ignores that the far
;;;; end should be squeezed, and the texture bends along the diagonal where
;;;; the two triangles of a face meet: watch the cube's faces as they turn
;;;; away.  (Dividing by z at every pixel puts it right, and is what the
;;;; machines of the day could not afford.)  The palette is changed to 16
;;;; colours of texture at each of 16 brightnesses, so the faces are still
;;;; shaded.
;;;;
;;;; All the arithmetic while it runs is in whole numbers: co-ordinates in
;;;; 1024ths, sines and normals in 4096ths, edges and texture co-ordinates
;;;; with 16 bits of fraction.  Each frame is made in a Lisp vector of 64,000
;;;; bytes and copied to video memory in one go.  The top line shows frames
;;;; a second (paced by WAIT-FRAME: 70 or so if the machine keeps up) and
;;;; how many pixels were filled in the frame.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.solid3d
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.solid3d)

(defconstant +width+ 320) (defconstant +height+ 200)
(defconstant +screen-bytes+ (* +width+ +height+))
(defconstant +white+ 255)               ; and colour 0 is black, in both palettes
(defconstant +unit+ 1024)               ; 1 in a co-ordinate
(defconstant +distance+ (* 9 512))      ; from the eye to the middle of the solid: 4.5
(defconstant +texture-size+ 64)

(deftype bytes () '(simple-array (unsigned-byte 8) (*)))
(deftype fixnums () '(simple-array fixnum (*)))
(deftype word32 () '(unsigned-byte 32))
(defun make-bytes (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))
(defun make-fixnums (n) (make-array n :element-type 'fixnum :initial-element 0))
(defun fixnums (list) (make-array (length list) :element-type 'fixnum :initial-contents list))

;;; ---- the palettes
(defun flat-palette ()
  "Three ramps of 64 shades, from colours 64, 128 and 192."
  (set-palette 0 0 0 0)
  (dotimes (i 64)
    (set-palette (+ 64 i) i (floor (* i 5) 8) (floor i 6))            ; orange
    (set-palette (+ 128 i) (floor i 4) (floor (* i 5) 8) i)           ; blue
    (set-palette (+ 192 i) (floor i 3) i (floor (* i 3) 8)))          ; green
  (set-palette +white+ 63 63 63))
(defun texture-palette ()
  "Colour brightness * 16 + c is the texture's colour c at that brightness."
  (dotimes (light 16)
    (loop for c from 0
          for (r g b) in '((16 18 26) (63 38 8) (10 26 58) (63 60 46) (40 8 8))
          do (set-palette (+ c (* 16 light))
                          (round (* r light) 15) (round (* g light) 15) (round (* b light) 15))))
  (set-palette +white+ 63 63 63))

;;; ---- the texture: 64 by 64, colours 0 to 4.  A chequer board of orange
;;; and blue with a pale frame round it, and two words.
(defun make-texture ()
  (let ((texture (make-bytes (* +texture-size+ +texture-size+))))
    (flet ((put (x y colour) (setf (aref texture (+ x (* +texture-size+ y))) colour)))
      (dotimes (y +texture-size+)
        (dotimes (x +texture-size+)
          (put x y (cond ((not (and (< 1 x 62) (< 1 y 62))) 3)
                         ((evenp (+ (ash x -4) (ash y -4))) 1)
                         (t 2)))))
      (loop for (string left top colour) in '(("LISP" 0 10 3) ("DOS" 8 38 3))
            do (dotimes (pass 2)        ; a dark shadow, then the letters, twice the BIOS's size
                 (dotimes (i (length string))
                   (dotimes (row 16)
                     (dotimes (column 16)
                       (when (logbitp (- 7 (ash column -1))
                                      (glyph-row (char string i) (ash row -1)))
                         (let ((x (+ left (* 16 i) column)) (y (+ top row)))
                           (if (zerop pass)
                               (put (min 63 (+ x 1)) (+ y 1) 0)
                               (put x y colour))))))))))
    texture))

;;; ---- the solids
(defstruct solid
  (name "" :type simple-string)
  (x (make-fixnums 0) :type fixnums)    ; the corners, in 1024ths
  (y (make-fixnums 0) :type fixnums)
  (z (make-fixnums 0) :type fixnums)
  (faces 0 :type fixnum)
  (corner (make-fixnums 0) :type fixnums)   ; four to a face: which corners, in order round it
  (u (make-fixnums 0) :type fixnums)        ; four to a face: where on the texture, in texels
  (v (make-fixnums 0) :type fixnums)
  (nx (make-fixnums 0) :type fixnums)       ; one to a face: its normal, in 4096ths
  (ny (make-fixnums 0) :type fixnums)
  (nz (make-fixnums 0) :type fixnums)
  (ramp (make-fixnums 0) :type fixnums))    ; one to a face: its colours in the flat palette

(defun build-solid (name points faces)
  "POINTS is a list of (x y z), in real numbers.  FACES is a list of
(corners us vs ramp): the numbers of four points, the texture co-ordinates
of each, and a palette ramp.  The points go round the face clockwise as
seen from outside, on the screen, where x is to the right, y up and z away
from the eye (by the algebra, where the handedness of the axes does not
come into it: such that the cross product below points outwards).
The normals are worked out here: the cross product of the face's diagonals
stands straight out of it."
  (flet ((scaled (list scale) (fixnums (mapcar (lambda (n) (round (* n scale))) list))))
    (let ((normals
            (loop for (corners) in faces
                  collect (destructuring-bind (p0 p1 p2 p3)
                              (mapcar (lambda (c) (nth c points)) corners)
                            (destructuring-bind (ax ay az) (mapcar #'- p2 p0)
                              (destructuring-bind (bx by bz) (mapcar #'- p3 p1)
                                (let* ((nx (- (* ay bz) (* az by)))
                                       (ny (- (* az bx) (* ax bz)))
                                       (nz (- (* ax by) (* ay bx)))
                                       (length (sqrt (+ (* nx nx) (* ny ny) (* nz nz)))))
                                  (list (/ nx length) (/ ny length) (/ nz length)))))))))
      (make-solid :name name
                  :x (scaled (mapcar #'first points) +unit+)
                  :y (scaled (mapcar #'second points) +unit+)
                  :z (scaled (mapcar #'third points) +unit+)
                  :faces (length faces)
                  :corner (fixnums (mapcan (lambda (f) (copy-list (first f))) faces))
                  :u (fixnums (mapcan (lambda (f) (copy-list (second f))) faces))
                  :v (fixnums (mapcan (lambda (f) (copy-list (third f))) faces))
                  :nx (scaled (mapcar #'first normals) 4096)
                  :ny (scaled (mapcar #'second normals) 4096)
                  :nz (scaled (mapcar #'third normals) 4096)
                  :ramp (fixnums (mapcar #'fourth faces))))))

(defun make-cube ()
  "Corner number i is at (+-1, +-1, +-1) by the bits of i, as in wire3d.
Each face has the whole texture, and opposite faces a colour."
  (build-solid
   "Cube"
   (loop for i below 8 collect (list (if (logbitp 0 i) 1.0 -1.0)
                                     (if (logbitp 1 i) 1.0 -1.0)
                                     (if (logbitp 2 i) 1.0 -1.0)))
   (loop for axis below 3
         for b = (mod (+ axis 1) 3) for c = (mod (+ axis 2) 3)
         append (loop for side below 2
                      collect (list (loop for (bit-b bit-c) in (if (= side 1)
                                                                   '((0 0) (1 0) (1 1) (0 1))
                                                                   '((1 0) (0 0) (0 1) (1 1)))
                                          collect (+ (ash side axis) (ash bit-b b) (ash bit-c c)))
                                    '(64 0 0 64) '(64 64 0 0)
                                    (+ 64 (* 64 axis)))))))

(defun make-ring (&optional (around 16) (across 8) (radius 1.0) (thickness 0.45))
  "A torus of AROUND by ACROSS faces.  The texture goes four times round
it and twice round its thickness, and the colours make a chequer board."
  (flet ((point (i j) (+ (mod j across) (* across (mod i around)))))
    (build-solid
     "Ring"
     (loop for i below around
           for a = (* i (/ (* 2 pi) around))
           append (loop for j below across
                        for b = (* j (/ (* 2 pi) across))
                        for r = (+ radius (* thickness (cos b)))
                        collect (list (* r (cos a)) (* r (sin a)) (* thickness (sin b)))))
     (loop for i below around
           append (loop for j below across
                        collect (list (list (point i j) (point (1+ i) j)
                                            (point (1+ i) (1+ j)) (point i (1+ j)))
                                      (let ((u (* 16 (- around i)))) (list u (- u 16) (- u 16) u))
                                      (let ((v (* 16 j))) (list v v (+ v 16) (+ v 16)))
                                      (if (evenp (+ i j)) 64 128)))))))

;;; ---- sines: 1024 steps to the circle, 4096 for 1
(defun make-sines ()
  (let ((sines (make-fixnums 1024)))
    (dotimes (i 1024 sines)
      (setf (aref sines i) (round (* 4096 (sin (* i (/ pi 512)))))))))

;;; ---- filling triangles.  LEFT and RIGHT have a number for each line of
;;; the screen: where the triangle begins on it, and the pixel after its end.
(defun walk-edge (left right x0 y0 x1 y1)
  "Note in LEFT and RIGHT where the edge from X0, Y0 to X1, Y1 crosses each
line of the screen from the upper end's to the one before the lower end's."
  (declare (type fixnums left right) (type (signed-byte 16) x0 y0 x1 y1)
           (optimize speed (safety 0)))
  (when (> y0 y1) (rotatef x0 x1) (rotatef y0 y1))          ; always downwards
  (unless (= y0 y1)                                          ; a level edge crosses no line
    (let ((x (+ (ash x0 16) #xFFFF))    ; 16 bits of fraction; shifting right then rounds up
          (step (truncate (ash (- x1 x0) 16) (- y1 y0))))
      (declare (fixnum x step))
      (loop for y of-type fixnum from y0 below y1
            do (when (< -1 y +height+)
                 (let ((whole (ash x -16)))
                   (when (< whole (aref left y)) (setf (aref left y) whole))
                   (when (> whole (aref right y)) (setf (aref right y) whole))))
               (incf x step)))))

(defun walk-triangle (left right x0 y0 x1 y1 x2 y2)
  "Set LEFT and RIGHT for the triangle.  Returns its first line and the line
after its last, kept to the screen."
  (declare (type fixnums left right) (type (signed-byte 16) x0 y0 x1 y1 x2 y2)
           (optimize speed (safety 0)))
  (let ((top (max 0 (min y0 y1 y2))) (bottom (min +height+ (max y0 y1 y2))))
    (loop for y of-type fixnum from top below bottom
          do (setf (aref left y) 32767 (aref right y) -32768))
    (walk-edge left right x0 y0 x1 y1)
    (walk-edge left right x1 y1 x2 y2)
    (walk-edge left right x2 y2 x0 y0)
    (loop for y of-type fixnum from top below bottom          ; and kept to the screen sideways
          do (setf (aref left y) (max 0 (aref left y))
                   (aref right y) (min +width+ (aref right y))))
    (values top bottom)))

(defun fill-triangle (buffer left right x0 y0 x1 y1 x2 y2 colour)
  "A triangle of one colour.  Returns the number of pixels filled."
  (declare (type bytes buffer) (type fixnums left right) (type (signed-byte 16) x0 y0 x1 y1 x2 y2)
           (type (unsigned-byte 8) colour) (optimize speed (safety 0)))
  (let ((pixels 0))
    (declare (fixnum pixels))
    (multiple-value-bind (top bottom) (walk-triangle left right x0 y0 x1 y1 x2 y2)
      (declare (fixnum top bottom))
      (loop for y of-type fixnum from top below bottom
            do (let ((row (* +width+ y)) (from (aref left y)) (to (aref right y)))
                 (declare (fixnum row from to))
                 (when (< from to)
                   (incf pixels (- to from))
                   (loop for i of-type fixnum from (+ row from) below (+ row to)
                         do (setf (aref buffer i) colour))))))
    pixels))

(defun texture-steps (x0 y0 x1 y1 x2 y2 u0 u1 u2)
  "How much the texture co-ordinate that is U0, U1 and U2 (in texels) at
the three corners changes for one pixel to the right and for one line down,
and what it would be at the top left corner of the screen: three numbers
with 16 bits of fraction, as 32-bit words (what is above 32 bits would only
say which repeat of the texture it is).  NIL if the triangle has no area.
Twice a triangle, so in Lisp's ordinary integers, which cannot overflow."
  (let ((area (- (* (- x1 x0) (- y2 y0)) (* (- x2 x0) (- y1 y0)))))
    (unless (zerop area)
      (let ((across (round (ash (- (* (- u1 u0) (- y2 y0)) (* (- u2 u0) (- y1 y0))) 16) area))
            (down (round (ash (- (* (- u2 u0) (- x1 x0)) (* (- u1 u0) (- x2 x0))) 16) area)))
        (values (logand across #xFFFFFFFF) (logand down #xFFFFFFFF)
                (logand (- (ash u0 16) (* x0 across) (* y0 down)) #xFFFFFFFF))))))

(defun texture-triangle (buffer left right texture light
                         x0 y0 u0 v0 x1 y1 u1 v1 x2 y2 u2 v2)
  "A triangle of TEXTURE, the texels U0, V0 and so on at its corners, with
LIGHT (a brightness times 16) added to each colour.  Returns the number of
pixels filled."
  (declare (type bytes buffer texture) (type fixnums left right)
           (type (signed-byte 16) x0 y0 x1 y1 x2 y2) (type (unsigned-byte 8) light))
  (multiple-value-bind (du row-du u) (texture-steps x0 y0 x1 y1 x2 y2 u0 u1 u2)
    (multiple-value-bind (dv row-dv v) (texture-steps x0 y0 x1 y1 x2 y2 v0 v1 v2)
      (if (not (and du dv))
          0
          (locally (declare (type word32 du row-du u dv row-dv v) (optimize speed (safety 0)))
            (let ((pixels 0))
              (declare (fixnum pixels))
              (multiple-value-bind (top bottom) (walk-triangle left right x0 y0 x1 y1 x2 y2)
                (declare (type (integer 0 200) top bottom))
                ;; U and V are for the left edge of the screen on the line being filled
                (setf u (logand (+ u (* top row-du)) #xFFFFFFFF)
                      v (logand (+ v (* top row-dv)) #xFFFFFFFF))
                (loop for y of-type fixnum from top below bottom
                      do (let ((from (aref left y)) (to (aref right y)))
                           (declare (type (integer 0 320) from to))
                           (when (< from to)
                             (incf pixels (- to from))
                             (let ((u (logand (+ u (* from du)) #xFFFFFFFF))
                                   (v (logand (+ v (* from dv)) #xFFFFFFFF)))
                               (declare (type word32 u v))
                               (loop for i of-type fixnum from (+ (* +width+ y) from)
                                       below (+ (* +width+ y) to)
                                     ;; the texel's row is bits 16 to 21 of V, its
                                     ;; column those of U: row * 64 + column
                                     do (setf (aref buffer i)
                                              (logior light
                                                      (aref texture
                                                            (logior (logand (ash v -10) #xFC0)
                                                                    (logand (ash u -16) #x3F)))))
                                        (setf u (logand (+ u du) #xFFFFFFFF)
                                              v (logand (+ v dv) #xFFFFFFFF))))))
                         (setf u (logand (+ u row-du) #xFFFFFFFF)
                               v (logand (+ v row-dv) #xFFFFFFFF))))
              pixels))))))

;;; ---- a frame
(defstruct (scene (:constructor %make-scene))
  (buffer (make-bytes +screen-bytes+) :type bytes)
  (left (make-fixnums +height+) :type fixnums)
  (right (make-fixnums +height+) :type fixnums)
  (sines (make-sines) :type fixnums)
  (texture (make-texture) :type bytes)
  ;; the corners after turning: in space, from the eye; and on the screen
  (x (make-fixnums 256) :type fixnums) (y (make-fixnums 256) :type fixnums)
  (z (make-fixnums 256) :type fixnums)
  (screen-x (make-fixnums 256) :type fixnums) (screen-y (make-fixnums 256) :type fixnums)
  ;; the faces to draw: which, how far away, and how brightly lit (0 to 4096)
  (order (make-fixnums 256) :type fixnums) (depth (make-fixnums 256) :type fixnums)
  (lit (make-fixnums 256) :type fixnums))

(defun turn-solid (scene solid a b)
  "Turn SOLID's corners by angle A about the y axis and then B about the x
axis, move it away from the eye, and project.  Then find the faces that
look at the eye, note their distance and brightness, and return how many."
  (declare (type scene scene) (type solid solid) (type (integer 0 1023) a b) (optimize speed))
  (let* ((sines (scene-sines scene))
         (sin-a (aref sines a)) (cos-a (aref sines (logand (+ a 256) 1023)))
         (sin-b (aref sines b)) (cos-b (aref sines (logand (+ b 256) 1023)))
         (xs (scene-x scene)) (ys (scene-y scene)) (zs (scene-z scene))
         (screen-x (scene-screen-x scene)) (screen-y (scene-screen-y scene))
         (order (scene-order scene)) (depth (scene-depth scene)) (lit (scene-lit scene))
         (corner (solid-corner solid)) (count 0))
    (declare (type (integer -4096 4096) sin-a cos-a sin-b cos-b) (fixnum count))
    (flet ((turn (x y z)                ; products of 1024ths (or 4096ths) and 4096ths
             (declare (type (integer -8192 8192) x y z))
             (let* ((x1 (ash (- (* x cos-a) (* z sin-a)) -12))
                    (z1 (ash (+ (* x sin-a) (* z cos-a)) -12))
                    (y2 (ash (- (* y cos-b) (* z1 sin-b)) -12))
                    (z2 (ash (+ (* y sin-b) (* z1 cos-b)) -12)))
               (values x1 y2 z2))))
      (declare (inline turn))
      (dotimes (i (length (solid-x solid)))
        (multiple-value-bind (x y z)
            (turn (aref (solid-x solid) i) (aref (solid-y solid) i) (aref (solid-z solid) i))
          (let ((z (+ z +distance+)))   ; z is now the distance in front of the eye
            (declare (type (integer 1024 16384) z))
            (setf (aref xs i) x (aref ys i) y (aref zs i) z
                  ;; perspective; the pixels are 6/5 as tall as they are wide
                  (aref screen-x i) (+ 160 (round (* x 250) z))
                  (aref screen-y i) (- 104 (round (* y 208) z))))))
      (dotimes (face (solid-faces solid))
        (multiple-value-bind (nx ny nz)
            (turn (aref (solid-nx solid) face) (aref (solid-ny solid) face)
                  (aref (solid-nz solid) face))
          (let* ((c0 (aref corner (* 4 face))) (c1 (aref corner (+ 1 (* 4 face))))
                 (c2 (aref corner (+ 2 (* 4 face)))) (c3 (aref corner (+ 3 (* 4 face))))
                 ;; the middle of the face (the eye is at 0, 0, 0)
                 (mx (ash (+ (aref xs c0) (aref xs c1) (aref xs c2) (aref xs c3)) -2))
                 (my (ash (+ (aref ys c0) (aref ys c1) (aref ys c2) (aref ys c3)) -2))
                 (mz (ash (+ (aref zs c0) (aref zs c1) (aref zs c2) (aref zs c3)) -2)))
            (declare (type (integer -16384 16384) mx my mz))
            ;; It looks at the eye if its normal points against the line from the
            ;; eye to it.
            (when (minusp (+ (* nx mx) (* ny my) (* nz mz)))
              (setf (aref order count) face
                    (aref depth face) mz
                    ;; the light comes from over the left shoulder: (-0.5, 0.6, -0.62)
                    (aref lit face) (max 0 (ash (+ (* nx -2048) (* ny 2458) (* nz -2540)) -12)))
              (incf count))))))
    count))

(defun sort-faces (scene count)
  "Put the COUNT faces to draw in order, the furthest first: an insertion
sort, which is simple and quick enough for a few dozen things."
  (declare (type scene scene) (fixnum count) (optimize speed (safety 0)))
  (let ((order (scene-order scene)) (depth (scene-depth scene)))
    (loop for i of-type fixnum from 1 below count
          do (let* ((face (aref order i)) (z (aref depth face)) (j i))
               (declare (fixnum j))
               (loop while (and (> j 0) (< (aref depth (aref order (1- j))) z))
                     do (setf (aref order j) (aref order (1- j)))
                        (decf j))
               (setf (aref order j) face)))))

(defun draw-solid (scene solid a b textured)
  "Draw SOLID, turned by A and B, into the scene's buffer.  Returns the
number of pixels filled and the number of faces drawn."
  (declare (type scene scene) (type solid solid))
  (let ((count (turn-solid scene solid a b)) (pixels 0)
        (buffer (scene-buffer scene)) (left (scene-left scene)) (right (scene-right scene))
        (sx (scene-screen-x scene)) (sy (scene-screen-y scene)) (texture (scene-texture scene))
        (corner (solid-corner solid)) (us (solid-u solid)) (vs (solid-v solid)))
    (declare (fixnum pixels))
    (fill buffer 0)
    (sort-faces scene count)
    (dotimes (n count)
      (let* ((face (aref (scene-order scene) n)) (lit (aref (scene-lit scene) face))
             (base (* 4 face)))
        (flet ((triangle (i j k)        ; three of the face's four corners
                 (let ((ci (aref corner (+ base i))) (cj (aref corner (+ base j)))
                       (ck (aref corner (+ base k))))
                   (incf pixels
                         (if textured
                             (texture-triangle
                              buffer left right texture (* 16 (+ 5 (floor (* 10 lit) 4096)))
                              (aref sx ci) (aref sy ci) (aref us (+ base i)) (aref vs (+ base i))
                              (aref sx cj) (aref sy cj) (aref us (+ base j)) (aref vs (+ base j))
                              (aref sx ck) (aref sy ck) (aref us (+ base k)) (aref vs (+ base k)))
                             (fill-triangle
                              buffer left right (aref sx ci) (aref sy ci) (aref sx cj) (aref sy cj)
                              (aref sx ck) (aref sy ck)
                              (+ (aref (solid-ramp solid) face) 18 (floor (* 44 lit) 4096))))))))
          (triangle 0 1 2)
          (triangle 0 2 3))))
    (values pixels count)))

(defun status-line (solid textured frames-a-second pixels)
  (coerce (format nil "~40A" (format nil "~A ~A ~3D frames/s ~6:D pixels"
                                     (solid-name solid) (if textured "textured" "flat    ")
                                     frames-a-second pixels))
          'simple-string))

(defun solid3d ()
  (let* ((scene (%make-scene)) (solids (list (make-cube) (make-ring))) (solid (first solids))
         (buffer (scene-buffer scene))
         (textured nil) (frame 0) (pixels 0) (faces 0)
         (frames 0) (frames-a-second 0) (since (timer-count))
         (status (status-line solid textured 0 0)) (vga *vga*))
    (with-video-mode (#x13)
      (flat-palette)
      (progn
        (loop
          (when (key-pressed-p)
            (case (get-key)
              (#\Escape (return))
              (#\Space (setf textured (not textured))
               (if textured (texture-palette) (flat-palette)))
              ((#\Return #\Newline)
               (setf solid (or (second (member solid solids)) (first solids)))))
            (setf status (status-line solid textured frames-a-second pixels)))
          (setf (values pixels faces)
                (draw-solid scene solid (logand (* 5 frame) 1023) (logand (* 3 frame) 1023)
                            textured))
          (with-vector-memory (memory buffer) (draw-glyphs memory 0 +width+ status +white+ 0))
          (wait-frame)
          (copy-vector-to-memory buffer 0 vga 0 +screen-bytes+)
          (incf frame) (incf frames)
          (let ((elapsed (- (timer-count) since)))
            (when (or (>= elapsed +timer-hz+) (< elapsed 0))         ; a second (or midnight)
              (when (> elapsed 0)
                (setf frames-a-second (round (* frames +timer-hz+) elapsed)
                      status (status-line solid textured frames-a-second pixels)))
              (setf frames 0 since (timer-count)))))))
    (format t "~&solid3d: the ~(~A~), ~:[flat shaded~;texture mapped~]: ~D frames a second, ~
               ~:D pixels in ~D faces in the last frame.~%"
            (solid-name solid) textured frames-a-second pixels faces)))

(defun main ()
  "A rotating cube and ring with filled faces: flat shaded or texture mapped."
  (solid3d)
  (values))

(doslib-examples:define-example "solid3d" 'main
  :category :graphics
  :description "Filled 3-D solids: culling, depth sorting, flat shading, affine texture mapping.")
