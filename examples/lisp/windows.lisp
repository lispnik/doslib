;;;; A window system in a page of CLOS: overlapping text-mode windows with
;;;; buttons, check boxes and lists, driven by the mouse.
;;;;
;;;; A widget is an object; what it looks like and what a click does are
;;;; generic functions (DRAW, CLICK) with a method per class.  A new kind
;;;; of widget is a DEFCLASS and a method or two -- METER, at the end, is
;;;; one, added without touching the rest.  :AFTER methods add behaviour to
;;;; every button at once, and :AROUND keeps the mouse pointer out of the
;;;; way of whatever is being drawn.
;;;;
;;;; Click a button; drag a window by its title bar; click [x] to close
;;;; it.  Esc leaves.  Needs a mouse driver (CTMOUSE).
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.windows
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.windows)

;;; ---- the mouse, in character cells
(defun mouse ()
  (multiple-value-bind (column row buttons) (mouse-cell)
    (values column (min 24 row) (logtest buttons 1))))

(defvar *windows* '() "Bottom first; the last is on top.")
(defvar *desktop-colour* #x13)
(defvar *shadows* t)
(defvar *running* t)

;;; ---- widgets
(defclass widget ()
  ((x :initarg :x :accessor x) (y :initarg :y :accessor y)       ; relative to the parent
   (width :initarg :width :initform 1 :accessor width)
   (height :initarg :height :initform 1 :accessor height)
   (parent :initform nil :accessor parent)))
(defgeneric draw (widget) (:documentation "Paint the widget on the screen."))
(defgeneric click (widget column row)
  (:documentation "The mouse was pressed on the widget, at COLUMN, ROW within it.")
  (:method ((widget widget) column row) (declare (ignore column row)) nil))
(defun screen-x (w) (+ (x w) (if (parent w) (screen-x (parent w)) 0)))
(defun screen-y (w) (+ (y w) (if (parent w) (screen-y (parent w)) 0)))
(defun inside-p (w column row)          ; screen coordinates
  (and (<= (screen-x w) column (+ (screen-x w) (width w) -1))
       (<= (screen-y w) row (+ (screen-y w) (height w) -1))))

(defclass label (widget)
  ((text :initarg :text :accessor text) (colour :initarg :colour :initform #x70 :accessor colour)))
(defmethod draw ((w label))
  (text-put (screen-x w) (screen-y w) (format nil "~VA" (width w) (text w)) (colour w)))

(defclass button (widget)
  ((text :initarg :text :accessor text)
   (action :initarg :action :initform (constantly nil) :accessor action)))
(defmethod initialize-instance :after ((w button) &key)
  (setf (width w) (+ 4 (length (text w)))))
(defmethod draw ((w button))
  (text-put (screen-x w) (screen-y w) (format nil "[ ~A ]" (text w)) #x2F))
(defmethod click ((w button) column row)
  (declare (ignore column row))
  (funcall (action w) w))
(defmethod click :before ((w button) column row)       ; every button flashes when pressed
  (declare (ignore column row))
  (text-put (screen-x w) (screen-y w) (format nil "[ ~A ]" (text w)) #xE0)
  (wait-tick 2))

(defclass check-box (button)            ; a button that remembers
  ((checked :initarg :checked :initform nil :accessor checked)))
(defmethod draw ((w check-box))
  (text-put (screen-x w) (screen-y w) (format nil "[~:[ ~;x~]] ~A" (checked w) (text w)) #x70))
(defmethod click :before ((w check-box) column row)
  (declare (ignore column row))
  (setf (checked w) (not (checked w))))

(defclass list-box (widget)
  ((items :initarg :items :accessor items) (selected :initform 0 :accessor selected)
   (action :initarg :action :initform (constantly nil) :accessor action)))
(defmethod initialize-instance :after ((w list-box) &key)
  (setf (height w) (length (items w))))
(defmethod draw ((w list-box))
  (loop for item in (items w) for i from 0
        do (text-put (screen-x w) (+ (screen-y w) i) (format nil " ~VA" (1- (width w)) item)
                     (if (= i (selected w)) #x1F #x70))))
(defmethod click ((w list-box) column row)
  (declare (ignore column))
  (setf (selected w) row)
  (funcall (action w) w (nth row (items w))))

(defclass window (widget)
  ((title :initarg :title :accessor title) (children :initform '() :accessor children)))
(defun add (window &rest widgets)
  (dolist (w widgets) (setf (parent w) window))
  (setf (children window) (append (children window) widgets))
  window)
(defmethod draw ((w window))
  (text-box (screen-x w) (screen-y w) (width w) (height w) :attribute #x70 :shadow *shadows*
            :title (title w) :title-attribute #x74)
  (text-put (+ (screen-x w) (width w) -5) (screen-y w) "[x]" #x74)
  (mapc #'draw (children w)))

;;; ---- the desktop
(defun redraw ()
  (mouse-hide)                          ; the pointer must not be drawn over, or under
  (text-fill 0 0 80 25 *desktop-colour* (code-char 176))
  (text-put 0 24 (format nil "~80A" " Click a button.  Drag a window by its title.  [x] closes it.  Esc leaves.") #x70)
  (mapc #'draw *windows*)
  (mouse-show))
(defun window-at (column row)
  (find-if (lambda (w) (inside-p w column row)) *windows* :from-end t))
(defun raise (window)
  (setf *windows* (append (remove window *windows*) (list window))))
(defun drag (window column)             ; follow the mouse while the button is down
  (let ((grip (- column (x window))))
    (loop
      (multiple-value-bind (mx my down) (mouse)
        (unless down (return))
        (let ((nx (max 0 (min (- 80 (width window)) (- mx grip))))
              (ny (max 0 (min (- 24 (height window)) my))))
          (unless (and (= nx (x window)) (= ny (y window)))
            (setf (x window) nx (y window) ny)
            (redraw)))))))
(defun press (column row)
  (let ((window (window-at column row)))
    (when window
      (raise window)
      (redraw)
      (cond ((and (= row (y window)) (<= (- (+ (x window) (width window)) 5) column
                                         (- (+ (x window) (width window)) 3)))
             (setf *windows* (remove window *windows*)))        ; the [x]
            ((= row (y window)) (drag window column))
            (t (let ((widget (find-if (lambda (w) (inside-p w column row)) (children window))))
                 (when widget
                   (mouse-hide)
                   (click widget (- column (screen-x widget)) (- row (screen-y widget)))
                   (mouse-show)))))
      (redraw))))

;;; ---- a new kind of widget, to show what adding one takes
(defclass meter (widget)
  ((value :initarg :value :initform 0 :accessor value)))       ; 0..1
(defmethod draw ((w meter))
  (let ((filled (round (* (width w) (max 0 (min 1 (value w)))))))
    (text-put (screen-x w) (screen-y w)
              (concatenate 'string (make-string filled :initial-element (code-char 219))
                           (make-string (- (width w) filled) :initial-element (code-char 176)))
              #x7A)))

;;; ---- an application: four windows
(defun make-desktop ()
  (let* ((count 0)
         (counter (make-instance 'label :x 2 :y 2 :width 20 :text "Count: 0"))
         (level (make-instance 'meter :x 2 :y 6 :width 24))
         (memory (make-instance 'label :x 2 :y 2 :width 34 :text "")))
    (flet ((bump (by)
             (lambda (button)
               (declare (ignore button))
               (setf count (if by (+ count by) 0)
                     (text counter) (format nil "Count: ~D" count)
                     (value level) (/ (mod count 13) 12))))
           (show-memory ()
             (setf (text memory) (format nil "Heap in use: ~:D bytes" (sb-kernel:dynamic-usage)))))
      (show-memory)
      (setf *windows*
            (list
             (add (make-instance 'window :x 44 :y 2 :width 34 :height 9 :title "About")
                  (make-instance 'label :x 2 :y 2 :width 30 :text "Windows, buttons and lists")
                  (make-instance 'label :x 2 :y 3 :width 30 :text "as CLOS objects, drawn by")
                  (make-instance 'label :x 2 :y 4 :width 30 :text "generic functions, on DOS.")
                  (make-instance 'button :x 12 :y 6 :text "Quit"
                                         :action (lambda (b) (declare (ignore b)) (setf *running* nil))))
             (add (make-instance 'window :x 3 :y 13 :width 40 :height 9 :title "Lisp")
                  (make-instance 'label :x 2 :y 1 :width 36
                                        :text (format nil "~A ~A" (lisp-implementation-type)
                                                      (lisp-implementation-version)))
                  memory
                  (make-instance 'button :x 2 :y 4 :text "Collect garbage"
                                         :action (lambda (b) (declare (ignore b)) (gc :full t) (show-memory)))
                  (make-instance 'button :x 24 :y 4 :text "Refresh"
                                         :action (lambda (b) (declare (ignore b)) (show-memory))))
             (add (make-instance 'window :x 50 :y 12 :width 26 :height 11 :title "Desktop")
                  (make-instance 'list-box :x 2 :y 2 :width 16 :items '("Blue" "Green" "Cyan" "Red" "Grey")
                                           :action (lambda (list item)
                                                     (declare (ignore list))
                                                     (setf *desktop-colour*
                                                           (cdr (assoc item '(("Blue" . #x13) ("Green" . #x2A)
                                                                              ("Cyan" . #x3B) ("Red" . #x4C)
                                                                              ("Grey" . #x78))
                                                                       :test #'string=)))))
                  (make-instance 'check-box :x 2 :y 8 :text "Shadows" :checked t
                                            :action (lambda (box) (setf *shadows* (checked box)))))
             (add (make-instance 'window :x 26 :y 6 :width 30 :height 10 :title "Counter")
                  counter
                  (make-instance 'button :x 2 :y 4 :text "-" :action (bump -1))
                  (make-instance 'button :x 12 :y 4 :text "+" :action (bump 1))
                  (make-instance 'button :x 20 :y 4 :text "0" :action (bump nil))
                  level))))))

(defun main ()
  "A window system in a page of CLOS, with the mouse."
  (unless (mouse-init)
    (format t "~&There is no mouse driver.  Load CTMOUSE and try again.~%")
    (return-from main nil))
  (setf *running* t *desktop-colour* #x13 *shadows* t)
  (hide-text-cursor)
  (make-desktop)
  (redraw)
  (let ((was-down nil))
    (loop while *running*
          do (when (key-pressed-p)
               (when (eql (get-key) (code-char 27)) (return)))
             (multiple-value-bind (column row down) (mouse)
               (when (and down (not was-down)) (press column row))
               (setf was-down (nth-value 2 (mouse))))))
  (mouse-hide)
  (text-fill 0 0 80 25 7)
  (text-cursor 0 0)
  (show-text-cursor)
  t)

(doslib-examples:define-example "windows" 'main
  :category :lisp
  :description "A window system in a page of CLOS, with the mouse.")
