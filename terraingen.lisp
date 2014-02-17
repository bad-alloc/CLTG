(require 'zpng)
(declaim (optimize (speed 2) (space 2) (debug 0)))

(defun power-of-two-p (x)
  (or (= x 0)                   ; 0 is not a power of two.
      (= 0 (logand x (1- x)))))

(defun average (&rest values)
  (/ (apply #'+ values) (length values)))

(defmacro point (x y)
  `(cons ,x ,y))

(defmacro px (point)
  `(car ,point))

(defmacro py (point)
  `(cdr ,point))

;gets random value in [-c; c]
(defmacro rand+/- (c)
  `(- (random (* 2 (coerce ,c 'float))) ,c))

(defmacro avg-square-points (array square)
  `(average (aref ,array (px (first ,square)) (py (first ,square)))
	    (aref ,array (px (second ,square)) (py (second ,square)))
	    (aref ,array (px (third ,square)) (py (third ,square)))
	    (aref ,array (px (fourth ,square)) (py (fourth ,square)))))

(defmacro set-point (map point value)
  `(setf (aref ,map (px ,point) (py ,point)) ,value))

(defmacro get-point (map point)
  `(aref ,map (px ,point) (py ,point)))

(defmacro set-point-safely (map point value)
  `(if (= 0 (get-point ,map ,point))
       (setf (aref ,map (px ,point) (py ,point)) ,value)))

(defun make-squares (square &optional (depth 0))
  (let* ((tl (first square))  ; these four are the corners of our square
	 (tr (second square))
	 (bl (third square))
	 (br (fourth square))
	 (size (- (px tr) (px tl)))
	 (mid (point (floor (/ (+ (px tr) (px tl)) 2.0))  ; square midpint
		     (floor (/ (+ (py bl) (py tl)) 2.0))))
	 (m1 (point (px mid) (py tl))) ; midpoint top side
	 (m2 (point (px tl) (py mid))) ; left side
	 (m3 (point (px tr) (py mid))) ; right side
	 (m4 (point (px mid) (py bl))) ; bottom side
	 (sub1 (list tl m1 m2 mid))  ;the new subsquares
	 (sub2 (list m1 tr mid m3))
	 (sub3 (list m2 mid bl m4))
	 (sub4 (list mid m3 m4 br))
	 (diamond `(:corners ,(list tl tr bl br) :mid ,mid :targets ,(list m1 m2 m3 m4) :depth ,depth))) ; the diamond in this square
    
    (if (= size 2)
	(list diamond)
	(append (list diamond)
		(make-squares sub1 (1+ depth))
		(make-squares sub2 (1+ depth))
		(make-squares sub3 (1+ depth))
		(make-squares sub4 (1+ depth))))))

(defun make-terrain (size &optional (corner-init '(10 5 7 2))  (roughness 1.0) (height-delta 100))
  (assert (power-of-two-p size))
  (let ((map (make-array (list (1+ size) (1+ size)) :initial-element 0)) ;we need sides of 2^n + 1
	; we need to stable sort this, so the diamonds are sorted by depth but their relative order
	; should be the same as the order they were generated in
	(diamonds (stable-sort (make-squares (list (point 0 0) (point size 0) (point 0 size) (point size size)))
			       (lambda (a b) (< (getf a :depth) (getf b :depth)))))
	(last-depth 0))
    ;init
    (set-point map (point 0 0) (first corner-init))
    (set-point map (point size 0) (second corner-init))
    (set-point map (point 0 size) (third corner-init))
    (set-point map (point size size) (fourth corner-init))
    (loop for diamond in diamonds do
	 (progn
	   (let* ((midpoint-average (avg-square-points map (getf diamond :corners)))
		  (mid-random (+ midpoint-average (rand+/- height-delta)))	  
		  (top-random (+ (average (get-point map (first  (getf diamond :corners)))
				       (get-point map (second (getf diamond :corners)))
				       mid-random)
				  (rand+/- height-delta)))
		  (left-random (+ (average (get-point map (first (getf diamond :corners)))
					   (get-point map (third (getf diamond :corners)))
					   mid-random)
				  (rand+/- height-delta)))
		  (right-random (+ (average (get-point map (second (getf diamond :corners)))
					    (get-point map (fourth (getf diamond :corners)))
					    mid-random)
				   (rand+/- height-delta)))
		  (bottom-random (+ (average (get-point map (third  (getf diamond :corners)))
					     (get-point map(fourth (getf diamond :corners)))
					     mid-random)
				    (rand+/- height-delta))))
	     ;midpoint displacement
	     (set-point-safely map (getf diamond :mid) mid-random)
	     ;points on edges
	     (set-point-safely map (first  (getf diamond :targets)) top-random)
	     (set-point-safely map (second (getf diamond :targets)) left-random)
	     (set-point-safely map (third  (getf diamond :targets)) right-random)
	     (set-point-safely map (fourth (getf diamond :targets)) bottom-random)
	     ;change height delta but only once per level
	     (if (> (getf diamond :depth) last-depth)
		 (progn
		   (setf height-delta (* height-delta (expt 2 (- roughness)))
			 last-depth (getf diamond :depth))
		   (format t "Set delta to ~a~%" height-delta))))))
    (return-from make-terrain map)))

(defun get-landscape-extremes (map)
  (let ((max 0) (min 0) (size (car (array-dimensions map))))
  (dotimes (y size)
    (dotimes (x size)
      (let ((val (get-point map (point x y))))
	(cond ((> val max) (setf max val))
	      ((< val min) (setf min val))))))
  (format t "Maximum = ~a; Minimum = ~a~%" max min)
  (list :max max :min min)))

(defun scale-to-byte (value min max) (round (* 255 (/ (- value min) (- max min)))))

(defun draw-heightmap (map &optional (file "Programme/Lisp/Terrain/heightmap.png"))
  (let* ((size (car (array-dimensions map)))
	 (png (make-instance 'zpng:png
                             :color-type :grayscale
                             :width size
                             :height size))
         (image (zpng:data-array png))
	 (extremes (get-landscape-extremes map)))
    (dotimes (y size (zpng:write-png png file))
      (dotimes (x size)
	(setf (aref image y x 0) 
	      (scale-to-byte (get-point map (point x y))
			     (getf extremes :min)
			     (1+ (getf extremes :max))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;optimizing;;;;;;;;;;;;;;;;;;;;
;;(time (draw-heightmap (make-terrain 1024 '(0 0 0 0) 2.0 100) "out.png"))
;; 
;; start:
;; 
;; 2.867 seconds of real time
;; 2.860000 seconds of total run time (2.640000 user, 0.220000 system)
;; [ Run times consist of 0.970 seconds GC time, and 1.890 seconds non-GC time. ]
;; 99.76% CPU
;; 8,044,249,634 processor cycles
;; 506,342,208 bytes consed
;;
;;
;; no debug
;;
;; 2.256 seconds of real time
;; 2.250000 seconds of total run time (2.130000 user, 0.120000 system)
;; [ Run times consist of 0.630 seconds GC time, and 1.620 seconds non-GC time. ]
;; 99.73% CPU
;; 6,332,781,399 processor cycles
;; 506,297,680 bytes consed