(load "points.lisp")

(declaim (optimize (debug 3)))

;; Array index convention:
;;  0,0        0,x
;;     +------->
;;     |
;;     |
;;     |
;; y,0 v        y,x


;; walk-midpoints calls point-function on all midpoints of squares by
;; starting at a midpoint origin (which the caller supplies, this
;; function knows nothing of the array being worked on). From there it
;; "hops" along rows and columns, and visits every midpoint once.
(defun walk-midpoints (origin stepsize times point-function)
  (do ((y (py origin) (+ y stepsize))
       (current-line 0 (1+ current-line)))
      ((> current-line times))
    (do ((x (px origin) (+ x stepsize)) 
	 (current-column 0 (1+ current-column)))
	((> current-column times))
      (funcall point-function y x stepsize)))) ; point-function gets the array from
					       ; a scope outside of theis function.

;; diamond-step gets a midpoint (x y) of a square and sets it to the
;; average of the corners of said square. It is randomly displaced by
;; a value in the range [-height-delta; +height-delta].
;; a---b
;; |\ /|
;; | x |   x is the point being set using the average s of a,b,c,d
;; |/ \|
;; c---d
(defun diamond-step (y x stepsize height-delta arr)
  (let ((d (/ stepsize 2)))
    (setf (aref arr y x)
	  (+ (rand+/- height-delta)
	     (average
	      (aref arr (- y d) (- x d))
	      (aref arr (+ y d) (- x d))
	      (aref arr (- y d) (+ x d))
	      (aref arr (+ y d) (+ x d)))))))

;; square step gets a midpoint (x y) of a square and sets the points
;; on the corners of said square to the average of the values of the
;; midpoint of this sqare, the midpoint of the adjacant square and the
;; two corners. It is randomly displaced by a value in the range
;; [-height-delta; +height-delta].
;;        |       |       
;;        |   a   |         
;;        |   v   |         
;; -------o---x---o-------    the four x-points are being set using
;;        |   ^   |           the values of the square corners, the
;;    l-> x <-o-> x <-r       square midpoint and the midpoints of 
;;        |   v   |           adjacant suqares above (a), below (b)
;; -------o---x---o-------    left (l) and right (r).
;;        |   ^   |          
;;        |   b   |       
;;        |       |       

(defun square-step (y x stepsize height-delta arr)
  (format t "squaring from (~a, ~a) by ~a... " x y stepsize)
  (let* ((d (/ stepsize 2))
	 (midpoint (aref arr x y))
	 (above (if (> 0 (- y stepsize))
		    0
		    (aref arr x (- y stepsize))))
	 (left (if (> 0 (- x stepsize))
		   0
		   (aref arr (- x stepsize) y)))
	 (below (if (>= (+ y stepsize) (array-dimension arr 0))
		    0
		    (aref arr x (+ y stepsize))))
	 (right (if (>= (+ x stepsize) (array-dimension arr 1))
		    0
		    (aref arr (+ x stepsize) y)))
	 (top-left     (aref arr (- y d) (- x d)))
	 (top-right    (aref arr (- y d) (+ x d)))
	 (bottom-left  (aref arr (+ y d) (- x d)))
	 (bottom-right (aref arr (+ y d) (+ x d))))
    ; point in top side
    (format t "ok ~%")
    (setf (aref arr (- y d) x)
	  (+ (rand+/- height-delta)
	     (average above midpoint top-left top-right)))
    ; point in left side
    (setf (aref arr y (- x d))
	  (+ (rand+/- height-delta)
	     (average left midpoint top-left bottom-left)))
    ; point in right side
    (setf (aref arr y (+ x d))
	  (+ (rand+/- height-delta)
	     (average right midpoint top-right bottom-right)))
    ; point in bottom side
    (setf (aref arr (+ y d) x)
	  (+ (rand+/- height-delta)
	     (average below midpoint bottom-left bottom-right)))))
	  

;; set-points generates a size*size array of zeros and walks over the
;; midpoints of the diamind-square algorithm. The array arr (which
;; must be of a size 2^n+1) can be subdivided log(n) times. For each
;; subdivision walk-midpoints is called, which calls an anonymous
;; function on every midpoint. This function can manipulate arr, since
;; it is created within the let-block where arr is created and thus
;; forms a closure.

(defun set-points (size)
  (let ((arr (make-array (list size size) :initial-element 0)))
    (do ((midpoint (cons #1=(floor (/ size 2)) #1#))
	 (stepsize (1- size) (/ stepsize 2))
	 (times 0 (1+ times))
	 (index 1 (1+ index)))
	((>= times (floor (log size 2))))
      (walk-midpoints midpoint stepsize (1- (expt 2 times))
		      (lambda (x y stepsize)
			(progn
			  (format t "Working on point (~a, ~a)~%" y x)
			  (diamond-step y x stepsize 5 arr)
			  (square-step y x stepsize 5 arr))))
      (if (= 0 #2=(floor (/ (car midpoint) 2)))
	  (setf midpoint (cons 1 1))
	  (setf midpoint (cons #2# #2#))))
    arr))(declaim (optimize (debug 3)))