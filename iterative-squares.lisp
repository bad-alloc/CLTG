(load "points.lisp")

(defun walk-midpoints (origin stepsize times point-function)
  (do ((y (py origin) (+ y stepsize))
       (current-line 0 (1+ current-line)))
      ((> current-line times))
    (do ((x (px origin) (+ x stepsize)) 
	 (current-column 0 (1+ current-column)))
	((> current-column times))
      (funcall point-function x y)))) ; point-function gets the array from
                                      ; a scope outside of theis function.

;; set-points generates a size*size array of zeros and walks over the
;; midpoints of the diamind-square algorithm. The array arr (which
;; must be of a size 2^n+1) can be subdivided log(n) times. For each
;; subdivision walk-midpoints is called, which calls an anonymous
;; function on every midpoint. This function can manipulate arr, since
;; it is created within the let-block where arr is created. 

(defun set-points (size)
  (let ((arr (make-array (list size size) :initial-element 0)))
    (do ((midpoint (cons #1=(floor (/ size 2)) #1#))
	 (stepsize (1- size) (/ stepsize 2))
	 (times 0 (1+ times))
	 (index 1 (1+ index)))
	((>= times (floor (log size 2))))
      (format t "Calling on ~a ~a ~a ~%" midpoint stepsize times)
      (walk-midpoints midpoint stepsize (1- (expt 2 times))
		      (lambda (x y) (setf (aref arr x y) index)))
      (if (= 0 #2=(floor (/ (car midpoint) 2)))
	  (setf midpoint (cons 1 1))
	  (setf midpoint (cons #2# #2#))))
    arr))