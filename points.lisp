(defmacro point (x y)
  `(cons ,x ,y))

(defmacro px (point)
  `(car ,point))

(defmacro py (point)
  `(cdr ,point))

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