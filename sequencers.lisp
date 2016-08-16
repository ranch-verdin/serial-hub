(in-package :sequencers)

(defparameter *master-beat-divisor* 24)

(defclass gesture-sequence ()
  ((ticks-index :initarg :ticks-index
		:initform 0
		:accessor ticks-index)
   (swing-ratio :initarg :swing-ratio
		:initform 0
		:accessor swing-ratio)))

(defun clock-divisor (seq)
  (/ *master-beat-divisor* (beat-divisor seq)))

(defclass grid-sequence (gesture-sequence)
  ((grid :initarg :sequence
	 :accessor grid)
   (beat-divisor :initarg :beat-divisor
		 :initform 4
		 :accessor beat-divisor)
   (grid-length :initarg :grid-length
		:initform 16
		:accessor grid-length)
   (gesture-map :initarg :gesture-map
		:initform nil
		:accessor gesture-map)))

(defun make-grid-sequence (x y gesture-map &key (grid-length 16) (beat-divisor 4) (swing 0.0))
  (make-instance 'grid-sequence
		 :sequence (make-array (list (+ x 1) y)
				       :element-type 'atom
				       :initial-element nil)
		 :gesture-map gesture-map
		 :beat-divisor beat-divisor
		 :swing-ratio swing
		 :grid-length grid-length))

(defgeneric record-gesture (gesture sequence))

(defmethod record-gesture (gesture (sequence grid-sequence))
  (declare (ignore gesture))
  (error "not implemented yet FIXME"))

(defgeneric sequence-tick-length (gesture-sequence))
(defmethod sequence-tick-length ((seq grid-sequence))
  (* (clock-divisor seq)
     (grid-length seq)))

(defgeneric do-tick (gesture-sequence))

(defmethod do-tick ((seq grid-sequence))
  (setf (ticks-index seq)
	(rem (+ (ticks-index seq)
		1)
	     (sequence-tick-length seq))))


(defgeneric resolve-gesture (seq gesture-index gesture-modifier))
(defmethod resolve-gesture ((seq grid-sequence) (gesture-index integer) (gesture-modifier symbol))
  (nth gesture-index
       (getf (gesture-map seq) gesture-modifier)))

(defmethod handle-gesture ((seq grid-sequence) gesture)
  (declare (ignorable gesture seq))
  #+nil
  (print gesture))

(defgeneric handle-gestures (seq))
(defmethod handle-gestures ((seq grid-sequence))
  (let ((grid-crossing (grid-crossing-point seq)))
    (when grid-crossing
      (loop for y below (cadr (array-dimensions (grid seq)))
	 do (handle-gesture seq
			    (resolve-gesture seq
					     y
					     (aref (grid seq)
						   grid-crossing y)))))))

(defgeneric grid-set-element (seq x y value))

(defmethod grid-set-element ((seq grid-sequence) (x rational) (y integer) (value symbol))
  (setf (aref (grid seq) (round x) y)
	value))

(defgeneric grid-set-column (seq x values))

(defmethod grid-set-column ((seq grid-sequence) (x rational) (values list))
  (loop for value in values
     for y below (length values)
     do (setf (aref (grid seq)
		    (round x) y)
	      value)))

(defclass free-sequence (gesture-sequence)
  ())

(defun do-integer-divisible-test ()
  (let ((*master-beat-divisor* 24)
	(seq (make-grid-sequence 16 4
				 (list :emph (loop for i below 4
						collect (format nil "note~a strong" i))
				       t (loop for i below 4
					    collect (format nil "note~a" i))))))
    (grid-set-column seq 0 '(t t :emph nil))
    (grid-set-element seq 1 3 :emph)
    (handle-gestures seq)
    (loop repeat (+ 6 (* 16 6))
       do (do-tick seq))
    (handle-gestures seq)))

(defun build-non-divisible-lookup-table (grid-beat-divisor master-clock-beat-divisor)
  (loop for (this next) on (loop for i below master-clock-beat-divisor
			      collect (nth-value 1 (round (/ i master-clock-beat-divisor)
							  (/ grid-beat-divisor))))
     for i below (- master-clock-beat-divisor 1)
     when (and (<= this 0)
	       (> next 0))
     collect i))

(defun build-non-divisible-lookup-tables (max-beat-divisor max-master-clock-beat-divisor)
  (let ((res (make-array (list (+ max-beat-divisor 1) (+ max-master-clock-beat-divisor 1)))))
    (loop for mb from 1 to max-beat-divisor
       do (loop for mm from 1 to max-master-clock-beat-divisor
	     do (setf (aref res mb mm)
		      (build-non-divisible-lookup-table mb mm))))
    res))

(defparameter *non-divisible-lookup-tables* (build-non-divisible-lookup-tables 15 96))

(defun grid-crossing-point (seq &optional (master-clock-divisor *master-beat-divisor*))
  (multiple-value-bind (int-ticks frac-ticks) (floor (floor (ticks-index seq)) master-clock-divisor)
    (let ((frac-master-clock
	   (position frac-ticks
		     (swing-grid-crossings
		      (aref *non-divisible-lookup-tables*
			    (beat-divisor seq)
			    master-clock-divisor)
		      (swing-ratio seq)))))
      (when frac-master-clock
	(+ (* (beat-divisor seq) int-ticks)
	   frac-master-clock)))))

(defun swing-grid-crossings (list-of-crossings swing-ratio &optional (master-beat-divisor *master-beat-divisor*))
  (mapcar (lambda (crossing)
	    (let ((max-ratio (/ (- master-beat-divisor crossing)
				master-beat-divisor)))
	      (round (+ crossing
			(* crossing max-ratio swing-ratio)))))
	  list-of-crossings))



(defun do-integer-non-divisible-test ()
  (let ((*master-beat-divisor* 96)
	(seq (make-grid-sequence 16 4 (list :emph (loop for i below 4
						collect (format nil "note~a strong" i))
				       t (loop for i below 4
					    collect (format nil "note~a" i)))
				 :beat-divisor 5
				 :grid-length 10
				 :swing 0.6)))
    (grid-set-column seq 0 '(t t :emph nil))
    (grid-set-element seq 1 3 :emph)
    (handle-gestures seq)
    (loop repeat (* 96 3)
       as gc = (progn (do-tick seq)
		      (handle-gestures seq)
		      (grid-crossing-point seq))
       when gc
       do
	 (format t "grid point = ~a~%" gc))))

(defun do-integer-non-divisible-non-beat-sync-test ()
  (let ((*master-beat-divisor* 96)
	(seq (make-grid-sequence 16 4 (list :emph (loop for i below 4
						collect (format nil "note~a strong" i))
				       t (loop for i below 4
					    collect (format nil "note~a" i)))
				 :beat-divisor 5
				 :grid-length 11
				 :swing 0.6)))
    (grid-set-column seq 0 '(t t :emph nil))
    (grid-set-element seq 1 3 :emph)
    (handle-gestures seq)
    (loop repeat (* 96 3)
       as gc = (progn (do-tick seq)
		      (handle-gestures seq)
		      (grid-crossing-point seq))
       do (print (ticks-index seq))
       when gc
       do
	 (format t "grid point = ~a~%" gc))))
