(in-package :cl-user)

(defparameter *master-beat-divisor* 24)

(defclass gesture-sequence ()
  ((ticks-index :initarg :ticks-index
		:initform 0
		:accessor ticks-index)))

(defun clock-divisor (seq)
  (/ *master-beat-divisor* (beat-divisor seq)))

(defclass grid-sequence (gesture-sequence)
  ((grid :initarg :sequence
	 :accessor grid)
   (beat-divisor :initarg :beat-divisor
		 :initform 4
		 :accessor beat-divisor)
   (gesture-map :initarg :gesture-map
		:initform nil
		:accessor gesture-map)))

(defun make-grid-sequence (x y gesture-map &key (beat-divisor 4))
  (make-instance 'grid-sequence
		 :sequence (make-array (list x y)
				       :element-type 'atom
				       :initial-element nil)
		 :gesture-map gesture-map
		 :beat-divisor beat-divisor))

(defgeneric record-gesture (gesture sequence))

(defmethod record-gesture (gesture (sequence grid-sequence))
  )

(defgeneric sequence-tick-length (gesture-sequence))
(defmethod sequence-tick-length ((seq grid-sequence))
  (* (clock-divisor seq)
     (car (array-dimensions (grid seq)))))

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
  (print gesture))

(defgeneric handle-gestures (seq))
(defmethod handle-gestures ((seq grid-sequence))
  (let ((grid-crossing (grid-crossing-point
			(floor (ticks-index seq));; XXX *think* this
			;; should never drop beats, even when the
			;; ticks index goes fractional due to, e.g
			;; 11:5 pattern-length wrapping on 24ppqn
			;; ticker.  But not sure!
			(beat-divisor seq))))
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

(defclass gesture-sequencer ()
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

(defun grid-crossing-point (ticks beat-divisor &optional (master-clock-divisor *master-beat-divisor*))
  (multiple-value-bind (int-ticks frac-ticks) (floor ticks master-clock-divisor)
    (let ((frac-master-clock (position frac-ticks
				       (aref *non-divisible-lookup-tables* beat-divisor master-clock-divisor))))
      (when frac-master-clock
	(+ (* beat-divisor int-ticks)
	   frac-master-clock)))))

(defun do-integer-non-divisible-test ()
  (let ((*master-beat-divisor* 96)
	(seq (make-grid-sequence 10 4
				 (list :emph (loop for i below 4
						collect (format nil "note~a strong" i))
				       t (loop for i below 4
					    collect (format nil "note~a" i)))
				 :beat-divisor 5)))
    (print (sequence-tick-length seq))
    (grid-set-column seq 0 '(t t :emph nil))
    (grid-set-element seq 1 3 :emph)
    (handle-gestures seq)
    (loop repeat 95
       do (do-tick seq)
	 (print (list 'tick (ticks-index seq)))
	 (handle-gestures seq))))

(defun swing-grid-crossings (list-of-crossings swing-ratio &optional (master-beat-divisor *master-beat-divisor*))
  (mapcar (lambda (crossing)
	    (let ((max-ratio (/ (- master-beat-divisor crossing)
				master-beat-divisor)))
	      (round (+ crossing
			(* crossing max-ratio swing-ratio)))))
	  list-of-crossings))
