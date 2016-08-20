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
  ((grid :initarg :grid
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
		 :grid (make-array (list (+ x 1) y)
				   :element-type 'atom
				   :initial-element nil)
		 :gesture-map gesture-map
		 :beat-divisor beat-divisor
		 :swing-ratio swing
		 :grid-length grid-length))

(defgeneric record-gesture (gesture sequence))

(defmethod record-gesture (gesture (sequence grid-sequence))
  (declare (ignore gesture))
  (warn "record for grid not implemented yet FIXME"))

(defgeneric sequence-tick-length (gesture-sequence))
(defmethod sequence-tick-length ((seq grid-sequence))
  (* (clock-divisor seq)
     (grid-length seq)))

(defgeneric (setf sequence-tick-length) (tick-length seq))
(defmethod (setf sequence-tick-length) (tick-length (seq grid-sequence))
  (setf (grid-length seq)
	(* (beat-divisor seq)
	   (/ tick-length *master-beat-divisor*))))

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

(defgeneric read-gestures (seq))
(defmethod read-gestures ((seq grid-sequence))
  (let ((grid-crossing (grid-crossing-point seq)))
    (when grid-crossing
      (loop for y below (cadr (array-dimensions (grid seq)))
	 collect (resolve-gesture seq
				  y
				  (aref (grid seq)
					grid-crossing y))))))

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

(defun do-integer-divisible-test ()
  (let ((*master-beat-divisor* 24)
	(seq (make-grid-sequence 16 4
				 (list :emph (loop for i below 4
						collect (format nil "note~a strong" i))
				       t (loop for i below 4
					    collect (format nil "note~a" i))))))
    (grid-set-column seq 0 '(t t :emph nil))
    (grid-set-element seq 1 3 :emph)
    (loop repeat (+ 6 (* 16 6))
       do (do-tick seq))
    (read-gestures seq)))

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
    (print (read-gestures seq))
    (loop repeat (* 96 3)
       as gc = (progn (do-tick seq)
		      (print (read-gestures seq))
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
    (print (read-gestures seq))
    (loop repeat (* 96 3)
       as gc = (progn (do-tick seq)
		      (print (read-gestures seq))
		      (grid-crossing-point seq))
       do (print (ticks-index seq))
       when gc
       do
	 (format t "grid point = ~a~%" gc))))

(defvar *max-free-seq-length* (* *master-beat-divisor* 150))

(defclass free-sequence (gesture-sequence)
  ((memory :initarg :memory
	   :accessor fs-memory
	   :initform (make-array *max-free-seq-length*
				 :fill-pointer 0
				 :initial-element nil))
   (quantisation :initarg :quantisation
		 :initform *master-beat-divisor*
		 :accessor quantisation)
   (hanging-rec-tones :initarg :hanging-rec-tones
		      :initform nil
		      :accessor hanging-rec-tones)
   (hanging-play-tones :initarg :hanging-play-tones
		      :initform nil
		      :accessor hanging-play-tones)
   (play-state :initarg :play-state
	       :initform nil
	       :accessor play-state)
   (rec-state :initarg :rec-state
	      :initform nil
	      :accessor rec-state)))

(defmethod (setf sequence-tick-length) (tick-length (seq free-sequence))
  (loop for i from tick-length to (fill-pointer (fs-memory seq))
     do (setf (aref (fs-memory seq) i)
	      nil))
  (setf (fill-pointer (fs-memory seq))
	tick-length))

(defmethod sequence-tick-length ((seq free-sequence))
  (fill-pointer (fs-memory seq)))

(defmethod armed-and-ready ((seq free-sequence))
  (and (eq (rec-state seq)
		 :overdub)
	     (> (ticks-index seq) 0)
	     (<= (ticks-index seq)
		 (fill-pointer (fs-memory seq)))))

(defmethod record-gesture (gesture (seq free-sequence))
  (warn "record-gesture quantisation not implemented yet...")
  (when (armed-and-ready seq)
    (push gesture (aref (fs-memory seq)
			(- (ticks-index seq)
			   1)))))

;; These are empty classes to denote a generalised type of 'note' that
;; can be either noteon or noteoff, e.g midi however this allows the
;; same method to be reused by, e.g an osc synth
(defmethod record-gesture :around ((gesture note-on-midi-message) (seq free-sequence))
  (when (armed-and-ready seq)
    (push gesture (hanging-rec-tones seq)))
  (call-next-method))

(defgeneric note-off (note-on))
(defmethod note-off (thing)
  (declare (ignore thing))
  nil)

(defmethod note-off ((note-on note-on-midi-message))
  (let ((ret (make-instance 'note-off-midi-message
			    :raw-midi (copy-list (slot-value note-on
							     'midi-packetiser::raw-midi)))))
    (rplaca (slot-value ret 'midi-packetiser::raw-midi)
	    (logand #b1110 (car (slot-value ret 'midi-packetiser::raw-midi))))))

(defmethod read-gestures ((seq free-sequence))
  (loop for gesture in (aref (fs-memory seq)
			     (ticks-index seq))
     do (push gesture (hanging-play-tones seq)) ;; XXX hack! we can
						;; only read each
						;; gesture once - badish..
     collect gesture))

(defmethod drain-hanging-rec-tones ((seq free-sequence))
  (loop for rec-tone in (hanging-rec-tones seq)
     do (record-gesture (note-off rec-tone) seq))
  (setf (hanging-rec-tones seq) nil))
(defmethod drain-hanging-play-tones ((seq free-sequence))
  (loop for play-tone in (hanging-play-tones seq)
     collect (note-off play-tone))
  (setf (hanging-play-tones seq) nil))

(defmethod drain-hanging-tones ((seq free-sequence))
  (drain-hanging-rec-tones seq)
  (drain-hanging-play-tones seq))

(defmethod do-tick ((seq free-sequence))
  (when (play-state seq)
    (incf (ticks-index seq))
    (when (>= (ticks-index seq)
	      (sequence-tick-length seq))
      (case (play-state seq)
	(:push-extend (if (< (sequence-tick-length seq)
			     *max-free-seq-length*)
			  (incf (sequence-tick-length seq))
			  (progn (break "monkey")
				 (setf (play-state seq)
				       :repeat)
				 (setf (sequence-tick-length seq)
				       0))))
	(:repeat (setf (ticks-index seq)
		       (- (ticks-index seq)
			  (sequence-tick-length seq)))
		 (drain-hanging-tones seq)))))
  (ticks-index seq))

(defgeneric erase-sequence (sequence))
(defmethod erase-sequence ((seq free-sequence))
  (loop for i below (sequence-tick-length seq)
     do (setf (aref (fs-memory seq) i) nil))
  (setf (fill-pointer (fs-memory seq)) 0)
  (setf (rec-state seq) nil)
  (setf (play-state seq) nil)
  (setf (ticks-index seq) 0)
  (drain-hanging-play-tones seq))

(defgeneric copy-sequence (seq1 seq2))
(defmethod copy-sequence ((seq1 free-sequence) (seq2 free-sequence))
  (erase-sequence seq2)
  (setf (fill-pointer (fs-memory seq2))
	(fill-pointer (fs-memory seq1)))
  (setf (rec-state seq2) nil)
  (setf (play-state seq2) nil)
  (loop for i below (sequence-tick-length seq1)
     do (setf (aref (fs-memory seq2) i)
	      (aref (fs-memory seq1) i))))

(defmethod play-push-extend ((seq free-sequence))
  (setf (play-state seq)
	:push-extend))

(defmethod play-repeat ((seq free-sequence))
  (setf (play-state seq)
	:repeat))

(defmethod play-stop ((seq free-sequence))
  "returns any hanging tones we need to emit to avoid 'stuck' notes"
  (setf (play-state seq) nil)
  (rec-unarm seq)
  (drain-hanging-tones seq))

(defmethod rec-arm ((seq free-sequence))
  (setf (rec-state seq) :overdub))

(defmethod rec-unarm ((seq free-sequence))
  (setf (rec-state seq) nil)
  (drain-hanging-rec-tones seq))

(defmethod loop-cycle ((seq free-sequence))
  (case (play-state seq)
    (:repeat (play-stop seq))
    (:push-extend (play-repeat seq)
		  (rec-unarm seq))
    (otherwise (if (= 0 (fill-pointer (fs-memory seq)))
		   (progn (rec-arm seq)
			  (play-push-extend seq))
		   (play-repeat seq)))))
