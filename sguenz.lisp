(in-package :sguenz)

(defun default-step-sequencer-triggers ()
  (list t (list (make-instance 'note-on-midi-message
			       :raw-midi '(144 42 53)) ;; closed hh
		(make-instance 'note-off-midi-message
			       :raw-midi '(144 47 81)) ;; tom Lo
		(make-instance 'note-on-midi-message
			       :raw-midi '(144 37 68)) ;; rimshot
		(make-instance 'note-on-midi-message
			       :raw-midi '(144 35 111))) ;; kick
	:emph (list (make-instance 'note-on-midi-message
				   :raw-midi '(144 46 53)) ;; open hh
		    (make-instance 'note-off-midi-message
				   :raw-midi '(144 48 81)) ;; Tom Hi
		    (make-instance 'note-on-midi-message
				   :raw-midi '(144 38 68)) ;; snare
		    (make-instance 'note-on-midi-message
				   :raw-midi '(144 36 111))) ;; long kick
	))

(defclass section ()
  ((grid-seq :initform (make-grid-sequence 16 4
					   (default-step-sequencer-triggers)
					   :beat-divisor 4
					   :swing 0))
   (free-seq1 :initform (make-instance 'free-sequence))
   (free-seq2 :initform (make-instance 'free-sequence))
   (free-seq3 :initform (make-instance 'free-sequence))))

(defmethod get-sequences ((sec section))
  (list (slot-value sec 'grid-seq)
	(slot-value sec 'free-seq1)
	(slot-value sec 'free-seq2)
	(slot-value sec 'free-seq3)))

(defvar *queued-section* nil)

(defun enqueue-section (idx)
  (setf *queued-section* (nth idx *sguenz-sections*)))

(defun seek-section-to (pos section)
  (mapcar (lambda (seq)
	    (if (> (sequence-tick-length seq)
		   0)
		(setf (ticks-index seq)
		      (mod (- pos 1)
			   (sequence-tick-length seq)))
		(setf (ticks-index seq)
		      0)))
	  (get-sequences section)))

(defun new-section ()
  (make-instance 'section))

(defvar *sguenz-sections* (loop for i below 3
			     collect (new-section)))

(defvar *current-section* (car *sguenz-sections*))

(defvar *active-phrase* 0)

(defgeneric handle-event (ev))

(defmethod handle-event ((ev t))
  (warn "unknown event received: ~a" ev))

(defun get-section-phrases (&optional (sec *current-section*))
  (list (slot-value sec 'grid-seq)
	(slot-value sec 'free-seq1)
	(slot-value sec 'free-seq2)
	(slot-value sec 'free-seq3)))

(defmethod handle-event ((ev midi-performance-gesture))
  (mapcar (lambda (seq)
	    (record-gesture ev seq))
	  (get-section-phrases *current-section*)))

(defun section-a (up-or-down)
  (format t "section-0 ~a~%" up-or-down)
  (when (eq up-or-down :press)
    (case *function-button-state*
      (:del (erase-sequence (get-active-grid)))
      (otherwise (enqueue-section 0)))))

(defun section-b (up-or-down)
  (format t "section-1 ~a~%" up-or-down)
  (when (eq up-or-down :press)
    (case *function-button-state*
      (:del (erase-sequence (get-active-grid)))
      (otherwise (enqueue-section 1)))))

(defun section-c (up-or-down)
  (format t "section-2 ~a~%" up-or-down)
  (when (eq up-or-down :press)
    (case *function-button-state*
      (:del (erase-sequence (get-active-grid)))
      (otherwise (enqueue-section 2)))))

(defparameter *function-button-state* nil)

(defun phrase-selector (phrase-group phrase-idx)
  (lambda (up-or-down)
    (format t "section-~a-phrase ~a ~a~%" phrase-group phrase-idx up-or-down)
    (when (eq :press up-or-down)
      (let* ((pushed-section (nth phrase-group *sguenz-sections*))
	     (pushed-sequence (nth phrase-idx (get-sequences pushed-section)))
	     (play-state-before (play-state pushed-sequence)))
	(case *function-button-state*
	  (:play (play-repeat pushed-sequence))
	  (:stop (play-stop pushed-sequence))
	  (:rec (rec-toggle pushed-sequence))
	  (:del (erase-sequence pushed-sequence))
	  (otherwise (let ((hung-gestures (loop-cycle pushed-sequence)))
		       (when (eq pushed-section *current-section*)
			 (mapcar #'transmit-gesture hung-gestures)

			 ;; XXX quick hack to quantise free-sequence
			 ;; lengths to 1 beat
			 (when (eq play-state-before :push-extend)
			   (setf (sequence-tick-length pushed-sequence)
				 (* *master-beat-divisor*
				    (max 1
					 (round (ticks-index pushed-sequence)
						*master-beat-divisor*))))
			   (setf *active-phrase* phrase-idx))))))))))

(defparameter *phrase-section-layout*
  (list (cons #'section-a
	      (mapcar (lambda (x)
			(phrase-selector 0 x))
		      '(1 2 3)))
	(cons #'section-b
	      (mapcar (lambda (x)
			(phrase-selector 1 x))
		      '(1 2 3)))
	(cons #'section-c
	      (mapcar (lambda (x)
			(phrase-selector 2 x))
		      '(1 2 3)))))

(defun get-active-phrase ()
  (cond ((= *active-phrase* 0)
	 (slot-value *current-section* 'grid-seq))
	((= *active-phrase* 1)
	 (slot-value *current-section* 'free-seq1))
	((= *active-phrase* 2)
	 (slot-value *current-section* 'free-seq2))
	((= *active-phrase* 3)
	 (slot-value *current-section* 'free-seq3))))

(defun set-quantised-pattern-duration (dur)
  (setf (grid-length (get-active-grid))
	dur))

(defparameter *ticker-strip-modifier-state* nil)

(defun ticker-strip (chan)
  (lambda (up-or-down)
    (when (eq :press up-or-down)
      (case *ticker-strip-modifier-state*
	(:swing (setf (swing-ratio (get-active-grid))
		      (/ chan 16)))
	(:timebase (setf (beat-divisor (get-active-grid))
			 (+ chan 1)))
	(:grid-length (set-quantised-pattern-duration
		       (+ 1 chan)))
	(otherwise (scrub-to-quantised-point chan))))))

(defun scrub-to-quantised-point (chan)
  (setf (grid-position (get-active-grid))
	chan))

(defmethod echo-gesture (mess)
  (declare (ignore mess))
  "don't blow up")

(defmethod echo-gesture ((mess midi-performance-gesture))
  (write-midi-message mess))

(defmethod transmit-gesture ((mess null))
  "don't blow up")

(defmethod transmit-gesture ((mess midi-performance-gesture))
  (write-midi-message mess))

(defun get-active-grid ()
  (slot-value *current-section* 'grid-seq))

(defvar *last-grid-draw* (get-internal-utime))
(defparameter *draw-frame-length* 10000)
(defun calculate-display-flashes ()
  (setf *flash-ticker* (mod (+ *flash-ticker* 1)
			    *flash-divisor*))
  (when (= 0 *flash-ticker*)
    ;; (print 'tick)
    (setf *fast-flash* (not *fast-flash*))
    (when *fast-flash*
      ;; (print 'tock)
      (setf *slow-flash* (not *slow-flash*)))))
(defparameter *flash-divisor* 5)
(defparameter *flash-ticker* 0)

(defparameter *fast-flash* nil)
(defparameter *slow-flash* nil)

(defun inc-ticker ()
  (prog1 (values (do-tick (slot-value *current-section* 'grid-seq))
		 (do-tick (slot-value *current-section* 'free-seq1))
		 (do-tick (slot-value *current-section* 'free-seq2))
		 (do-tick (slot-value *current-section* 'free-seq3)))
    (mapcar (lambda (phrase)
	      (mapcar #'transmit-gesture
		      (read-gestures phrase)))
	    (list (slot-value *current-section* 'grid-seq)
		  (slot-value *current-section* 'free-seq1)
		  (slot-value *current-section* 'free-seq2)
		  (slot-value *current-section* 'free-seq3)))
    (when (and (= (round (ticks-index (get-active-grid)))
		  (round (- (sequence-tick-length (get-active-grid))
			    1)))
	       *queued-section*)
      (seek-section-to 0 *queued-section*)
      (setf *current-section* *queued-section*)
      (setf *queued-section* nil))
    (draw-grid)))

(defmethod handle-event ((mess clock-tick-midi-message))
  ;; (write-midi-message mess *rang-output-stream*)
  (inc-ticker))

(defparameter *ticker-strip-inputs*
  (loop for x below 16 collect (ticker-strip x)))

(defun play (up-or-down)
  (format t "play ~a~%" up-or-down)
  (if (eq :press up-or-down)
      (setf *function-button-state* :play)
      (setf *function-button-state* nil)))
(defun stop (up-or-down)
  (format t "stop ~a~%" up-or-down)
  (if (eq :press up-or-down)
      (setf *function-button-state* :stop)
      (setf *function-button-state* nil)))
(defun rec (up-or-down)
  (format t "overdub ~a~%" up-or-down)
  (if (eq :press up-or-down)
      (setf *function-button-state* :rec)
      (setf *function-button-state* nil)))
(defun del (up-or-down)
  (format t "del ~a~%" up-or-down)
  (if (eq :press up-or-down)
      (setf *function-button-state* :del)
      (setf *function-button-state* nil)))
(defun appending-copy (up-or-down)
  (format t "appending copy ~a~%" up-or-down))
(defun layering-copy (up-or-down)
  (format t "layering copy ~a~%" up-or-down))
(defun set-grid-length (up-or-down)
  (format t "timebase ~a~%" up-or-down)
  (if (eq :press up-or-down)
      (setf *ticker-strip-modifier-state* :grid-length)
      (setf *ticker-strip-modifier-state* nil)))
(defun timebase (up-or-down)
  (format t "timebase ~a~%" up-or-down)
  (if (eq :press up-or-down)
      (setf *ticker-strip-modifier-state* :timebase)
      (setf *ticker-strip-modifier-state* nil)))
(defun swing (up-or-down)
  (format t "swing ~a~%" up-or-down)
  (if (eq :press up-or-down)
      (setf *ticker-strip-modifier-state* :swing)
      (setf *ticker-strip-modifier-state* nil)))
(defun mute (up-or-down)
  (format t "mute ~a~%" up-or-down))
(defun stop-all (up-or-down)
  (format t "stop-all ~a~%" up-or-down))
(defun emph (up-or-down)
  (when (eq up-or-down :press)
    (setf *emph-state*
	  (case *emph-state*
	    (:emph t)
	    (t :emph)))))
(defun toggle-arrange-or-rec-mode (up-or-down)
  (format t "toggle-arrange-or-rec-mode ~a~%" up-or-down))

(defparameter *function-buttons*
  (list (list #'play #'stop #'rec #'del)
	(list #'set-grid-length #'timebase #'swing #'emph)
	(list #'mute #'stop-all #'layering-copy #'appending-copy)))

(defparameter *emph-state* :emph)

(defun step-sequencer-button (x y)
  (declare (optimize (debug 3)))
  (lambda (up-or-down)
    (format t "step-sequencer-button ~a ~a ~a~%" x y up-or-down)
    (symbol-macrolet ((button-emph (aref (grid (get-active-grid)) x y)))
      (case up-or-down
	(:press (setf button-emph
		      (or (and (not button-emph)
			       *emph-state*)
			  (and (not (eq button-emph *emph-state*))
			       *emph-state*))))))))
(defun draw-step-sequencer ()
  (let ((whole-grid (append (loop repeat 4 collect
				 (loop repeat 16 collect 0))
			    (loop for y below 4
			       collect
				 (loop for x below 16
				    as cell = (aref (grid (get-active-grid)) x y)
				    collect (or (and cell
						     (if (eq cell :emph)
							 15
							 5))
						0))))))
    (monome-map-128 whole-grid)))

(defparameter *grid-section*
  (loop for y below 4
     collect (loop for x below 16
		collect (step-sequencer-button x y))))

(defparameter *whole-grid*
  (append (mapcar #'append *phrase-section-layout* *function-buttons*)
	  (list *ticker-strip-inputs*)
	  *grid-section*))

(defmethod handle-event ((event monome-button-event))
  (let ((x (slot-value event 'x))
	(y (slot-value event 'y)))
    (funcall (or (nth x (nth y *whole-grid*))
		 (lambda (z)
		   (format t "unused button ~a ~a ~a~%" x y z)
		   ;; (when (eq foo :press);; xx hack - just send
		   ;; 			;; boomerang taptempo on
		   ;; 			;; unused buttons for now
		   ;;   (calispel:! *boomerang-taptempo-chan*
		   ;; 		 (get-internal-utime)))
		   ))
	     (typecase event
	       (monome-button-press :press)
	       (monome-button-release :release))))
  (draw-grid))

(defun factors (n &aux (lows '()) (highs '()))
  (do ((limit (1+ (isqrt n))) (factor 1 (1+ factor)))
      ((= factor limit)
       (when (= n (* limit limit))
         (push limit highs))
       (remove-duplicates (nreconc lows highs)))
    (multiple-value-bind (quotient remainder) (floor n factor)
      (when (zerop remainder)
        (push factor lows)
        (push quotient highs)))))

(defun draw-grid-seq-ticker ()
  (let ((current-timebase (beat-divisor (get-active-grid)))
	(current-pattern-length (grid-length (get-active-grid))))
    (case *ticker-strip-modifier-state*
      (:swing (monome-row-intensities 0 3
				      (loop for i below 8
					 collect i))
	      (monome-row-intensities 8 3
				      (loop for i below 8
					 collect (+ i 8))))
      (:timebase (monome-row-intensities 0 3
					 (loop for i below 8
					    collect (if (< i (grid-length (get-active-grid)))
							4
							0)))
		 (monome-row-intensities 8 3
					 (loop for i from 8 below 16
					    collect (if (< i (grid-length (get-active-grid)))
							4
							0)))
		 (monome-set-led-intensity (round (* (/ (ticks-index (get-active-grid))
							*master-beat-divisor*)
						     (beat-divisor (get-active-grid))))
					   3 15)
		 (monome-set-led-intensity (- current-timebase 1)
					   3 15)
		 (loop for i in (factors current-pattern-length)
		    do (monome-set-led-intensity (- i 1)  3 8)))
      (:grid-length
       (monome-row-intensities 0 3
			       (loop for i below 8
				  collect (floor (* (/ 15 current-timebase)
						    (mod i current-timebase)))))
       (monome-row-intensities 8 3
			       (loop for i below 8
				  collect (floor (* (/ 15 current-timebase)
						    (mod (+ i 8) current-timebase)))))
       (monome-set-led-intensity (round (* (/ (ticks-index (get-active-grid))
					      *master-beat-divisor*)
					   (beat-divisor (get-active-grid))))
				 3 15))
      (otherwise
       (monome-row-intensities 0 3
			       (loop for i below 8
				  collect (if (< i (grid-length (get-active-grid)))
					      4
					      0)))
       (monome-row-intensities 8 3
			       (loop for i from 8 below 16
				  collect (if (< i (grid-length (get-active-grid)))
					      4
					      0)))
       (monome-set-led-intensity (round (* (/ (ticks-index (get-active-grid))
					      *master-beat-divisor*)
					   (beat-divisor (get-active-grid))))
				 3 15)))))

(defun draw-utility-button-states ()
  (monome-set-led-intensity 4 0 (if (eq *function-button-state* :play)
				    15
				    6))
  (monome-set-led-intensity 5 0 (if (eq *function-button-state* :stop)
				    15
				    8))
  (monome-set-led-intensity 6 0 (if (eq *function-button-state* :rec)
				    15
				    10))
  (monome-set-led-intensity 7 0 (if (eq *function-button-state* :del)
				    15
				    12))
  (monome-set-led-intensity 4 1 (if (eq *ticker-strip-modifier-state* :grid-length)
				    15
				    12))
  (monome-set-led-intensity 5 1 (if (eq *ticker-strip-modifier-state* :timebase)
				    15
				    10))
  (monome-set-led-intensity 6 1 (if (eq *ticker-strip-modifier-state* :swing)
				    15
				    8))
  (monome-set-led-intensity 7 1 (if (eq *emph-state* :emph)
				    15
				    6)) ;; emph button
  )

(defun fast-flash (on-intensity &optional (off-intensity 0))
  (if *fast-flash*
      on-intensity
      off-intensity))

(defun slow-flash (on-intensity &optional (off-intensity 0))
  (if *slow-flash*
      on-intensity
      off-intensity))

(defun draw-section-sequence-states ()
  (loop for section in *sguenz-sections*
     for i below (length *sguenz-sections*)
     do (monome-set-led-intensity 0 i (if (eq *current-section*
					      section)
					  15
					  6))
       (loop for j from 1 to 3
	  for seq in (cdr (get-sequences section))
	  do (unless (empty-p seq)
	       (monome-set-led-intensity j i
					 (if (play-state seq)
					     (if (rec-state seq)
						 (slow-flash 15)
						 15)
					     6))))))

(defun draw-grid ()
  ;; (monome-set-all 0)
  ;; (return-from draw-grid)
  (when (> (get-internal-utime) (+ *last-grid-draw* *draw-frame-length*))
    (calculate-display-flashes)
    (draw-step-sequencer)
    (draw-grid-seq-ticker)
    (draw-utility-button-states)
    (draw-section-sequence-states)
    (setf *last-grid-draw* (get-internal-utime))))

(defvar *sguenz-thread* nil)

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *default-midi-out-dev*
    (or (get-oss-midi-dev-named "E-MU")
	(get-virmidi 0))))

(defun sguenz-main ()
  (with-midi-oss-out (*default-midi-out-stream*
		      *default-midi-out-dev*)
    (with-monome-output ()
      ;; (with-midi-uart-out (*rang-output-stream* "/dev/ttyS2")
      (unwind-protect
	   (loop (let ((mess (? *reader-ochan*)))
		   (handle-event mess)
		   (echo-gesture mess)))
	(setf *sguenz-thread* nil)))))

(defun start-sguenz-app ()
  (assert (null *sguenz-thread*))

  ;; XXX hack eeeewwww....
  (ignore-errors (start-monome-reader))
  (sleep 0.5)
  (ignore-errors (start-midi-reader))
  (sleep 0.5)
  (setf *sguenz-thread*
	(bt:make-thread #'sguenz-main
			:name "sguenz-app")))

(defun stop-sguenz-app ()
  (bt:destroy-thread *sguenz-thread*)
  (setf *sguenz-thread* nil))

(defun sguenz-grab-focus ()
  (with-monome-output ()
    (grab-focus)
    (sleep 0.1)
    (draw-grid)))
