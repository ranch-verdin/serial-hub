(in-package :sguenz)

(defparameter *assigning-midi* nil)
(defvar *selected-trigger* '(t . 0))

(defun set-selected-trig-midi (ev seq)
  (when *selected-trigger*
    (setf (nth (cdr *selected-trigger*)
	       (getf (gesture-map seq)
		     (car *selected-trigger*)))
	  ev)))

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

(defparameter *gm-drum-notes*
  (mapcar (lambda (note-num)
	    (make-instance 'note-on-midi-message
			   :raw-midi `(144 ,note-num 127)))
	  (list
	   35 ;; Bass Drum 2
	   36 ;; Bass Drum 1
	   37 ;; Side Stick
	   38 ;; Snare Drum 1
	   39 ;; Hand Clap
	   40 ;; Snare Drum 2
	   41 ;; Low Tom 2
	   42 ;; Closed Hi-hat
	   43 ;; Low Tom 1
	   44 ;; Pedal Hi-hat
	   45 ;; Mid Tom 2
	   46 ;; Open Hi-hat
	   47 ;; Mid Tom 1
	   48 ;; High Tom 2
	   49 ;; Crash Cymbal 1
	   50 ;; High Tom 1
	   51 ;; Ride Cymbal 1
	   52 ;; Chinese Cymbal
	   53 ;; Ride Bell
	   54 ;; Tambourine
	   55 ;; Splash Cymbal
	   56 ;; Cowbell
	   )))

(defparameter *gm-drum-leds*
  (loop for i below 3
     collect (loop for j below 8
		collect 0)))

(defparameter *gm-drum-triggers*
  (loop for i below 3
     collect (loop for j below 7
		collect (let ((note-event (nth (+ (* i 7)
						  j)
					       *gm-drum-notes*))
			      (i i)
			      (j j))
			  (lambda (press)
			    (when (eq press :press)
			      (transmit-gesture note-event)
			      (handle-event note-event))
			    (setf (nth j (nth i *gm-drum-leds*))
				  (if (eq press :press)
				      15
				      0)))))))

(defclass section ()
  ((grid-seq :initform (make-grid-sequence 16 4
					   (default-step-sequencer-triggers)
					   :beat-divisor 4
					   :swing 0
					   :play-state :repeat))
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

(defun drain-section (section)
  (mapcar (lambda (seq)
	    (mapcar #'transmit-gesture (drain-hanging-tones seq)))
	  (get-sequences section)))

(defun seek-section-to (pos section)
  (drain-section section)
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

(defparameter *lookahead-list*
  (loop for i below (round (/ *master-beat-divisor* 2))
     collect nil))

(defun lookahead-store (ev)
  (push ev (car *lookahead-list*)))

(defun lookahead-tick ()
  (push nil *lookahead-list*)
  (setf *lookahead-list*
	(subseq *lookahead-list*
		0 (round (/ *master-beat-divisor* 2)))))

(defun lookahead-dump ()
  (loop for i in *lookahead-list*
	   append (loop for j in i
		     when j
		     collect j)))

(defmethod handle-event ((ev note-on-midi-message))
  (if *assigning-midi*
      (set-selected-trig-midi ev (get-active-grid))
      (call-next-method)))

(defmethod handle-event ((ev midi-performance-gesture))
  (unless *remix-record*
    (lookahead-store ev))
  (mapcar (lambda (seq)
	    (record-gesture ev seq))
	  (get-section-phrases *current-section*)))

(defparameter *copy-sources* nil)

(defun section-a (up-or-down)
  (section-n 0 up-or-down))
(defun section-b (up-or-down)
  (section-n 1 up-or-down))
(defun section-c (up-or-down)
  (section-n 2 up-or-down))

(defparameter *function-button-state* nil)

(defun section-n (n up-or-down)
  (format t "section-~a ~a~%" n up-or-down)
  (let ((this-seq (slot-value (nth n *sguenz-sections*) 'grid-seq)))
    (match (list up-or-down *function-button-state*)
      ((list :press :del)
       (erase-sequence this-seq))
      ((list :press :appending-copy-source)
       (push this-seq *copy-sources*))
      ((list :press :appending-copy-dest)
       (dolist (seq (reverse *copy-sources*))
	   (appending-copy-sequence seq this-seq))
       (setf *copy-sources* nil)
       (setf *function-button-state* nil))
      ((list :press :layering-copy-source)
       (pushnew this-seq *copy-sources*))
      ((list :press :layering-copy-dest)
       (dolist (seq (reverse *copy-sources*))
	   (layering-copy-sequence seq this-seq))
       (setf *copy-sources* nil)
       (setf *function-button-state* nil))
      ((list :press _)
       (enqueue-section n)))))

(defun phrase-selector (phrase-group phrase-idx)
  (declare (optimize (debug 3)))
  (lambda (up-or-down)
    (declare (optimize (debug 3)))
    (format t "section-~a-phrase ~a ~a~%" phrase-group phrase-idx up-or-down)
    (let* ((pushed-section (nth phrase-group *sguenz-sections*))
	   (pushed-sequence (nth phrase-idx (get-sequences pushed-section))))
      (match (list up-or-down *function-button-state*)
	((list :press :rec)
	 (rec-toggle pushed-sequence))
	((list :press :del)
	 (mapcar #'transmit-gesture (drain-hanging-tones pushed-sequence))
	 (erase-sequence pushed-sequence))
	((list :press :appending-copy-source)
	 (unless (empty-p pushed-sequence)
	   (push pushed-sequence *copy-sources*)))
	((list :press :appending-copy-dest)
	 (dolist (seq (reverse *copy-sources*))
	   (appending-copy-sequence seq pushed-sequence))
	 (setf *copy-sources* nil)
	 (setf *function-button-state* nil))
	((list :press :layering-copy-source)
	 (unless (empty-p pushed-sequence)
	   (pushnew pushed-sequence *copy-sources*)))
	((list :press :layering-copy-dest)
	 (dolist (seq (reverse *copy-sources*))
	   (layering-copy-sequence seq pushed-sequence))
	 (setf *copy-sources* nil)
	 (setf *function-button-state* nil))
	((list :press _)
	 (let ((play-state-before (play-state pushed-sequence)))
	   (loop-cycle pushed-sequence (lookahead-dump))
	   (when (eq pushed-section *current-section*)
	     (case play-state-before
	       (:push-extend
		;; (mapcar #'transmit-gesture (print )) ;; fixme -that ain't quite right
		(let ((hanging-tones (drain-hanging-tones pushed-sequence))
		      (snapped-ticks (* *master-beat-divisor*
					(max 1
					     (round (ticks-index pushed-sequence)
						    *master-beat-divisor*))))
		      (unsnapped-length (sequence-tick-length pushed-sequence)))
		  (when (< unsnapped-length snapped-ticks)
		    (print 'early)
		    (setf (sequence-tick-length pushed-sequence)
			  snapped-ticks)
		    (loop for i from unsnapped-length below snapped-ticks
		       do (setf (aref (sequencers::fs-memory pushed-sequence)
				      (- snapped-ticks 1))
				nil)
			 ))
		  (when (> unsnapped-length snapped-ticks)
		    (print 'late)
		    (loop for i from snapped-ticks below unsnapped-length
		       do (push (aref (sequencers::fs-memory pushed-sequence)
				      (- snapped-ticks 1))
				(aref (sequencers::fs-memory pushed-sequence)
				      i))
			 )
		    (loop for i below (-  unsnapped-length snapped-ticks)
		       do (mapcar #'transmit-gesture
				  (aref (sequencers::fs-memory pushed-sequence)
					i)))
		    (setf (sequence-tick-length pushed-sequence)
			  snapped-ticks))
		  (when (= snapped-ticks unsnapped-length)
		    (print 'on-time)
		    (setf (sequence-tick-length pushed-sequence)
			  snapped-ticks))
		  ;; (break "hanging tone cleanup ~a" hanging-tones)
		  (print (list 'hanging hanging-tones))
		  (setf (aref (sequencers::fs-memory pushed-sequence)
		  	      (- snapped-ticks 1))
		  	(append (aref (sequencers::fs-memory pushed-sequence)
		  		      (- snapped-ticks 1))
		  		hanging-tones))
		  (setf *active-phrase* phrase-idx))
		(when *remix-record*
		    (play-stop pushed-sequence)))
	       (:repeat
		(mapcar #'transmit-gesture (drain-hanging-tones pushed-sequence)))
	       (otherwise
		(drain-hanging-tones pushed-sequence)
		(mapcar #'transmit-gesture
			(read-gestures pushed-sequence)))))))))))

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

(defun get-active-phrases ()
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
      (case *lower-half-mode*
	(:grid (case *ticker-strip-modifier-state*
		 (:swing (setf (swing-ratio (get-active-grid))
			       (/ chan 16)))
		 (:timebase (setf (beat-divisor (get-active-grid))
				  (+ chan 1)))
		 (:grid-length (set-quantised-pattern-duration
				(+ 1 chan)))
		 (otherwise (scrub-to-quantised-point chan))))
	(:bass (bass-ticker-press chan))))))

(defun scrub-to-quantised-point (chan)
  (setf (grid-position (get-active-grid))
	chan))

(defmethod echo-gesture (mess)
  (declare (ignore mess))
  "don't blow up")

(defmethod echo-gesture ((mess midi-performance-gesture))
  (write-midi-message mess))

(defvar *remix-record* nil)

(defmethod transmit-gesture :around ((g midi-performance-gesture))
  (call-next-method)
  (when *remix-record*
    (handle-event g)))

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

(defvar *ticks-this-beat* 0)

(defun inc-ticker ()
  (prog1 (values (do-tick (slot-value *current-section* 'grid-seq))
		 (do-tick (slot-value *current-section* 'free-seq1))
		 (do-tick (slot-value *current-section* 'free-seq2))
		 (do-tick (slot-value *current-section* 'free-seq3)))
    (mapcar (lambda (phrase)
	        (when (eq (play-state phrase) :repeat)
		  (mapcar #'transmit-gesture
			  (read-gestures phrase))))
	    (list (slot-value *current-section* 'grid-seq)
		  (slot-value *current-section* 'free-seq1)
		  (slot-value *current-section* 'free-seq2)
		  (slot-value *current-section* 'free-seq3)))
    (when (and (= (round (ticks-index (get-active-grid)))
		  (round (- (sequence-tick-length (get-active-grid))
			    1)))
	       *queued-section*)
      (seek-section-to 0 *queued-section*)
      (drain-section *current-section*)
      (setf *current-section* *queued-section*)
      (setf *queued-section* nil))
    (lookahead-tick)
    ;; (print *ticks-this-beat*)
    (incf *ticks-this-beat*)
    (when (>= *ticks-this-beat* *master-beat-divisor*)
      (decf *ticks-this-beat* *master-beat-divisor*))))

(defmethod handle-event ((mess clock-tick-midi-message))
  (inc-ticker)
  (draw-grid))

(defmethod handle-event ((mess stop-midi-message))
  (drain-section *current-section*))

(defmethod handle-event ((mess start-midi-message))
  (setf *ticks-this-beat* 0)
  (seek-section-to 0 *current-section*))

(defmethod handle-event ((mess continue-midi-message))
  (print 'continue-midi))

(defparameter *ticker-strip-inputs*
  (loop for x below 16 collect (ticker-strip x)))

(defun midi-assign (up-or-down)
  (format t "midi-assign ~a~%" up-or-down)
  (setf *assigning-midi* (eq :press up-or-down)))
(defun remix-record (up-or-down)
  (format t "remix ~a~%" up-or-down)
  (when (eq :press up-or-down)
    (setf *remix-record* (not *remix-record*))))
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
  (format t "appending copy ~a~%" up-or-down)
  (if (eq :press up-or-down)
      (progn (setf *function-button-state* :appending-copy-source)
	     (setf *copy-sources* nil))
      (if *copy-sources*
	  (setf *function-button-state* :appending-copy-dest)
	  (setf *function-button-state* nil))))
(defun layering-copy (up-or-down)
  (format t "layering copy ~a~%" up-or-down)
  (if (eq :press up-or-down)
      (progn (setf *function-button-state* :layering-copy-source)
	     (setf *copy-sources* nil))
      (if *copy-sources*
	  (setf *function-button-state* :layering-copy-dest)
	  (setf *function-button-state* nil))))
(defun set-grid-length (up-or-down)
  (format t "grid-length ~a~%" up-or-down)
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
(defvar *lower-half-mode* :grid)
(defun lower-half-mode (up-or-down)
  (format t "lower-half-mode ~a~%" up-or-down)
  (when (eq :press up-or-down)
    (setf *lower-half-mode*
	  (case *lower-half-mode*
	    (:grid :bass)
	    (:bass :grid)))))

(defun sync (up-or-down)
  (format t "sync ~a~%" up-or-down)
  ;; FIXME maybe make this a modifier, to sync particular sequences in
  ;; current section?
  (seek-section-to *ticks-this-beat* *current-section*))
(defun emph (up-or-down)
  (format t "emph ~a~%" up-or-down)
  (if (eq up-or-down :press)
      (setf *emph-state* :emph)
      (setf *emph-state* t))
  (when *assigning-midi*
    (if (eq up-or-down :press)
	(setf (car *selected-trigger*) :emph)
	(setf (car *selected-trigger*) t))))
(defun toggle-arrange-or-rec-mode (up-or-down)
  (format t "toggle-arrange-or-rec-mode ~a~%" up-or-down))

(defun lower-half-button-1 (up-or-down)
  (case *lower-half-mode*
    (:grid (set-grid-length up-or-down))
    (:bass (setf *bass-grid-ticker-function* :volume))))
(defun lower-half-button-2 (up-or-down)
  (case *lower-half-mode*
    (:grid (timebase up-or-down))
    (:bass (setf *bass-grid-ticker-function* :octave))))
(defun lower-half-button-3 (up-or-down)
  (case *lower-half-mode*
    (:grid (set-grid-length up-or-down))
    (:bass (setf *bass-grid-ticker-function* :channel))))

(defparameter *function-buttons*
  (list (list #'midi-assign #'remix-record #'rec #'del)
	(list #'emph #'sync #'layering-copy #'appending-copy)
	(list #'lower-half-button-1 #'lower-half-button-2 #'lower-half-button-3 #'lower-half-mode)))

(defparameter *emph-state* t)

(defun step-sequencer-button (x y up-or-down)
  (declare (optimize (debug 3)))
  (format t "step-sequencer-button ~a ~a ~a~%" x y up-or-down)
  (setf *selected-trigger* (cons *emph-state* y))
  (unless *assigning-midi*
    (symbol-macrolet ((button-emph (aref (grid (get-active-grid)) x y)))
      (case up-or-down
	(:press (setf button-emph
		      (match (print (list *emph-state* button-emph))
			((list :emph nil)
			 (setf button-emph :emph))
			((list :emph t)
			 (setf button-emph :emph))
			((list :emph :emph)
			 (setf button-emph t))
			((list t nil)
			 (setf button-emph t))
			((list t t)
			 (setf button-emph nil))
			((list t :emph)
			 (setf button-emph nil))
			)))))))

(defun draw-step-sequencer ()
  (append (loop for y below 4
	     collect
	       (loop for x below 16
		  as cell = (aref (grid (get-active-grid)) x y)
		  collect (or (and cell
				   (if (eq cell :emph)
				       15
				       5))
			      (if (and *assigning-midi*
				       (= y (cdr *selected-trigger*)))
				  (if (eq (car *selected-trigger*) :emph)
				      (slow-flash 15)
				      (slow-flash 5))
				  0))))))

(defparameter *bass-grid-leds*
  (loop for i below 4
     collect (loop for j below 16
		collect 0)))
(defvar *bass-grid-volume* 127)
(defvar *bass-grid-midi-channel* 2)
(defvar *bass-grid-root-note* 48)
(defvar *bass-grid-ticker-function* :octave)

(defun bass-ticker-press (chan)
  (case *bass-grid-ticker-function*
    (:octave (when (< (* chan 12) 128)
	       (setf *bass-grid-root-note* (* chan 12))))
    (:volume (setf *bass-grid-volume* (min (* chan 8) 127)))
    (:channel (setf *bass-grid-midi-channel* chan))))

(defun bass-grid-ticker-pos ()
  (case *bass-grid-ticker-function*
    (:octave (floor *bass-grid-root-note* 12))
    (:volume (floor *bass-grid-volume* 8))
    (:channel *bass-grid-midi-channel*)))
(defun draw-bass-grid-ticker-strip ()
  (loop for i below 16
     collect (if (= (bass-grid-ticker-pos) i)
		 15
		 4)))

(defun bass-button (j i up-or-down)
  (let* ((note-idx (+ *bass-grid-root-note*
		      j (* (- 3 i) 5)))
	 (g (if (eq :press up-or-down)
	       (make-midi-note-on *bass-grid-midi-channel*
				  note-idx)
	       (make-midi-note-off *bass-grid-midi-channel*
				   note-idx))))
    (setf (nth j (nth i *bass-grid-leds*))
	  (if (eq :press up-or-down)
	      15
	      0))
    (print (list up-or-down j i))
    (transmit-gesture g)
    (handle-event g)))

(defparameter *bottom-half-buttons*
  (loop for y below 4
     collect (loop for x below 16
		collect (let ((y y)
			      (x x))
			  (lambda (up-or-down)
			    (case *lower-half-mode*
			      (:grid (step-sequencer-button x y up-or-down))
			      (:bass (bass-button x y up-or-down))))))))

(defparameter *whole-grid*
  (append (mapcar #'append *phrase-section-layout* *function-buttons* *gm-drum-triggers*)
	  (list *ticker-strip-inputs*)
	  *bottom-half-buttons*))

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
      (:swing (loop for i below 16
		 collect i))
      (:timebase (loop for i below 16
		    collect (cond ((= i (- current-timebase 1))
				   15)
				  ((= i (floor (* (/ (ticks-index (get-active-grid))
						     *master-beat-divisor*)
						  (beat-divisor (get-active-grid)))))
				   12)
				  ((find (+ i 1) (factors current-pattern-length))
				   8)
				  ((< i current-pattern-length)
				   2)
				  (t 0))))
      (:grid-length
       (loop for i below 16
	  collect (cond ((= i (floor (* (/ (ticks-index (get-active-grid))
					   *master-beat-divisor*)
					(beat-divisor (get-active-grid)))))
			 15)
			(t (floor (* (/ 12 current-timebase)
				     (mod i current-timebase)))))))
      (otherwise
       (loop for i below 16
	  collect (cond ((= i (floor (* (/ (cache-grid-crossing-point (get-active-grid))
					  *master-beat-divisor*)
				       (beat-divisor (get-active-grid)))))
			 15)
			((< i (grid-length (get-active-grid)))
			 4)
			(t 0)))))))

(defun draw-utility-button-states ()
  (list (list (if *assigning-midi*
		  15
		  4)
	      (if *remix-record*
		  15
		  6)
	      (if (eq *function-button-state* :rec)
		  15
		  8)
	      (if (eq *function-button-state* :del)
		  15
		  10))
	(list (if (eq *emph-state* :emph)
		  15
		  4) ;; emph button
	      6      ;; sync button
	      8      ;; layering copy button
	      10)    ;; appending copy button
	(case *lower-half-mode*
	  (:grid (list (if (eq *ticker-strip-modifier-state* :grid-length)
			  15
			  4)
		      (if (eq *ticker-strip-modifier-state* :timebase)
			  15
			  6)
		      (if (eq *ticker-strip-modifier-state* :swing)
			  15
			  8)
		      15))
	  (:bass (list (if (eq *bass-grid-ticker-function* :volume)
			   15
			   4)
		       (if (eq *bass-grid-ticker-function* :octave)
			   15
			   6)
		       (if (eq *bass-grid-ticker-function* :channel)
			   15
			   8)
		       10)))))

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
     collect (cons (if (eq *current-section*
			   section)
		       (if (find (car (get-sequences section))
				 *copy-sources*)
			   (fast-flash 15)
			   (if (eq section *queued-section*)
			       (slow-flash 15)
			       15))
		       (if (find (car (get-sequences section))
				 *copy-sources*)
			   (fast-flash 6)
			   (if (eq section *queued-section*)
			       (slow-flash 6)
			       6)))
		   (loop for j from 1 to 3
		      for seq in (cdr (get-sequences section))
		      collect (if (empty-p seq)
				  0
				  (if (play-state seq)
				      (if (rec-state seq)
					  (slow-flash 15)
					  (if (find (nth j (get-sequences section))
						    *copy-sources*)
					      (fast-flash 15)
					      15))
				      (if (find (nth j (get-sequences section))
						*copy-sources*)
					  (fast-flash 6)
					  6)))))))

(defvar *sguenz-has-focus* t)

;; FIXME this should be handled in monome/OSC backend.  draw-grid
;; should be a callback, circumventing the need for app to know about
;; focus
;; (defmethod handle-event ((event monome-focus-event))
;;   (setf *sguenz-has-focus* (focus event))
;;   (draw-grid))

(defun draw-grid ()
  ;; (monome-set-all 0)
  ;; (return-from draw-grid)
  (when (and *sguenz-has-focus*
	     (> (get-internal-utime) (+ *last-grid-draw* *draw-frame-length*)))
    (calculate-display-flashes)
    (monome-map-128 (append (mapcar #'append
				    (draw-section-sequence-states)
				    (draw-utility-button-states)
				    *gm-drum-leds*)
			    (list (case *lower-half-mode*
				    (:grid (draw-grid-seq-ticker))
				    (:bass (draw-bass-grid-ticker-strip))))
			    (case *lower-half-mode*
			      (:grid (draw-step-sequencer))
			      (:bass *bass-grid-leds*))))
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
      ;; (grab-focus)
      ;; (with-midi-uart-out (*rang-output-stream* "/dev/ttyS2")
      (unwind-protect
	   (loop (let ((mess (? *reader-ochan*)))
		   (handle-event mess)
		   ;; (echo-gesture mess)
		   ))
	(setf *sguenz-thread* nil)))))

(defun start-sguenz-app ()
  (assert (null *sguenz-thread*))

  ;; XXX hack eeeewwww....
  (ignore-errors (start-monome-reader))
  (sleep 0.1)
  (ignore-errors (start-midi-reader))
  (sleep 0.1)
  (setf *sguenz-thread*
	(bt:make-thread #'sguenz-main
			:name "sguenz-app")))

(defun stop-sguenz-app ()
  (bt:destroy-thread *sguenz-thread*)
  (setf *sguenz-thread* nil))

(defun sguenz-grab-focus ()
  (with-monome-output ()
    ;; (grab-focus)
    (sleep 0.1)
    (draw-grid)))
