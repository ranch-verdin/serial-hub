(in-package :sguenz)

(defun default-step-sequencer-triggers ()
  (list t (list (make-instance 'note-on-midi-message
			       :raw-midi '(144 42 53)) ;; closed hh
		(make-instance 'note-off-midi-message
			       :raw-midi '(144 46 81))
		(make-instance 'note-on-midi-message
			       :raw-midi '(144 38 68)) ;; snare
		(make-instance 'note-on-midi-message
			       :raw-midi '(144 35 111))) ;; kick
	:emph (list (make-instance 'note-on-midi-message
				   :raw-midi '(144 42 127)) ;; closed hh
		    (make-instance 'note-off-midi-message
				   :raw-midi '(144 46 81))
		    (make-instance 'note-on-midi-message
				   :raw-midi '(144 38 68)) ;; snare
		    (make-instance 'note-on-midi-message
				   :raw-midi '(144 36 111))) ;; kick
	))

(defclass section ()
  ((grid-seq :initform (make-grid-sequence 16 4
					   (default-step-sequencer-triggers)
					   :beat-divisor 4
					   :swing 0))
   (free-seq1 :initform (make-instance 'free-sequence))
   (free-seq2 :initform (make-instance 'free-sequence))
   (free-seq3 :initform (make-instance 'free-sequence))))

(defun new-section ()
  (make-instance 'section))

(defvar *sguenz-sections* (loop for i below 3 collect (new-section)))

(defvar *current-section* (car *sguenz-sections*))

(defvar *active-phrase* 0)

(defgeneric handle-event (ev))

(defmethod handle-event ((ev t))
  (warn "unknown event received: ~a" ev))

(defun section-a (up-or-down)
  (format t "section-a ~a~%" up-or-down)
  (setf *current-section* (car *sguenz-sections*)))

(defun section-b (up-or-down)
  (format t "section-b ~a~%" up-or-down)
  (setf *current-section* (cadr *sguenz-sections*)))

(defun section-c (up-or-down)
  (format t "section-c ~a~%" up-or-down)
  (setf *current-section* (caddr *sguenz-sections*)))

(defun phrase-selector (phrase-idx)
  (lambda (up-or-down)
    (format t "section-c-phrase ~a ~a~%" phrase-idx up-or-down)
    (when (eq :pressed up-or-down)
      (setf *active-phrase* phrase-idx))))

(defparameter *phrase-section-layout*
  (list (cons #'section-a
	      (mapcar #'phrase-selector
		      '(1 2 3 )))
	(cons #'section-b
	      (mapcar #'phrase-selector
		      '(1 2 3 )))
	(cons #'section-c
	      (mapcar #'phrase-selector
		      '(1 2 3 )))))

;; if we hold and press left-most, then a button in middle,
;; the pattern length can be changed
(defvar *setting-pattern-length* nil)

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
  (setf (grid-length (get-active-phrase))
	dur))

(defun ticker-strip (chan)
  (lambda (up-or-down)
    (case up-or-down
      (:press (if (= chan 0)
		  (setf *setting-pattern-length* t)
		  (if *setting-pattern-length*
		      (progn (set-quantised-pattern-duration (+ chan 1))
			     (setf *setting-pattern-length* nil))
		      (scrub-to-quantised-point chan))))
      (:release (when (= chan 0)
		  (when *setting-pattern-length*
		    (setf *setting-pattern-length* nil)
		    (scrub-to-quantised-point chan)))))))

(defun scrub-to-quantised-point (chan)
  (setf (ticks-index (get-active-phrase))
	(nth-value 1 (floor (+ (* *master-beat-divisor*
				  (round (- (* chan (sequence-tick-length (get-active-phrase)))
					    (ticks-index (get-active-phrase)))
					 *master-beat-divisor*))
			       (ticks-index (get-active-phrase)))
			    (* (grid-length (get-active-phrase))
			       (sequence-tick-length (get-active-phrase)))))))
(defun inc-ticker ()
  (prog1 (do-tick (get-active-phrase))
    (handle-gestures (get-active-phrase))
    (when (grid-crossing-point (get-active-phrase))
      (draw-grid))))

(defmethod handle-event ((mess clock-tick-midi-message))
  (declare (ignore mess))
  (inc-ticker))

(defparameter *ticker-strip-inputs*
  (loop for x below 16 collect (ticker-strip x)))

(defun play (up-or-down)
  (format t "play ~a~%" up-or-down))
(defun stop (up-or-down)
  (format t "stop ~a~%" up-or-down))
(defun overdub (up-or-down)
  (format t "overdub ~a~%" up-or-down))
(defun overwrite (up-or-down)
  (format t "overwrite ~a~%" up-or-down))
(defun del (up-or-down)
  (format t "del ~a~%" up-or-down))
(defun copy (up-or-down)
  (format t "copy ~a~%" up-or-down))
(defun timebase (up-or-down)
  (format t "timebase ~a~%" up-or-down))
(defun quantise (up-or-down)
  (format t "quantise ~a~%" up-or-down))
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
  (list (list #'play #'stop #'overdub #'overwrite)
	(list #'del #'copy #'timebase #'quantise)
	(list #'mute #'stop-all
	      #'emph
	      #'toggle-arrange-or-rec-mode)))

(defparameter *emph-state* :emph)

(defun step-sequencer-button (x y)
  (declare (optimize (debug 3)))
  (lambda (up-or-down)
    (format t "step-sequencer-button ~a ~a ~a~%" x y up-or-down)
    (symbol-macrolet ((button-emph (aref (grid (get-active-phrase)) x y)))
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
				    as cell = (aref (grid (get-active-phrase)) x y)
				    collect (or (and cell
						     (if (eq cell :emph)
							 15
							 5))
						0))))))
    (print whole-grid)
    (monome-map-intensities 0 0
			    (mapcar (lambda (row)
				      (subseq row 0 8))
				    whole-grid))
    (monome-map-intensities 8 0
			    (mapcar (lambda (row)
				      (subseq row 8))
				    whole-grid))))

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
		 (lambda (foo)
		   (when (eq foo :press)
		     (inc-ticker))))
	     (typecase event
	       (monome-button-press :press)
	       (monome-button-release :release)))))

(defmethod handle-gesture ((seq grid-sequence) (mess note-on-midi-message))
  (write-midi-message mess))

(defun draw-grid ()
  ;; (monome-set-all 0)
  (draw-step-sequencer)
  (monome-row-intensities 0 3
		  (loop for i below 8
		     collect (if (< i (grid-length (get-active-phrase)))
				 4
				 0)))
  (monome-row-intensities 8 3
		  (loop for i from 8 below 16
		     collect (if (< i (grid-length (get-active-phrase)))
				 4
				 0)))
  (monome-set-led-intensity (round (* (/ (ticks-index (get-active-phrase))
					 *master-beat-divisor*)
				      (beat-divisor (get-active-phrase))))
			    3 15)
  (monome-set-led-intensity 6 2 (if (eq *emph-state* :emph)
				    15
				    6)))

(defvar *cntrl-thread* nil)

(defun start-cntrl-app ()
  (assert (null *cntrl-thread*))
  (setf *cntrl-thread*
	(bt:make-thread (lambda ()
			  (with-midi-out (*default-midi-out-stream* "/dev/snd/midiC1D0")
			    (with-monome-output-stream ()
			      (unwind-protect
				   (loop (handle-event (? *reader-ochan*)))
				(setf *cntrl-thread* nil)))))
			:name "cntrl-app")))

(defun stop-cntrl-app ()
  (bt:destroy-thread *cntrl-thread*)
  (setf *cntrl-thread* nil))
