(in-package :cntrl)

(defparameter *current-section* :a)

(defgeneric handle-event (ev))

(defmethod handle-event ((ev t))
  (warn "unknown event received: ~a" ev))

(defun section-a (up-or-down)
  (format t "section-a ~a~%" up-or-down)
  (case up-or-down
    (setf *current-section* :a)))
(defun section-a-phrase (phrase-idx)
  (lambda (up-or-down)
    (format t "section-a-phrase ~a ~a~%" phrase-idx up-or-down)))

(defun section-b (up-or-down)
  (format t "section-b ~a~%" up-or-down)
  (setf *current-section* :b))
(defun section-b-phrase (phrase-idx)
  (lambda (up-or-down)
    (format t "section-b-phrase ~a ~a~%" phrase-idx up-or-down)))

(defun section-c (up-or-down)
  (format t "section-c ~a~%" up-or-down)
  (setf *current-section* :c))
(defun section-c-phrase (phrase-idx)
  (lambda (up-or-down)
    (format t "section-c-phrase ~a ~a~%" phrase-idx up-or-down)))

(defparameter *phrase-section-layout*
  (list (cons #'section-a
	      (mapcar #'section-a-phrase
		      '(1 2 3 )))
	(cons #'section-b
	      (mapcar #'section-b-phrase
		      '(1 2 3 )))
	(cons #'section-c
	      (mapcar #'section-c-phrase
		      '(1 2 3 )))))

(defvar *ticker-pos* 0)

(defvar *ticker-timebase* 4)

(defvar *master-timebase* 24)

(defun ticker-strip (chan)
  (declare (ignore chan))
  (lambda (up-or-down)
    (format t "ticker-strip ~a~%" up-or-down)))

(defun inc-ticker-quantised ()
  (setq *ticker-pos*
	(rem (+ *ticker-pos* (/ *master-timebase* *ticker-timebase*))
	     (* (/ *master-timebase* *ticker-timebase*)
		16))))

(defun inc-ticker ()
  (setq *ticker-pos*
	(rem (+ *ticker-pos* 1)
	     (* (/ *master-timebase* *ticker-timebase*)
		16)))
  (multiple-value-bind (int frac) (floor *ticker-pos*
					 (/ *master-timebase* *ticker-timebase*))
    (when (= 0 frac)
      (draw-grid)
      ;; (with-midi-out (ms "/dev/snd/midiC1D0")
      (loop for trig in (read-step-sequencer-column int)
	 for message in *step-sequencer-triggers*
	 when trig
	 do (write-midi-message message)))))

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
(defun assign-step-sequencer-channel (up-or-down)
  (format t "assign-step-sequencer-channel ~a~%" up-or-down))
(defun toggle-arrange-or-rec-mode (up-or-down)
  (format t "toggle-arrange-or-rec-mode ~a~%" up-or-down))

(defparameter *function-buttons*
  (list (list #'play #'stop #'overdub #'overwrite)
	(list #'del #'copy #'timebase #'quantise)
	(list #'mute #'stop-all
	      #'assign-step-sequencer-channel
	      #'toggle-arrange-or-rec-mode)))

(defvar *step-sequencer-grid* (loop for y below 4
				 collect (loop for x below 16
					    collect nil)))

(defvar *step-sequencer-triggers*
  (list (make-instance 'note-on-midi-message
		       :raw-midi '(144 35 111)) ;; kick
	(make-instance 'note-on-midi-message
		       :raw-midi '(144 38 68)) ;; snare
	(make-instance 'note-on-midi-message
		       :raw-midi '(144 42 53)) ;; closed hh
	(make-instance 'note-off-midi-message
		       :raw-midi '(144 46 81))))

(defun read-step-sequencer-column (x)
  (mapcar (lambda (row)
	    (nth x row))
	  *step-sequencer-grid*))

(defun step-sequencer-button (x y)
  (declare (optimize (debug 3)))
  (lambda (up-or-down)
    (format t "step-sequencer-button ~a ~a ~a~%" x y up-or-down)
    (print up-or-down)
    (case up-or-down
      (:press (setf (nth x (nth y *step-sequencer-grid*))
		    (not (nth x (nth y *step-sequencer-grid*))))))))

(defun draw-step-sequencer ()
  (loop for y below 4
     do 
       (loop for x below 16
	  as cell = (nth x (nth y *step-sequencer-grid*))
	  when cell
	  do (monome-set-led-intensity x (+ y 4) 15))))

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
		     (inc-ticker)
		     (draw-grid))))
	     (typecase event
	       (monome-button-press :press)
	       (monome-button-release :release)))))

(defun draw-grid ()
  (monome-set-all 0)
  (draw-step-sequencer)
  (monome-set-led-intensity (floor (/ (* *ticker-pos* *ticker-timebase*)
				      *master-timebase*))
			    3 15))

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
