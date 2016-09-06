(in-package :boomerang)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *midi-dev* (get-oss-midi-dev-named "E-MU")))

(defun setup-boomerang-dev (&optional (dev-file *midi-dev*))
  (external-program:run "stty" (list "-F" dev-file "115200" "sane" "-brkint" "-icrnl" "-opost" "-onlcr" "-isig" "-icanon" "-iexten" "-echo" "-echoe")))

(defun midi-sniff (&optional (midi-dev *midi-dev*))
  (setup-boomerang-dev midi-dev)
  (with-open-file (stream midi-dev
			  :direction :io
			  :if-exists :overwrite
			  :element-type '(unsigned-byte 8))
    (loop (print (read-byte stream)))))

(defun write-bytes (bytes stream)
  (dolist (byte bytes)
    (write-byte byte stream)))

(defvar *rang-output-stream* nil)


(defmacro with-rang-output-stream (&body body)
  `(with-open-file (*rang-output-stream* (or *midi-dev*
					     (get-oss-midi-dev-named "E-MU"))
					 :direction :output
					 :if-exists :overwrite
					 :element-type '(unsigned-byte 8))
     ,@body))

(defvar *rang-input-stream* nil)

(defmacro with-rang-input-stream (&body body)
  `(with-open-file (*rang-input-stream* (or *midi-dev*
					     (get-oss-midi-dev-named "E-MU"))
					:direction :io
					:if-exists :overwrite
					:element-type '(unsigned-byte 8))
     ,@body))

(defun send-brosync-cmd (byte)
  (write-bytes `(240 0 31 127 15 ,byte)
	       *rang-output-stream*))

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant +brosync-sync-message+ 19)
  (defparameter +brosync-toggle-message+ 18)
  (defparameter +brosync-play-stop-all-message+ 8)
  (defparameter +brosync-cancel-rec-message+ 3)
  (defconstant +sysex-begin+ #xF0))

(defun send-brosync-sync ()
  (send-brosync-cmd +brosync-sync-message+))
(defun send-brosync-toggle ()
  (send-brosync-cmd +brosync-toggle-message+))
(defun send-brosync-play-stop-all()
  (send-brosync-cmd +brosync-play-stop-all-message+))
(defun send-brosync-cancel-rec-master ()
  (send-brosync-cmd +brosync-cancel-rec-message+))

(defvar *brosync-state* :empty)
(defvar *brosync-loop-origin* 0)
(defvar *brosync-loop-duration* 0)

(defun brosync-handle-message (byte recv-time)
  (case *brosync-state*
    (:empty (brosync-empty-handle-message byte recv-time))

    (:stopped-master (brosync-stopped-master-handle-message byte recv-time))
    (:rec-master (brosync-rec-master-handle-message byte recv-time))
    (:play-master (brosync-play-master-handle-message byte recv-time))

    (:stopped-slave (brosync-stopped-slave-handle-message byte recv-time))
    (:rec-slave (brosync-rec-slave-handle-message byte recv-time))
    (:play-slave (brosync-play-slave-handle-message byte recv-time)))
  (format t "~%after recving byte ~A, brosync state is: ~A" byte *brosync-state*))

(defun brosync-empty-handle-message (message recv-time)
  (case message
    (#.+brosync-toggle-message+ (setf *brosync-state* :rec-slave)
				(setf *brosync-loop-origin*
				      recv-time))
    (#.+brosync-cancel-rec-message+ nil)
    (#.+brosync-play-stop-all-message+ nil)
    (#.+brosync-sync-message+)))

(defun brosync-stopped-master-handle-message (message recv-time)
  (declare (ignore recv-time))
  (case message
    (#.+brosync-cancel-rec-message+)
    (#.+brosync-play-stop-all-message+)
    (#.+brosync-toggle-message+)
    (#.+brosync-sync-message+)))

(defun brosync-rec-master-handle-message (message recv-time)
  (declare (ignore recv-time))
  (case message
    (#.+brosync-cancel-rec-message+)
    (#.+brosync-play-stop-all-message+)
    (#.+brosync-toggle-message+)
    (#.+brosync-sync-message+)))

(defun brosync-play-master-handle-message (message recv-time)
  (declare (ignore recv-time))
  (case message
    (#.+brosync-cancel-rec-message+)
    (#.+brosync-play-stop-all-message+)
    (#.+brosync-toggle-message+)
    (#.+brosync-sync-message+)))

(defvar *ticker-thread* nil)

(defun stop-ticker ()
  (ignore-errors (bt:destroy-thread *ticker-thread*)))

(defparameter *clock-subdivision* 48)

(defun usleep (usecs)
  (when (> usecs 0)
    (sleep (/ usecs 1000000))))

(defun boomerang-tick-func ()
  ;; (serial-trigger-param 0 1)
  (calispel:! serial-hub-utils:*reader-ochan*
	      (make-instance 'midi-packetiser:clock-tick-midi-message))
  ;; (print 'tick)
  )

(defparameter *boomerang-taptempo-chan* (make-nonblock-buf-channel))
(defparameter *last-tap* (get-internal-utime))
(defparameter *max-tap-time* 1500000)

(defun start-ticker (loop-origin loop-duration)
  (stop-ticker)
  (setf *ticker-thread*
	(bt:make-thread
	 (lambda ()
	   (loop
	      (incf loop-origin loop-duration)
	      (let ((div *clock-subdivision*))
		(loop for i below div
		   do
		     (multiple-value-bind (value success?)
			 (calispel:? *boomerang-taptempo-chan* 0)
		       (when success?
			 (let ((tap-intvl (- (get-internal-utime)
					     *last-tap*)))
			   (when (< tap-intvl *max-tap-time*)
			     (setf *clock-subdivision*
				   (* 24
				      (round (/ *brosync-loop-duration*
						(- value *last-tap*)))))))
			 (setf *last-tap* (get-internal-utime))))
		     (boomerang-tick-func)
		     (usleep (/ (- loop-origin
				   (get-internal-utime))
				(- div i))))))))))

(defun brosync-stopped-slave-handle-message (message recv-time)
  (case message
    (#.+brosync-cancel-rec-message+)
    (#.+brosync-play-stop-all-message+)
    (#.+brosync-toggle-message+ (setf *brosync-state* :rec-slave)
				(setf *brosync-loop-origin*
				      recv-time))
    (#.+brosync-sync-message+ (start-ticker (get-internal-utime)
					    *brosync-loop-duration*)
			      (setf *brosync-state* :play-slave))))

(defun brosync-rec-slave-handle-message (message recv-time)
  (case message
    (#.+brosync-cancel-rec-message+ (setf *brosync-state* :empty))
    (#.+brosync-play-stop-all-message+)
    (#.+brosync-toggle-message+ (setf *brosync-state* :play-slave)
				(setf *brosync-loop-duration*
				      (- recv-time
					 *brosync-loop-origin*))
				(start-ticker (+ *brosync-loop-origin*
						 *brosync-loop-duration*)
					      *brosync-loop-duration*))
    (#.+brosync-sync-message+ (setf *brosync-state* :play-slave)
			      (start-ticker *brosync-loop-origin*
					    *brosync-loop-duration*))))

(defun brosync-play-slave-handle-message (message recv-time)
  (declare (ignore recv-time))
  (case message
    (#.+brosync-cancel-rec-message+)
    (#.+brosync-play-stop-all-message+ (setf *brosync-state* :stopped-slave)
				       (stop-ticker))
    (#.+brosync-toggle-message+ (setf *brosync-state* :stopped-slave)
				(stop-ticker))
    (#.+brosync-sync-message+)))

(defun brosync-listener ()
  (with-rang-input-stream
    (let ((state :open)
	  (sysex-counter 0)
	  (packet nil)
	  (recv-time (get-internal-utime)))
      (loop 
	 (let ((new-byte (read-byte *rang-input-stream*)))
	   (case state
	     (:open (case new-byte
		      (#.+sysex-begin+ (setf state :sysex)
				       (setf sysex-counter 5)
				       (push new-byte packet)
				       (setf recv-time (get-internal-utime)))))
	     (:sysex (decf sysex-counter)
		     (push new-byte packet)
		     (when (= sysex-counter 0)
		       (progn (brosync-handle-message new-byte recv-time)
			      (setf packet nil)
			      (setf state :open))))))))))

(defun start-brosync-sync ()
  (bt:make-thread #'brosync-listener :name "brosync-sync"))

