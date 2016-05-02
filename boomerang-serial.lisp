(in-package :cl-user)
(defun midi-sniff ()
  (with-open-file (stream "/dev/midi"
			  :direction :io
			  :if-exists :overwrite
			  :element-type '(unsigned-byte 8))
    (loop (print (read-byte stream)))))

(defun write-bytes (bytes stream)
  (dolist (byte bytes)
    (write-byte byte stream)))

(defvar *rang-output-stream* nil)

(defvar *midi-dev* "/dev/midi4")

(defmacro with-rang-output-stream (&body body )
  `(with-open-file (*rang-output-stream* *midi-dev*
					 :direction :output
					 :if-exists :overwrite
					 :element-type '(unsigned-byte 8))
     ,@body))

(defvar *rang-input-stream* nil)

(defmacro with-rang-input-stream (&body body)
  `(with-open-file (*rang-input-stream* *midi-dev*
			   :direction :io
			   :if-exists :overwrite
			   :element-type '(unsigned-byte 8))
     ,@body))

(defun send-brosync-cmd (byte)
  (write-bytes (print `(240 0 31 127 15 ,byte))
	       *rang-output-stream*))

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant +brosync-sync-message+ 19)
  (defparameter +brosync-toggle-message+ 18)
  (defparameter +brosync-play-stop-all-message+ 8)
  (defparameter +brosync-cancel-rec-message+ 3))

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

(defparameter *clock-subdivision* (* 16 2))

(defun get-internal-utime ()
  (multiple-value-bind (s us) (sb-ext:get-time-of-day)
    (+ (* 1000000 s)
       us)))

(defun usleep (usecs)
  (when (> usecs 0)
    (sleep (/ usecs 1000000))))

(defun aleph-tick-func ()
  ;; (serial-trigger-param 0 1)
  (serial-trigger-in 51 1)
  ;; (print 'tick)
  )

(defun start-ticker (loop-origin loop-duration)
  (stop-ticker)
  (setf *ticker-thread*
	(bt:make-thread
	 (lambda ()
	   (with-aleph-output-stream
	     (loop
		(incf loop-origin loop-duration)
		(let ((div *clock-subdivision*))
		  (loop for i below div
		     do
		       (aleph-tick-func)
		       (usleep (/ (- loop-origin
				     (get-internal-utime))
				  (- div i)))))))))))

(defun brosync-stopped-slave-handle-message (message recv-time)
  (declare (ignore recv-time))
  (case message
    (#.+brosync-cancel-rec-message+)
    (#.+brosync-play-stop-all-message+)
    (#.+brosync-toggle-message+ (start-ticker (get-internal-utime)
					      *brosync-loop-duration*)
				(setf *brosync-state* :play-slave))
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
    (+brosync-sync-message+)))

(defun brosync-play-slave-handle-message (message recv-time)
  (declare (ignore recv-time))
  (case message
    (#.+brosync-cancel-rec-message+)
    (#.+brosync-play-stop-all-message+ (setf *brosync-state* :stopped-slave)
				       (stop-ticker))
    (#.+brosync-toggle-message+ (setf *brosync-state* :stopped-slave)
				(stop-ticker))
    (#.+brosync-sync-message+)))

(defconstant +sysex-begin+ #xF0)

(defun brosync-listen ()
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

