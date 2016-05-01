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

(defvar *brosync-state* :stopped)
(defvar *brosync-loop-origin* 0)
(defvar *brosync-loop-duration* 0)

(defun brosync-handle-message (byte)
  (case *brosync-state*
    (:empty (brosync-empty-handle-message byte))

    (:stopped-master (brosync-stopped-master-handle-message byte))
    (:rec-master (brosync-rec-master-handle-message byte))
    (:play-master (brosync-play-master-handle-message byte))

    (:stopped-slave (brosync-stopped-slave-handle-message byte))
    (:rec-slave (brosync-rec-slave-handle-message byte))
    (:play-slave (brosync-play-slave-handle-message byte)))
  (format t "~%after recving byte, brosync state is: ~A" *brosync-state*))

(defun brosync-empty-handle-message (message)
  (case message
    (#.+brosync-toggle-message+ (setf *brosync-state* :rec-slave)
				(setf *brosync-loop-origin*
				      (get-internal-real-time)))
    (#.+brosync-cancel-rec-message+ nil)
    (#.+brosync-play-stop-all-message+ nil)
    (#.+brosync-sync-message+)))

(defun brosync-stopped-master-handle-message (message)
  (case message
    (#.+brosync-cancel-rec-message+)
    (#.+brosync-play-stop-all-message+)
    (#.+brosync-toggle-message+)
    (#.+brosync-sync-message+)))

(defun brosync-rec-master-handle-message (message)
  (case message
    (#.+brosync-cancel-rec-message+)
    (#.+brosync-play-stop-all-message+)
    (#.+brosync-toggle-message+)
    (#.+brosync-sync-message+)))

(defun brosync-play-master-handle-message (message)
  (case message
    (#.+brosync-cancel-rec-message+)
    (#.+brosync-play-stop-all-message+)
    (#.+brosync-toggle-message+)
    (#.+brosync-sync-message+)))

(defvar *ticker-thread* nil)
(defun brosync-stopped-slave-handle-message (message)
  (setf *brosync-state* :empty)
  (ignore-errors (bt:destroy-thread *ticker-thread*))
  (brosync-empty-handle-message message))

(defun brosync-rec-slave-handle-message (message)
  (case message
    (#.+brosync-cancel-rec-message+)
    (#.+brosync-play-stop-all-message+)
    (#.+brosync-toggle-message+ (setf *brosync-state* :play-slave)
				(print
				 (setf *brosync-loop-duration*
				       (- (get-internal-real-time)
					  *brosync-loop-origin*)))
				(ignore-errors (bt:destroy-thread *ticker-thread*))
				(setf *ticker-thread*
				      (bt:make-thread
				       (lambda ()
					 (with-open-file (stream "/dev/ttyACM0"
								 :direction :output
								 :if-exists :overwrite
								 :element-type '(unsigned-byte 8))
					   
					   (loop (serial-trigger-in stream 34 1)
					      (sleep (/ *brosync-loop-duration*
							32000))
					      (serial-trigger-in stream 34 0)
					      (sleep (/ *brosync-loop-duration*
							32000))))))))
    (+brosync-sync-message+)))

(defun brosync-play-slave-handle-message (message)
  (case message
    (#.+brosync-cancel-rec-message+)
    (#.+brosync-play-stop-all-message+)
    (#.+brosync-toggle-message+ (setf *brosync-state* :stopped-slave))
    (#.+brosync-sync-message+)))

(defun brosync-listen ()
  (with-rang-input-stream
    (let ((state :open)
	  (sysex-counter 0)
	  (packet nil))
      (loop 
	 (let ((new-byte (read-byte *rang-input-stream*)))
	   (case state
	     (:open (case new-byte
		      (240 (setf state :sysex)
			   (setf sysex-counter 5)
			   (push new-byte packet))))
	     (:sysex (decf sysex-counter)
		     (push new-byte packet)
		     (when (= sysex-counter 0)
		       (progn (brosync-handle-message new-byte)
			      (setf packet nil)
			      (setf state :open))))))))))
