(in-package :monome-glue)

(defvar *monome-reader-thread* nil)

(defun start-monome-reader ()
  ;; (assert (null *monome-dev*))
  (assert (null *monome-reader-thread*))
  (setq *monome-reader-thread*
       	(bt:make-thread (lambda ()
			  (setup-monome-dev)
			  (with-monome-output ()
			    (grab-focus))
			  (with-monome-input ()
			    (loop (! *reader-ochan*
				     (monome-receive-message)))))
			:name "monome reader thread")))
(defun stop-monome-reader ()
  (bt:destroy-thread *monome-reader-thread*)
  (setf *monome-reader-thread* nil))
