(in-package :monome-glue)

(defvar *monome-reader-thread* nil)

(defun start-monome-reader ()
  ;; (assert (null *monome-dev*))
  (assert (or (null *monome-reader-thread*)
	      (not (bt:thread-alive-p *monome-reader-thread*))))
  ;; (loop while (not monome-serialosc::*monome-devices*)
  ;;    do (setup-monome-dev)
  ;;      (sleep 0.2))
  (setq *monome-reader-thread*
       	(bt:make-thread (lambda ()
			  ;; (with-monome-output ()
			  ;;   (grab-focus))
			  (with-monome-input ()
			    (loop (! *reader-ochan*
				     (monome-receive-message)))))
			:name "monome reader thread")))

(defun stop-monome-reader ()
  (bt:destroy-thread *monome-reader-thread*)
  (setf *monome-reader-thread* nil))
