(in-package :monome-glue)

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *monome-dev* nil))

(defun monome-led (x y brightness &optional (monome-dev *monome-dev*))
  (assert monome-dev)
  (assert (< brightness 16))
  (assert (>= brightness 0))
  (monome-led-level-set monome-dev x y brightness))

(defmacro with-open-monome ((&optional
			     (monome-dev '*monome-dev*)
			     (monome-dev-file "/dev/ttyUSB0"))
			       &body body)
  `(let ((,monome-dev (monome-open ,monome-dev-file (cffi:null-pointer))))
     (unwind-protect (progn ,@body)
       (monome-close ,monome-dev))))

(defvar *button-down-fn* (lambda (x y)
			   (print (list 'down x y))))

(defcallback handle-button-down :void ((monome-event :pointer) (data :pointer))
  (declare (ignore data))
  (with-foreign-pointer (*x 4)
    (with-foreign-pointer (*y 4)
      (with-foreign-pointer (**monome 4)
	(monome-event-get-grid monome-event *x *y **monome)
	(let ((x (mem-ref *x :uint))
	      (y (mem-ref *y :uint)))
	  (funcall *button-down-fn* x y))))))

(defun monome-clear (&optional (monome-dev *monome-dev*))
  (monome-led-all monome-dev 0))

(defvar *button-up-fn* (lambda (x y)
			 (print (list 'up x y))))
(defcallback handle-button-up :void ((monome-event :pointer) (data :pointer))
  (declare (ignore data))
  (with-foreign-pointer (*x 4)
    (with-foreign-pointer (*y 4)
      (with-foreign-pointer (**monome 4)
	(monome-event-get-grid monome-event *x *y **monome)
	(let ((x (mem-ref *x :uint))
	      (y (mem-ref *y :uint)))
	  (funcall *button-up-fn* x y))))))

(defun run-monome-input-loop (down-fn up-fn)
  (assert (probe-file "/dev/ttyUSB0"))
  (assert (not *monome-dev*))
  (setf *monome-dev* (monome-open "/dev/ttyUSB0" (cffi:null-pointer)))
  (unwind-protect
       (progn (setf *button-up-fn* up-fn)
	      (setf *button-down-fn* down-fn)
	      (monome-register-handler *monome-dev*
				       (cffi:foreign-enum-value 'monome-event-type-t
								:monome-button-down)
				       (callback handle-button-down)
				       (cffi:null-pointer))
	      (monome-register-handler *monome-dev*
				       (cffi:foreign-enum-value 'monome-event-type-t
								:monome-button-up)
				       (callback handle-button-up)
				       (cffi:null-pointer))
	      (monome-event-loop *monome-dev*))
    (progn (monome-close *monome-dev*)
	   (setf *monome-dev* nil))))

(defun monome-stress-test ()
  (assert (null *monome-dev*))
  (let ((bg (bt:make-thread (lambda ()
			      (run-monome-input-loop (lambda (x y)
						       (print (list 'down x y)))
						     (lambda (x y)
						       (print (list 'up x y))))))))
    (unwind-protect (progn
		      (sleep 0.01)
		      (loop (monome-led-level-set *monome-dev* 
						  (random 16)
						  (random 8)
						  (random 16))))
      (monome-clear)
      (bt:destroy-thread bg))))

(defvar *monome-reader-thread* nil)

(defclass monome-button-event ()
  ((x :initarg :x) (y :initarg :y)))
(defclass monome-button-press (monome-button-event)
  ())
(defclass monome-button-release (monome-button-event)
  ())

(defun start-monome-reader ()
  (assert (null *monome-dev*))
  (assert (null *monome-reader-thread*))
  (setq *monome-reader-thread*
       	(bt:make-thread (lambda ()
			  (run-monome-input-loop
			   (lambda (x y)
			     (! *reader-ochan*
				(make-instance 'monome-button-press
					       :x x :y y)))
			   (lambda (x y)
			     (! *reader-ochan*
				(make-instance 'monome-button-release
					       :x x :y y)))))
			:name "monome reader thread")))
(defun stop-monome-reader ()
  (bt:destroy-thread *monome-reader-thread*)
  (setf *monome-reader-thread* nil))
