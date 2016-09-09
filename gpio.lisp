(in-package :serial-hub)

(defun open-gpio-led-fd (gpio-num)
  (iolib/syscalls:open (format nil "/sys/class/gpio/gpio~a/value" gpio-num)
		       (logior iolib/syscalls:o-wronly
			       iolib/syscalls:o-creat
			       iolib/syscalls:o-trunc)
		       (logior iolib/syscalls:s-irusr
			       iolib/syscalls:s-iwusr
			       iolib/syscalls:s-irgrp
			       iolib/syscalls:s-iroth)))

(defparameter *gpio-buttons* '(66 69 45 23 47 27))

(defparameter *gpio-leds* '(67 68 44 26 46 65))

(defvar *gpio-led-fds*)
(defvar *gpio-led-on*)
(defvar *gpio-led-off*)

(defmacro with-gpio-leds (&body body)
  `(let ((*gpio-led-fds* (mapcar #'open-gpio-led-fd *gpio-leds*)))
     (with-foreign-string (*gpio-led-on* "1")
       (with-foreign-string (*gpio-led-off* "0")
	 (unwind-protect (progn ,@body)
	   (mapcar #'iolib/syscalls:close *gpio-led-fds*))))))

(defun set-led (idx state)
  (iolib/syscalls:write (nth idx *gpio-led-fds*)
			(if state
			    *gpio-led-on*
			    *gpio-led-off*)
			1))

#+nil
(with-gpio-leds
  (loop (set-led 0 (= (random 2) 0))
     (sleep 0.01)))
