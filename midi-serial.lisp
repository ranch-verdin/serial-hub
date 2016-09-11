(in-package :midi-packetiser)

(defconstant +midi-baud-speed+ 31250)

(defmacro with-midi-uart-output ((fd dev-filename) &body body)
  (let ((tio (gensym)))
    `(let ((,fd (iolib.syscalls:open ,dev-filename o-wronly)))
       (unwind-protect (cffi:with-foreign-pointer (,tio (cffi:foreign-type-size '(:struct termios2)))
			 (assert (>= (iolib.syscalls:ioctl ,fd tcgets2 ,tio)
				     0))
			 (cffi:with-foreign-slots ((c-cflag c-ispeed c-ospeed) ,tio
						   termios2)
			   (setf c-cflag (logior bother
						 (logand (lognot cbaud)
							 c-cflag)))
			   (setf c-ispeed +midi-baud-speed+)
			   (progn ,@body)))
	 (iolib.syscalls:close ,fd)))))

(defun write-list-to-fd (list fd)
  (cffi:with-foreign-object (foo :uchar (length list))
      (dotimes (i (length list))
	(setf (cffi:mem-aref foo :uchar i)
	      (nth i list)))
      (iolib.syscalls:write fd foo 3)))

(defun tester()
  (with-midi-uart-output (midi-fd "/dev/ttyS2")
   (write-list-to-fd '(144 35 111) midi-fd)))
