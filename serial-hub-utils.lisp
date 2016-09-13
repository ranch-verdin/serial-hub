(in-package :serial-hub-utils)

(defun make-nonblock-buf-channel (&optional (queue 100))
  (make-instance 'calispel:channel
                 :buffer (MAKE-INSTANCE
                          'jpl-queues:LOSSY-BOUNDED-FIFO-QUEUE
                          :CAPACITY queue)
                 ))

(defun drain-channel (chan)
  (loop (pri-alt ((? chan res)
                  (print res))
                 (otherwise
                  (return-from drain-channel)))))



(defvar *reader-ochan* (make-nonblock-buf-channel))


#+sbcl
(defun get-internal-utime ()
  (multiple-value-bind (s us) (sb-ext:get-time-of-day)
    (+ (* 1000000 s)
       us)))
#+ccl
(defun get-internal-utime ()
  (round (/ (ccl:current-time-in-nanoseconds)
	    1000)))
