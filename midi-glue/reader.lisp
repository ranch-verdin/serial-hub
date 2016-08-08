(in-package :midi-glue)

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

(define-condition stop-thread (error)
  ())

(defvar *reader-ochan* (make-nonblock-buf-channel))

(defun midi-input (midi-stream clock-ichan reader-ochan
		   ;; reader-map used to have a reader map here - but
		   ;; I think better to burden application level with
		   ;; mappings for their device
		   )
  (let ((message (read-midi-message midi-stream)))
    (typecase message
      (midi-performance-gesture (! reader-ochan message))
      (midi-timing-message (! clock-ichan message)))))
      ;; (midi-clock-gesture 
      ;; (if-gesture
      ;;   (mapcar
      ;;    (lambda (mapped-mess)
      ;;      (! reader-ochan mapped-mess))
      ;;    (funcall reader-map
      ;;             (list mess))))
      ;; (if-clock
      ;;   (! clock-ichan mess)))))

(defvar *reader-thread* nil)

(defun start-reader (clock-ichan)
  (assert (null *reader-thread*))
  (setf *reader-thread*
        (bt:make-thread (lambda ()
                          (sleep 0.1)
                          (unwind-protect
                               (handler-case
                                   (with-open-file (midi-stream "/dev/snd/midiC3D0"
								:direction :io
								:if-exists :overwrite
								:element-type  '(unsigned-byte 8))
                                     (loop
                                        (restart-case
                                            (midi-input midi-stream
                                                        clock-ichan
                                                        *reader-ochan*)
                                          (carry-on-reading ()))))
                                 (stop-thread ()))
                            (setf *reader-thread* nil)))
                        :name "simple-midi-reader")))

(defun stop-reader ()
  (bt:interrupt-thread
   *reader-thread* (lambda ()
                      (error 'stop-thread))))
