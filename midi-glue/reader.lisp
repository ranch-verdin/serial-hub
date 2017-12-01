(in-package :midi-glue)

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
(define-condition stop-thread (error)
  ())

(defun start-midi-reader (&optional (clock-ichan *reader-ochan*))
  (assert (null *reader-thread*))
  (setf *reader-thread*
        (bt:make-thread (lambda ()
                          (sleep 0.1)
                          (unwind-protect
                               (handler-case
                                   (with-midi-oss-in (midi-stream "/dev/snd/midiC1D0")
                                     (loop
                                        (restart-case
                                            (midi-input midi-stream
                                                        clock-ichan
                                                        *reader-ochan*)
                                          (carry-on-reading ()))))
                                 (stop-thread ()))
                            (setf *reader-thread* nil)))
                        :name "simple-midi-reader")))

(defun stop-midi-reader ()
  (bt:interrupt-thread
   *reader-thread* (lambda ()
                      (error 'stop-thread))))
