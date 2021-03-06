(in-package :midi-glue)
(defun inspect-midi-glue ()
  (list '*reader-thread* *reader-thread*
        '*clock-thread* *clock-thread*
        '*writer-thread* *writer-thread*))

(defun start-midi-glue (&optional
                           (master-slave :master)
                           (ppqn 96) (reader-map #'identity))
  (assert (or (= ppqn 96)
              (= ppqn 24)))
  (assert (or (eq master-slave :master)
              (eq master-slave :slave)))
  (alexandria:doplist (key val (inspect-midi-glue))
    (assert (null val)))
  (drain-channel *clock-ochan*)
  (drain-channel *clock-ctrl-chan*)
  (start-midi-reader *clock-ctrl-chan*)
  (start-clock *clock-ctrl-chan* master-slave ppqn)
  (start-writer-thread))

(defun check-midi-glue ()
  (alexandria:doplist
      (key val (inspect-midi-glue))
    (if (null val)
        (warn "Helper ~A not running" key))))

(defun stop-midi-glue ()
  (check-midi-glue)
  (ignore-errors (stop-midi-reader))
  (setf *reader-thread* nil)
  (ignore-errors (stop-writer-thread))
  (setf *writer-thread* nil)
  (ignore-errors (stop-clock))
  (setf *clock-thread* nil)
  (inspect-midi-glue))
