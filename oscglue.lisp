(in-package :sguenz)

(defparameter *sgynth-port* 57120)

(defvar *sgynth-socket* (usocket:socket-connect #(127 0 0 1) *sgynth-port*
						:protocol :datagram
						:element-type '(unsigned-byte 8)))
(defun close-sgynth-socket ()
  (usocket:socket-close *sgynth-socket*))

(defun recrank-sgynth-socket ()
  (ignore-errors (close-sgynth-socket))
  (setf *sgynth-socket* (usocket:socket-connect #(127 0 0 1) 57120
						:protocol :datagram
						:element-type '(unsigned-byte 8))))

(defun osc-send (&rest args)
  (let ((mess (apply #'osc:encode-message args)))
    (usocket:socket-send *sgynth-socket* mess (length mess))))

(defun load-sgynth-engine()
  (osc-send "/engine/load/name" "./sgynth.so"))

(defun test-pling ()
  (osc-send "/command/sgynth_string_string3_gate" 1.0))

(defun test-bump ()
  (osc-send "/command/sgynth_bd_bd" 1.0))

(defclass osc-gesture
    ()
  ())

(defclass osc-performance-gesture
    ()
  ())

(defclass osc-trigger
    (osc-performance-gesture)
  ((gate-address :initarg :gate-address)
   (volume :initarg :volume)))

(defclass osc-tuned-trigger
    (osc-trigger)
  ((freq-address :initarg :freq-address)
   (freq :initarg :freq)))

(defmethod note-off ((gesture osc-trigger))
  nil);; there's no corresponding note-off for percussive triggers

(defmethod sequencers::hang-play-tone ((seq free-sequence) (gesture osc-trigger))
  );; nothing to do with percussive triggers

