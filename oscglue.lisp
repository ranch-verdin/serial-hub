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

(defclass osc-noteon
    (osc-performance-gesture)
  ((gate-address :initarg :gate-address)
   (volume :initarg :volume)
   (freq-address :initarg :freq-address)
   (freq :initarg :freq)))

(defclass osc-noteoff
    (osc-performance-gesture)
  ((gate-address :initarg :gate-address)))

(defun osc-note= (x y)
  (string= (slot-value x 'gate-address)
	   (slot-value y 'gate-address)))

(defmethod record-gesture :after ((gesture osc-noteon) (seq free-sequence))
  (when (sequencers::armed-and-ready seq)
    (pushnew (note-off gesture)
	     (sequencers::hanging-rec-tones seq)
	     :test #'osc-note=)))

(defmethod record-gesture :after ((gesture osc-noteoff) (seq free-sequence))
  (when (sequencers::armed-and-ready seq)
    (setf (sequencers::hanging-rec-tones seq)
	  (remove gesture (sequencers::hanging-rec-tones seq)
		  :test #'osc-note=))))

(defmethod note-off ((note-on osc-noteon))
  (make-instance 'osc-noteoff
		 :gate-address (slot-value note-on 'gate-address)))

(defmethod sequencers::hang-play-tone ((seq free-sequence) (gesture osc-noteon))
  (push (note-off gesture)
	(sequencers::hanging-play-tones seq)))

(defmethod sequencers::hang-play-tone ((seq free-sequence) (gesture osc-noteoff))
  (setf (sequencers::hanging-play-tones seq)
	(remove gesture (sequencers::hanging-play-tones seq)
		:test #'osc-note=)))

