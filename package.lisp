;;;; package.lisp

(defpackage #:serial-hub-utils
  (:use #:cl #:calispel)
  (:export #:get-internal-utime #:make-nonblock-buf-channel
	   #:drain-channel #:*reader-ochan*))

(defpackage #:aleph-serial
  (:use #:cl #:optima #:cffi #:iterate)
  (:export #:serial-trigger-in #:with-aleph-output-stream))

(defpackage #:boomerang
  (:use #:cl #:aleph-serial #:serial-hub-utils #:cl-rtmidi)
  (:export #:start-brosync-sync #:*boomerang-taptempo-chan*
	   #:*rang-input-stream* #:*rang-output-stream*))

(defpackage #:midi-glue
  (:use #:cl #:cffi #:cl-rtmidi #:optima #:optima.extra #:calispel
	#:serial-hub-utils)
  (:export #:*clock-ochan* #:*clock-ctrl-chan*
           #:*reader-ichan* #:*reader-ochan*
           #:set-master-bpm #:inspect-midihelper
           ;; #:start-midi-glue #:stop-midi-glue
           ;; #:check-midi-glue
	   #:start-midi-reader #:stop-midi-reader
	   #:send-event
           #:ev-tick #:ev-microtick))

(defpackage #:monome-glue
  (:use #:cl #:monome-serialosc #:cffi #:serial-hub-utils #:calispel)
  (:export #:start-monome-reader  #:stop-monome-reader))

(defpackage #:serial-hub
  (:use #:cl #:cffi #:monome-glue #:midi-glue #:calispel)
  (:export #:*reader-ochan*))

(defpackage #:sequencers
  (:use #:cl #:cl-rtmidi)
  (:export #:*master-beat-divisor*
	   #:grid-sequence #:make-grid-sequence
	   #:ticks-index #:swing-ratio
	   #:beat-divisor #:gesture-map
	   #:grid #:grid-length
	   #:grid-crossing-point
	   #:sequence-tick-length #:read-gestures
	   #:record-gesture #:do-tick
	   #:grid-set-element #:grid-set-column
	   
	   #:free-sequence #:note-on #:note-off
	   #:do-tick #:erase-sequence #:copy-sequence
	   #:play-push-extend #:play-repeat #:play-stop
	   #:play-state #:rec-state #:empty-p
	   #:rec-arm #:rec-unarm #:rec-toggle
	   #:loop-cycle #:grid-position))


(defpackage #:sguenz
  (:use #:cl #:serial-hub #:calispel #:monome-glue #:midi-glue #:monome-serialosc #:cl-rtmidi #:sequencers #:serial-hub-utils #:boomerang)
  (:export #:start-sguenz-app #:sguenz-grab-focus))
