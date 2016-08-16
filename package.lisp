;;;; package.lisp

(defpackage #:serial-hub-utils
  (:use #:cl #:calispel)
  (:export #:get-internal-utime #:make-nonblock-buf-channel
	   #:drain-channel #:*reader-ochan*))

(defpackage #:aleph-serial
  (:use #:cl #:optima #:cffi #:iterate)
  (:export #:serial-trigger-in #:with-aleph-output-stream))

(defpackage #:midi-packetiser
  (:use #:cl #:optima #:serial-hub-utils)
  (:export #:midi-message
	   ;;Below are symbols related to performance gestures
	   #:midi-performance-gesture #:note-on-midi-message
	   #:note-off-midi-message #:key-pressure-midi-message
	   #:control-change-midi-message #:channel-mode-midi-message
	   #:program-change-midi-message #:channel-pressure-midi-message
	   #:pitch-bend-midi-message

	   #:sysex-message
	   #:sysex-dump #:boomerang-sysex-message
	   ;; Below are symbols related to midi clock
	   #:midi-timing-message #:clock-tick-midi-message
	   #:start-midi-message #:continue-midi-message
	   #:stop-midi-message #:song-position-pointer-midi-message
	   
	   ;; #:hi-nibble
	   ;; #:lo-nibble #:hi-bit
	   ;; #:pack-nibbles #:parse-packet
	   #:write-midi-message #:with-midi-out #:*default-midi-out-stream*
	   #:read-midi-message #:with-midi-in #:*default-midi-in-stream*))

(defpackage #:boomerang
  (:use #:cl #:aleph-serial #:serial-hub-utils))

(defpackage #:midi-glue
  (:use #:cl #:cffi #:midi-packetiser #:optima #:optima.extra #:calispel
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
  (:use #:cl #:cl-monome #:cffi #:serial-hub-utils #:calispel)
  (:export #:start-monome-reader  #:stop-monome-reader))

(defpackage #:serial-hub
  (:use #:cl #:cffi #:monome-glue #:midi-glue #:calispel)
  (:export #:*reader-ochan*))

(defpackage #:sequencers
  (:use #:cl)
  (:export #:*master-beat-divisor* #:handle-gesture

	   #:grid-sequence #:make-grid-sequence
	   #:ticks-index #:swing-ratio
	   #:beat-divisor #:gesture-map
	   #:grid #:grid-length
	   #:sequence-tick-length #:handle-gestures
	   #:record-gesture #:do-tick
	   #:grid-set-element #:grid-set-column
	   
	   ;;FIXME - nothing done yet for free-sequence
	   #:free-sequence))


(defpackage #:sguenz
  (:use #:cl #:serial-hub #:calispel #:monome-glue #:midi-glue #:cl-monome #:midi-packetiser #:sequencers))
