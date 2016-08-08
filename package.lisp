;;;; package.lisp

(defpackage #:serial-hub-utils
  (:use #:cl #:calispel)
  (:export #:get-internal-utime #:make-nonblock-buf-channel
	   #:drain-channel #:*reader-ochan*))

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
	   #:midi-timing-message #:clock-message
	   #:start-message #:continue-message
	   #:stop-message #:song-position-pointer
	   
	   ;; #:hi-nibble
	   ;; #:lo-nibble #:hi-bit
	   ;; #:pack-nibbles #:parse-packet
	   #:read-midi-message))

(defpackage :midi-glue
  (:use #:cl #:cffi #:midi-packetiser #:optima #:optima.extra #:calispel
	#:serial-hub-utils)
  (:export #:*clock-ochan* #:*clock-ctrl-chan*
           #:*reader-ichan* #:*reader-ochan*
           #:set-master-bpm #:inspect-midihelper
           #:start-midi-glue #:stop-midi-glue
           #:check-midi-glue #:send-event
           #:ev-tick #:ev-microtick))

(defpackage #:monome-glue
  (:use #:cl #:cl-monome #:cffi #:serial-hub-utils #:calispel)
  (:export #:start-monome-reader
	   #:monome-button-press #:monome-button-release
	   #:*monome-reader-thread* #:monome-led #:with-open-monome
	   #:*button-down-fn* #:*button-up-fn* #:monome-clear
	   #:run-monome-input-loop))

(defpackage #:serial-hub
  (:use #:cl #:cl-monome #:cffi))

