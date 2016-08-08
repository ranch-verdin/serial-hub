;;;; package.lisp

(defpackage #:serial-hub-utils
  (:use #:cl)
  (:export #:get-internal-utime))

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
	   
	   #:hi-nibble
	   #:lo-nibble #:hi-bit
	   #:pack-nibbles #:parse-packet
	   #:read-midi-message))

(defpackage :midi-glue
  (:use #:cl #:cffi #:midi-packetiser #:optima #:optima.extra #:calispel)
  (:export #:*clock-ochan* #:*clock-ctrl-chan*
           #:*reader-ichan* #:*reader-ochan*
           #:set-master-bpm #:inspect-midihelper
           #:start-midihelper #:stop-midihelper
           #:check-midihelper #:if-gesture
           #:if-clock #:macromatch
           #:drain-channel #:send-event
           #:ev-noteon #:ev-noteoff
           #:ev-tick #:ev-microtick
           #:ev-start #:ev-stop
           #:ev-continue #:ev-songpos))

(defpackage #:serial-hub
  (:use #:cl #:cl-monome #:cffi))

