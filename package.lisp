;;;; package.lisp

(defpackage #:serial-hub-utils
  (:use #:cl)
  (:export #:get-internal-utime))

(defpackage #:midi-packetiser
  (:use #:cl #:optima #:serial-hub-utils)
  (:export #:midi-message #:note-on-midi-message
	   #:note-off-midi-message #:key-pressure-midi-message
	   #:control-change-midi-message #:channel-mode-midi-message
	   #:program-change-midi-message #:channel-pressure-midi-message
	   #:pitch-bend-midi-message #:sysex-message
	   #:sysex-dump #:boomerang-sysex-message
	   #:clock-message #:start-message
	   #:continue-message #:stop-message
	   #:song-position-pointer #:hi-nibble
	   #:lo-nibble #:hi-bit
	   #:pack-nibbles #:parse-packet
	   #:read-midi-message))
(defpackage #:serial-hub
  (:use #:cl #:cl-monome #:cffi))

