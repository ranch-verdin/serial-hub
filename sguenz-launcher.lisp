(ql:quickload (list :serial-hub :swank))

(in-package :sguenz)
(swank:create-server :port 4006)

(ignore-errors (start-monome-reader))
(sleep 0.1)
(ignore-errors (start-midi-reader))
(sleep 0.1)

(sguenz-main)
