;;;; serial-hub.asd

(asdf:defsystem #:serial-hub
  :description "Serial glue for boomerang pedal & monome aleph"
  :author "Rick Venn <sasquatch@rickvenn.com>"
  :license "GPL v2"
  :serial t
  :defsystem-depends-on ("cffi-grovel")
  :components ((:file "package")
	       (:file "serial-hub-utils")
	       (:file "midi-packetiser")
	       (:cffi-grovel-file "midi-grovel")
	       (:file "midi-serial")
	       (:file "aleph-serial")
	       (:file "gpio")
	       (:file "boomerang-serial")
	       (:module "midi-glue"
			:components ((:file "reader")
				     (:file "writer")
				     (:file "clock")
				     (:file "midi-glue")))
	       (:file "monome-glue")
	       (:file "serial-hub")
	       (:file "sequencers")
	       (:file "sguenz"))
  :depends-on (:optima :cffi
		       :iterate :external-program :calispel
		       :cl-monome :cl-ppcre :iolib))

