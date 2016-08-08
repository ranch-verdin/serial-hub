;;;; serial-hub.asd

(asdf:defsystem #:serial-hub
  :description "Serial glue for boomerang pedal & monome aleph"
  :author "Rick Venn <sasquatch@rickvenn.com>"
  :license "GPL v2"
  :serial t
  :components ((:file "package")
	       (:file "serial-hub-utils")
	       (:file "aleph-serial")
	       (:file "boomerang-serial")
	       (:module "midi-glue"
			:components ((:file "reader")
				     (:file "writer")
				     (:file "clock")
				     (:file "midi-glue")))
	       (:file "monome-glue")
	       (:file "serial-hub"))
  :depends-on (:optima :cffi :iterate :external-program :calispel))

