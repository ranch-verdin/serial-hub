;;;; serial-hub.asd

(asdf:defsystem #:serial-hub
  :description "Serial glue for boomerang pedal & monome aleph"
  :author "Rick Venn <sasquatch@rickvenn.com>"
  :license "GPL v2"
  :serial t
  :components ((:file "package")
	       (:file "aleph-serial")
	       (:file "boomerang-serial")
	       (:file "serial-hub")))

