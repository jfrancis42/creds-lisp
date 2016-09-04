;;;; creds.asd

(asdf:defsystem #:creds
  :description "A portable method for securely storing credentials."
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :depends-on (#:cl-yaclyaml
	       #:ironclad
	       #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "creds")))
