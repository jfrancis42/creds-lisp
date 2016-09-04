;;;; package.lisp

(defpackage #:creds
  (:use #:cl)
  (:export :load-creds
	   :get-cred
	   :set-cred
	   :delete-cred))
