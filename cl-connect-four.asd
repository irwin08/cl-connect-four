;;;; cl-connect-four.asd

(asdf:defsystem #:cl-connect-four
  :description "Describe cl-connect-four here"
  :author "Jesse Irwin"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "cl-connect-four")
	       (:file "ai")))
