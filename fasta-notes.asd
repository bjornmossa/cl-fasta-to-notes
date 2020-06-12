;;;; fasta-notes.asd

(asdf:defsystem #:fasta-notes
  :description "Fasta-notes is a Common lisp tool for DNA data sonification"
  :author "bjornmossa"
  :license  "GNU GPLv3"
  :version "0.2.1"
  :serial t
  :depends-on (#:cl-ppcre
               #:replic)
  :components ((:file "utils")
               (:file "model")
               (:file "fasta-notes")
               (:file "input")
               (:file "output")
               (:file "user")
               (:file "readline"))
  :build-operation "asdf:program-op"
  :build-pathname "fasta-notes"
  :entry-point "replic:repl")
