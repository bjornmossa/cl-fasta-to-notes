;;;; fasta-notes.asd

(asdf:defsystem #:fasta-notes
  :description "Fasta-notes is a Common lisp tool for DNA data sonification"
  :author "bjornmossa"
  :license  "GNU GPLv3"
  :version "0.1.0"
  :serial t
  :components ((:file "model")
               (:file "fasta-notes")
               (:file "input")
               (:file "output")))
