;;;; fasta-notes.asd

(asdf:defsystem #:fasta-notes
  :description "Fasta-notes is a Common lisp tool for DNA data sonification"
  :author "bjornmossa"
  :license  "GNU GPLv3"
  :version "0.1.0"
  :serial t
  :components ((:file "model")
               (:file "package")
               (:file "fasta-notes")
               (:file "input")
               (:file "output"))
  :in-order-to ((test-op (test-op fasta-notes-test))))

(asdf:defsystem #:fasta-notes-test
  :description "Unit tests for fasta-notes package"
  :author "Bjornmossa"
  :depends-on (#:fasta-notes
               #:prove
               #:prove-asdf)
  :components ((:test-file "fasta-notes-spec"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
