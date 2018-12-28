(in-package :cl-user)

(defpackage #:fasta-notes-tests
  (:use #:cl
        #:fasta-notes
        #:prove))

(in-package #:fasta-notes-tests)

(subtest "Return list of associations"
  (ok (listp (get-nuc-assocs))))

(finalize)
