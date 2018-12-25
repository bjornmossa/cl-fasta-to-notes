(in-package #:fasta-notes)

(prove:subtest "Return list of associations"
  (prove:ok (listp (get-nuc-assocs))))

(prove:finalize)
