(in-package :cl-user)

(defpackage #:fasta-notes-tests
  (:use #:cl
        #:fasta-notes
        #:prove))

(in-package #:fasta-notes-tests)

(subtest "Return list of associations"
  (ok (equal (get-nuc-assocs) '((#\A . 0) (#\G . 1) (#\T . 2) (#\C . 3)))))

(subtest "Return list of codons"
  (ok (equal (get-stop-codons) '("TGA" "TAG" "TAA"))))

(subtest "Return 0"
  (ok (= (get-nuc-assoc-val #\A (cons #\A 0)) 0)))

(subtest "Return nil"
  (ok (null (get-nuc-assoc-val #\F (cons #\A 0)))))

(subtest "Return list where car is 0"
  (ok (=
       (car (get-assoc-numlist #\A))
       0)))

(subtest "Return list of nils"
  (ok (every #'null (get-assoc-numlist #\F))))

(subtest "Return 0"
  (ok (=
       (find-nuc-assoc #\A))))

(subtest "Return nil"
  (ok (null (find-nuc-assoc #\R))))

(subtest "Return T"
  (ok (is-nuc? #\T)))

(subtest "Return nil"
  (ok (null (is-nuc? #\R))))

(subtest "Return list of chars"
  (ok (equal '(#\A #\B #\C) (str-to-list "ABC"))))

(subtest "Return T"
  (ok (nuc-string? "AATCCG")))

(subtest "Return nil"
  (ok (null (nuc-string? "AATCCGF"))))

(subtest "Return T"
  (ok (is-stop-codon? "TGA")))

(subtest "Return nil"
  (ok (null (is-stop-codon? "AAT"))))

(subtest "Return list of numbers"
  (ok (every #'numberp (codon-to-assocs "ATG"))))

(subtest "Return cons pair"
  (ok (consp (codon-to-notes "ATG"))))

(subtest "Return 2"
  (ok (= 2 (car (codon-to-notes "TAG")))))

(subtest "Return 1"
  (ok (= 1 (cdr (codon-to-notes "TAG")))))

(finalize)
