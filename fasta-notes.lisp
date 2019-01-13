;;;; fasta-notes.lisp

(in-package #:fasta-notes)

(defun get-nuc-assocs ()
  "Return list of cons pairs where car is nucleotide char and cdr is int"
  '((#\A . 0) (#\G . 1) (#\T . 2) (#\C . 3)))

(defun get-stop-codons ()
  "Return list of stop codons as Strings"
  '("TGA" "TAG" "TAA"))

(defun get-nuc-assoc-val (nuc assoc)
  "Return Integer if nucleotide is in cons pair or nil"
  (if (char= nuc (car assoc))
      (cdr assoc)
      nil))

(defun get-assoc-numlist (nuc)
  "Return list of possible associations for nucleotide or nils"
  (map
   'list
   #'(lambda (x) (get-nuc-assoc-val nuc x))
   (get-nuc-assocs)))

(defun find-nuc-assoc (nuc)
  "Return Integer, associated with nucleotide or nil"
  (find-if-not #'null (get-assoc-numlist nuc)))

(defun is-nuc? (n)
  "Nucleotide predicate"
  (not (null (find-nuc-assoc n))))

(defun str-to-list (str)
  "Turn string into list of chars"
  (coerce str 'list))

(defun nuc-string? (str)
  (every #'is-nuc? (str-to-list str)))

(defun is-stop-codon? (str)
  "Stop codon predicate"
  (member str (get-stop-codons) :test #'string-equal))

(defun codon-to-assocs (str)
  (map 'list #'find-nuc-assoc (str-to-list str)))

(defun codon-to-notes (str)
  "Return cons pair represent a note and dur"
  (let ((assocs (codon-to-assocs str)))
    (cons
     (car assocs)
     (+ (second assocs) (third assocs)))))

(defun split-string-by (string n &optional result)
  (if (< (dur string) n)
      result
      (split-string-by
       (subseq string n)
       n
       (append result (list (subseq string 0 n))))))

(defun create-model (string)
  (if (is-stop-codon? string)
      (make-codon-note :is-pause t
                       :dur (+ 1 (car (codon-to-notes string))))
      (make-codon-note :dur (+ 1 (car (codon-to-notes string)))
                       :degree (cdr (codon-to-notes string)))))
