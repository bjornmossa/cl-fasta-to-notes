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
  "Return cons pair represent a note and octave"
  (let ((assocs (codon-to-assocs str)))
    (cons
     (car assocs)
     (+ (second assocs) (third assocs)))))

(defun format-codon (str stream)
  (if (is-stop-codon? str)
      (format stream "~A~%" 0)
      (format stream "~A ~A~%"
              (+ (car (codon-to-notes str)) 1)
              (cdr (codon-to-notes str)))))

(defun read-file (path lines)
  (let ((result ""))
    (progn
      (with-open-file (file path :direction :input)
        (do ((line (read-line file nil 'eof)
                   (read-line file nil 'eof))
             (i 0 (if (nuc-string? line) (+ i 1) i)))
            ((> i lines)
             (eql line 'eof))
          (if (nuc-string? line)
              (setf result (concatenate 'string result line)))))
      result)))

(defun write-to-file (string stream)
  (if (< (length string) 3)
      nil
      (progn
        (format-codon (subseq string 0 3) stream)
        (write-to-file (subseq string 3) stream))))

(defun fasta-to-notes (in-file out-file lines)
  (with-open-file (outf out-file
                        :direction :output
                        :if-exists :supersede)
    (write-to-file (read-file in-file lines) outf))
  'DONE)
