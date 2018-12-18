;;;; fasta-notes.lisp

(in-package #:fasta-notes)

(defun get-nuc-assocs ()
  '((#\A . 0) (#\G . 1) (#\T . 2) (#\C . 3)))

(defun get-stop-codons ()
  '("TGA" "TAG" "TAA"))

(defun get-nuc-assoc (nuc assoc)
  (if (char= nuc (car assoc))
      (cdr assoc)
      nil))

(defun get-assoc-numlist (nuc)
  (map
   'list
   #'(lambda (x) (get-nuc-assoc nuc x))
   (get-nuc-assocs)))

(defun find-nuc-assoc (nuc)
  (find-if-not #'null (get-assoc-numlist nuc)))

(defun is-nuc? (n)
  (not (null (find-nuc-assoc n (get-nuc-assocs)))))

(defun str-to-list (str)
  (loop for char across str
       collect char))

(defun nuc-string? (str)
  (every #'is-nuc? (str-to-list str)))

(defun is-stop-codon? (str)
  (member str (get-stop-codons) :test #'string-equal))

(defun codon-to-notes (str)
  (cons (find-nuc-assoc (car (str-to-list str)) (get-nuc-assocs))
        (+
         (find-nuc-assoc (second (str-to-list str)) (get-nuc-assocs))
         (find-nuc-assoc (third (str-to-list str)) (get-nuc-assocs)))))

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
