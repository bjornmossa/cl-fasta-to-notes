(in-package :cl-user)

(defpackage #:fasta-notes.input
  (:use #:cl
        #:fasta-notes))

(in-package #:fasta-notes.input)

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

(defun file-to-models (file &optional (lines 10))
  (let* ((fasta-string (read-file file lines))
         (codon-list (split-string-by fasta-string 3)))
    (map 'list #'create-model codon-list)))
