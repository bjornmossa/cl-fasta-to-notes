(in-package :cl-user)

(defpackage #:fasta-notes.user
  (:use #:cl
        #:fasta-notes
        #:fasta-notes.input
        #:fasta-notes.model))

(in-package :fasta-notes.user)

;; load file
;; show file info
(defparameter *file* '(ERROR . "No file selected"))

(defmacro with-file-env (file-env &rest body)
  (let ((file (gensym)))
    `(let ((,file ,file-env))
      (if (eq (car ,file) 'ERROR)
          (cdr ,file)
          ,@body))))

(defun load-file ()
  (print "Enter path to .fasta file:")
  (let ((path (read-line)))
    (progn
      (setf *file* (read-fasta-file path))
      (with-file-env *file*
        (cdr *file*)))))

(defun file-info ()
  (with-file-env *file*
    (progn
      (print (fasta-file-header (cdr *file*)))
      (format t "~%File contains ~d nucleotides" (length (fasta-file-content (cdr *file*))))
      (format t "~%and ~d codons" (/ (length (fasta-file-content (cdr *file*))) 3)))))

(defun show-sequence (start end)
  (with-file-env *file*
    (let* ((codons (fasta-file-content (cdr *file*)))
           (sequence (create-codon-models codons start end))
           (durs (map 'list #'codon-note-dur sequence))
           (degrees (map 'list #'codon-note-degree sequence)))      
      (format t "Degrees: ~{~a~^, ~}~%Durations: ~{~a~^, ~}" degrees durs))))
